"""The table of conditions, stored without its zeros.

The reweighting writes each target as a condition on the weights. The conditions form a table with one row per record
and one column per target; an entry is the record's contribution to the target. Most records contribute nothing to
most targets: a target for one AGI group has zeros on every record outside the group. In a past year more than nine
entries in ten are zero.

The table is therefore kept as a sparse matrix (scipy.sparse), which stores only the entries that are not zero, with
their positions. Two layouts are used. The layout by column (CSC) keeps each target's entries together and is used to
build the table and to read one target's column. The layout by row (CSR) keeps each record's entries together and is
used to pick records and to work through the table a block of records at a time.

The solver needs products of the whole table with itself (solver.py, the Hessian). Those are computed on blocks of
records: each block is written out in full, with its zeros, multiplied, and added to the result. Only one block is held
in full at a time.

The functions that read the table (column, block, reached) also accept an ordinary numpy array, for the small tables
that are built in full.
"""
import numpy as np
import scipy.sparse as sp

# The number of entries in a block of records that is written out in full (2 ** 25 entries take 256 MB).
BLOCK_ENTRIES = 2 ** 25


def is_sparse(F):
    return sp.issparse(F)


# ---- Building the table ----

class Columns:
    """Collects a table's columns one at a time and keeps only the entries that are not zero. put(j, values) sets
    column j from one value per record; add(j, values) adds to a column already set; table() returns the table in the
    layout by column."""

    def __init__(self, n_records, n_columns):
        self.n, self.m = n_records, n_columns
        self.rows = [np.zeros(0, np.int32)] * n_columns
        self.values = [np.zeros(0)] * n_columns

    def put(self, j, values):
        values = np.asarray(values, float)
        assert values.shape == (self.n,)
        self.rows[j] = np.flatnonzero(values).astype(np.int32)
        self.values[j] = values[self.rows[j]]

    def add(self, j, values):
        full = np.zeros(self.n)
        full[self.rows[j]] = self.values[j]
        self.put(j, full + values)

    def table(self):
        """The table in the layout by column. The columns collected are let go one by one as they are copied into the
        table, and the collection is empty afterward."""
        counts = np.array([len(r) for r in self.rows], dtype=np.int64)
        starts = np.concatenate([[0], np.cumsum(counts)])
        rows, values = np.empty(starts[-1], np.int32), np.empty(starts[-1])
        for j in range(self.m):
            rows[starts[j]:starts[j + 1]], values[starts[j]:starts[j + 1]] = self.rows[j], self.values[j]
            self.rows[j] = self.values[j] = None
        return sp.csc_matrix((values, rows, starts), shape=(self.n, self.m))


def with_columns(F, new):
    """A copy of the sparse table F in which each column j in the dict new is replaced by new[j] (one value per
    record). Returned in the layout by column."""
    F = sp.csc_matrix(F)
    cols = Columns(*F.shape)
    for j in range(F.shape[1]):
        if j in new:
            cols.put(j, new[j])
        else:
            a, b = F.indptr[j], F.indptr[j + 1]
            keep = F.data[a:b] != 0
            cols.rows[j], cols.values[j] = F.indices[a:b][keep], F.data[a:b][keep]
    return cols.table()


def scale_columns(F, factors):
    """Multiplies each column of the sparse table F (in the layout by column) by its factor, in place."""
    assert sp.isspmatrix_csc(F)
    F.data *= np.repeat(np.asarray(factors, float), np.diff(F.indptr))
    return F


# ---- Reading the table ----

def column(F, j):
    """Column j of the table, one value per record."""
    if sp.issparse(F):
        return np.asarray(F[:, [j]].toarray(), float).ravel()
    return F[:, j]


def block(F, j0, j1):
    """Columns j0 to j1 - 1 of the table, written out in full as a new array."""
    if sp.issparse(F):
        return np.asarray(F[:, j0:j1].toarray(order='C'), float)
    return np.array(F[:, j0:j1], float)


def records(F, keep):
    """The rows of the sparse table F for the records in the mask keep, as a new table in the layout by column."""
    return sp.csc_matrix(F)[np.flatnonzero(keep)]


def reached(F, records=None):
    """For each column, whether any record has an entry that is not zero. With records (a mask), only those records
    are looked at."""
    if not sp.issparse(F):
        return ((F if records is None else F[records]) != 0).any(0)
    F = sp.csc_matrix(F)
    on = F.data != 0
    if records is not None:
        on &= np.asarray(records, bool)[F.indices]
    # The number of such entries in each column: the running count at the column's end less the count at its start.
    count = np.concatenate([[0], np.cumsum(on, dtype=np.int64)])
    return count[F.indptr[1:]] > count[F.indptr[:-1]]


def all_finite(F):
    return bool(np.isfinite(F.data if sp.issparse(F) else F).all())


# ---- Products on blocks of records ----

def record_blocks(X):
    """Works through the sparse table X (in the layout by row) a block of records at a time. Yields the first record
    of the block, the record after its last, and the block written out in full. A block has at most BLOCK_ENTRIES
    entries."""
    assert sp.isspmatrix_csr(X)
    n, m = X.shape
    step = max(1, BLOCK_ENTRIES // max(m, 1))
    for r0 in range(0, n, step):
        r1 = min(r0 + step, n)
        yield r0, r1, X[r0:r1].toarray(order='C')


def weighted_square(X, e):
    """X' diag(e) X for the sparse table X (in the layout by row) and one weight e per record, as a full matrix of
    targets by targets. Each block of records is multiplied by the square roots of its weights, and its product with
    itself is added to the result."""
    m = X.shape[1]
    H = np.zeros((m, m))
    root = np.sqrt(e)
    for r0, r1, B in record_blocks(X):
        B *= root[r0:r1, None]
        H += B.T @ B
    return H

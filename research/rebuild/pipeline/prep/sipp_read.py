"""The SIPP public-use person-month files, only the columns roster_sipp.py uses.

Each release (settings sipp.releases) is a pipe-delimited file of several GB, one row per person and month. Only the
columns below are read, and the result is cached in the data folder as sipp_raw_<year>.parquet; a cache that lacks a
column the release has is read again. Column names are matched without regard to case and stored in upper case.
"""
import os
import zipfile
import gzip

import pandas as pd
import pyarrow as pa
import pyarrow.csv as pcsv

from shared import common

PERSON = ['SSUID', 'PNUM', 'MONTHCODE', 'ERESIDENCEID', 'THHLDSTATUS', 'SPANEL', 'SWAVE', 'ERELRPE', 'ESEX', 'TAGE',
          'TAGE_EHC', 'EMS', 'EMS_EHC', 'EPNSPOUSE', 'EPNSPOUS_EHC', 'EPNCOHAB', 'EPNCOHAB_EHC', 'EPNPAR1', 'EPNPAR2',
          'EPAR1TYP', 'EPAR2TYP', 'EDEPCLM', 'EFSTATUS', 'WPFINWGT', 'RENROLL', 'EEDGRADE', 'RDIS', 'RFAMNUM',
          'RFAMREF']
INCOME = ['TPEARN', 'TSSSAMT', 'TSSI_AMT', 'TUC1AMT', 'TUC2AMT', 'TPPRPINC', 'TPOTHINC', 'TPTOTINC']
JOBS = 7            # job lines 1-7: earnings (MSUM), industry (IND) and class of worker (CLWRK)
TIP_JOBS = 4        # tips (TXAMT) are published for job lines 1-4 only
JOB_COLS = [f'TJB{j}_MSUM' for j in range(1, JOBS + 1)] + [f'TJB{j}_IND' for j in range(1, JOBS + 1)] + \
    [f'EJB{j}_CLWRK' for j in range(1, JOBS + 1)] + [f'TJB{j}_TXAMT' for j in range(1, TIP_JOBS + 1)]
RET_ITEMS = 8       # retirement income items 1-8 (company or union, federal, state, local, military, railroad, reserve, other)
OWNERSHIP = ['EOWN_ST', 'EOWN_MF', 'EJSOWNST', 'EJSOWNMF', 'EJOOWNST', 'EJOOWNMF']
# income from stocks and mutual funds (yearly), retirement income items (monthly), and the questions that decide who
# is asked each amount: received Social Security, received retirement item k, owned stocks or mutual funds
SHARED = ['TINC_STMF'] + [f'TRET{k}AMT' for k in range(1, RET_ITEMS + 1)] + \
    ['ESSSMNYN'] + [f'ERET{k}MNYN' for k in range(1, RET_ITEMS + 1)] + OWNERSHIP
COLUMNS = PERSON + INCOME + JOB_COLS + SHARED
OPTIONAL = [common.SETTINGS['deferrals']['sipp_item']]   # elective deferrals, read where the release has them
STRINGS = ['SSUID', 'ERESIDENCEID'] + [f'TJB{j}_IND' for j in range(1, JOBS + 1)]


def release_path(year):
    S = common.SETTINGS['sipp']
    return os.path.join(common.RAW['sipp'], S['releases'][str(year)])


def _open(path):
    """A binary stream of the release's CSV (plain, gzipped or the one CSV in a zip)."""
    if path.endswith('.zip'):
        z = zipfile.ZipFile(path)
        names = [n for n in z.namelist() if n.lower().endswith('.csv')]
        assert len(names) == 1, (path, names)
        return z.open(names[0])
    if path.endswith('.gz'):
        return gzip.open(path, 'rb')
    return open(path, 'rb')


def read_release(year, cache=True):
    """The person-month rows of the release for reference year `year`, the columns COLUMNS (upper case), numbers as
    floats (missing = NaN), SSUID, the residence id (letters in the 2014 panel) and industry codes as strings."""
    out = f'{common.DATA}/sipp_raw_{year}.parquet'
    path = release_path(year)
    with _open(path) as f:
        header = f.readline().decode().rstrip('\r\n').split('|')
    by_upper = {h.upper(): h for h in header}
    want = COLUMNS + [c for c in OPTIONAL if c in by_upper]
    if cache and os.path.exists(out):
        d = pd.read_parquet(out)
        if all(c in d.columns for c in want):
            return d
        common.say(f'SIPP {year}: the cached {os.path.basename(out)} lacks {[c for c in want if c not in d.columns]}; '
                   'reading the release again')
    missing = [c for c in COLUMNS if c not in by_upper]
    assert not missing, f'SIPP {year} ({path}): columns the pipeline needs are not in this release: {missing}'
    names = [by_upper[c] for c in want]
    types = {by_upper[c]: (pa.string() if c in STRINGS else pa.float64()) for c in want}
    with _open(path) as f:
        tab = pcsv.read_csv(f, parse_options=pcsv.ParseOptions(delimiter='|'),
                            read_options=pcsv.ReadOptions(block_size=1 << 26),
                            convert_options=pcsv.ConvertOptions(include_columns=names, column_types=types,
                                                                strings_can_be_null=True))
    d = tab.to_pandas()
    d.columns = [c.upper() for c in d.columns]
    common.say(f'SIPP {year} ({os.path.basename(path)}): {len(d):,} person-month rows, '
               f'{d.groupby(["SSUID", "PNUM"]).ngroups:,} people')
    if cache:
        os.makedirs(common.DATA, exist_ok=True)
        d.to_parquet(out, index=False)
    return d

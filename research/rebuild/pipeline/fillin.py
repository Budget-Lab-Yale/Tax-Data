"""Function 2, holding: the two-slot fill-in, run during training (train.py).

Some SCF households that report zero interest, dividends or capital gains hold
some. Each item with fill_in in the translation table (translate.py) has two slots on
SCF rows: the reported amount (model.REPORTED, never changed) and the
PUF-concept amount (the shared slot), which reporters copy and zero-reporters
get filled in. From the pass fill_from_pass on, at the start of each pass,
refit() gives every SCF zero-reporter its chance of a hidden holding in each
size range, from the reporting-rate formula:
  report_probs (C_k)       its chance of reporting range k: SCF label, the
                           reported slot, everything else shown except every
                           PUF-concept slot (so no fill-in ever feeds the formula);
  report_probs_by_type (B_k)  C_k averaged over SCF units of the same type: the
                           survey year and the table's type_for_fill_in
                           variables (married, dependents and the wage band);
  puf_probs_by_range (A_k) the tax data's share given the type only: the
                           count-weighted average of the filers' share and the
                           non-filers' share (step 2, 23 October; the law of
                           total probability),
                             A_k = (N_f A_k(filers) + N_n A_k(non-filers)) / (N_f + N_n),
                           N_f and N_n the filers and non-filers of the type in
                           the tax data for the survey year (build_table.py,
                           tax_data; data/tax_data_<year>.parquet). Filers: PUF
                           label, the PUF year, over the PUF's units other than
                           dependent filers: the item given the type and each
                           adult age range, mixed by the model's chance of that
                           range. With settings training.fillin_puf_side
                           "data" (needed with grouped weighting, which
                           tilts the network's answer toward the top AGI
                           groups) the filers' share is the PUF training
                           rows' own, survey-weighted, by type; the network's
                           answer is kept only for types with fewer than 30
                           PUF records. Non-filers: CPS label, the CPS year nearest the
                           survey year, the filing flag 0, everything else but
                           the type hidden; an item the CPS does not measure
                           (interest, from 23 October) has no non-filer holder,
                           as generation gives non-filers none. A type with no unit in the tax data
                           takes the filers' share alone (counted in the log);
  excess (e_k)             max(A_k / B_k - 1, 0): how many more holders the PUF
                           has than the survey reports, per reporter;
  hidden (h_k)             C_k * e_k / C_0, capped at 100% in total.
Within a range, bands follow A. Then draw() redraws, in each batch, the
PUF-concept slot of its zero-reporters from those chances. The formula assumes
a holder's chance of reporting depends only on its type and true amount, not
on anything else on the record.

Writes common.MODEL/fill_log.csv: per pass and item, the share with none as
reported, after the fill-in, in the tax data at the SCF's types (the mixed
share) and among filers only (the PUF label alone, the share before step 2),
all on the PUF-year wave; the share of zero-reporters capped; the largest gap
from 1 of the mixed shares' sum over size ranges in any type (the law of total
probability check); and fill_types.csv, the last refit's shares of none by
type (filers, non-filers, mixed) with the type's two counts. report_translation.py calls
refit() on a trained model for the report's stage after function 2.
"""
import math
import os

import numpy as np
import pandas as pd
import torch

import common
import model as M
import translate as TR

ROWS = {r['variable']: r for r in TR.filled()}
assert all(r['survey'] == 'scf' for r in ROWS.values()), 'function 2 runs on the SCF only so far (this file reads SCF rows)'
MAPPED = list(ROWS)
TYPE = TR.fill_type()                             # the type the formula conditions on, with the survey year
C = len(M.CONTEXT)
P = M.PREDICTED
CPS_YEARS = [y for y in common.SETTINGS['cps']['years'] if y in M.YEARS]
RANGES = {v: [(-math.inf if lo is None else lo, math.inf if hi is None else hi) for lo, hi in r['fill_in']['size_ranges']]
          for v, r in ROWS.items()}


def in_range(x, lo, hi):
    """Small positive ranges are (0, hi); every other range is [lo, hi)."""
    return 0 < x < hi if lo == 0 else lo <= x < hi


def nearest_cps_year(y):
    """The CPS year nearest survey year y (the earlier of two equally near), as build_table.translate_surveys reads it."""
    return min(CPS_YEARS, key=lambda c: (abs(c - y), c))


def band_on_edge(x, edge, first_positive):
    """Bands of dollars x on an edge record's cut points (build_table.band_variable with fixed cut points)."""
    b = np.zeros(len(x), dtype=np.int64)
    neg, pos = x < 0, x > 0
    if neg.any():
        b[neg] = 1 + np.searchsorted(np.array(edge['negative']['cuts']), x[neg], side='left')
    if pos.any():
        b[pos] = first_positive + np.searchsorted(np.array(edge['positive']['cuts']), x[pos], side='left')
    return b


def type_keys(df, survey, y, banded):
    """The fill-in's type columns of df (one survey year): categories as they are; an amount as its band, read from
    the band column if banded, else put on the survey year's cut points from its dollars."""
    out = {}
    for v in TYPE:
        if v in M.CATEGORICAL:
            out[v] = df[v].to_numpy(np.int64)
        elif banded:
            out[v] = df[v + '_band'].to_numpy(np.int64)
        else:
            out[v] = band_on_edge(df[v].to_numpy(float), common.edges()[f'{survey}/{y}/{v}'],
                                  M.BAND_LAYOUT[v]['first_positive'])
    return pd.DataFrame(out)


def tax_data_counts(scf):
    """Per SCF row (scf: the SCF training rows): the filers and the non-filers of its type in the tax data for its
    survey year."""
    n_f, n_n = np.zeros(len(scf)), np.zeros(len(scf))
    for y in sorted(scf['year'].unique()):
        sel = (scf['year'] == y).to_numpy()
        d = scf[sel]
        own = type_keys(d, 'scf', y, banded=True)
        for v in TYPE:   # the bands put on the cut points from the dollars must be the table's own
            if v not in M.CATEGORICAL:
                again = type_keys(d, 'scf', y, banded=False)[v].to_numpy()
                assert (again == own[v].to_numpy()).all(), f'{v}: bands from the cut points differ from the table\'s'
        T = pd.read_parquet(f'{common.DATA}/tax_data_{y}.parquet')
        assert T['filer'].isin([0, 1]).all() and (T['weight'] >= 0).all()
        tk = type_keys(T, 'scf', y, banded=False)
        tk['filer'], tk['weight'] = T['filer'].to_numpy(), T['weight'].to_numpy(float)
        counts = tk.groupby(TYPE + ['filer'])['weight'].sum().unstack('filer', fill_value=0.0)
        m = own.merge(counts, left_on=TYPE, right_index=True, how='left').fillna(0.0)
        n_f[sel] = m[1].to_numpy() if 1 in m else 0.0
        n_n[sel] = m[0].to_numpy() if 0 in m else 0.0
        common.say(f'  fill-in tax data {y}: {T.loc[T.filer == 1, "weight"].sum() / 1e6:.2f}M filers, '
                   f'{T.loc[T.filer == 0, "weight"].sum() / 1e6:.2f}M non-filers; SCF units in a type with no tax-data '
                   f'unit {100 * d.weight[(n_f[sel] + n_n[sel]) == 0].sum() / d.weight.sum():.2f}%')
    return n_f, n_n


def range_of_band(v):
    """Size range of every band of v by its PUF-year median: 0 = zero, 1..K = the ranges, K + 1 = a band with no
    PUF records."""
    out = []
    for bd in common.edges()[f'puf/{common.PUF_YEAR}/{v}']['bands']:
        if bd['band'] == 0:
            out.append(0)
        elif bd['median'] is None:
            out.append(len(RANGES[v]) + 1)
        else:
            hit = [i for i, (lo, hi) in enumerate(RANGES[v]) if in_range(bd['median'], lo, hi)]
            assert len(hit) == 1, (v, bd)
            out.append(hit[0] + 1)
    return np.array(out, dtype=np.int64)


class FillIn:
    """The fill-in for the training records tr (train.py's encoded training split; train_rows its table rows)."""

    def __init__(self, model, tr, train_rows, gen):
        self.model, self.tr, self.gen = model, tr, gen
        device = tr['values'].device
        self.device = device
        self.n_bands = {v: M.N_CHOICES[v] for v in MAPPED}
        self.slot = {v: P.index(v) for v in MAPPED}                      # the PUF-concept slot
        self.reported_slot = {v: P.index(v + '_reported') for v in MAPPED}
        self.band_range = {v: torch.tensor(range_of_band(v), device=device) for v in MAPPED}
        self.n_ranges = {v: len(RANGES[v]) + 2 for v in MAPPED}
        self.scf_rows = tr['scf'].nonzero().squeeze(1)
        self.position = torch.full((len(tr['values']),), -1, dtype=torch.long, device=device)   # row -> SCF position
        self.position[self.scf_rows] = torch.arange(len(self.scf_rows), device=device)
        keys = ['year'] + [M.column(v) for v in TYPE]
        self.type_of = torch.tensor(train_rows.groupby(keys, sort=False).ngroup().to_numpy(),
                                    device=device)[self.scf_rows]
        self.n_types = int(self.type_of.max()) + 1
        # the population weights (survey weights, no q: qgroups.py) for the averages over a type and the log
        self.weight = tr['pop_weight'][self.scf_rows].double()
        self.puf_year_wave = torch.tensor(train_rows['year'].to_numpy() == common.PUF_YEAR, device=device)[self.scf_rows]
        scf = train_rows[(train_rows['source'] == 'scf').to_numpy()]
        # the filers' share (A, PUF side): settings training.fillin_puf_side, "data" from the PUF's training rows
        # directly (puf_data_shares), "model" from the network (puf_probs_by_band)
        self.puf_side = common.SETTINGS['training'].get('fillin_puf_side') or 'model'
        assert self.puf_side in ('model', 'data'), self.puf_side
        if self.puf_side == 'data':
            self.data_a, self.data_ok = self.puf_data_shares(train_rows, scf)
        n_f, n_n = tax_data_counts(scf)
        self.n_filer = torch.tensor(n_f, dtype=torch.float64, device=device)
        self.n_nonfiler = torch.tensor(n_n, dtype=torch.float64, device=device)
        self.cps_year = torch.tensor([M.YEARS.index(nearest_cps_year(int(y))) for y in scf['year']], device=device)
        t_np = self.type_of.cpu().numpy()
        first = pd.Series(np.arange(len(t_np))).groupby(t_np).first()     # one SCF row of each type, for the log
        self.type_labels = scf[keys].iloc[first.to_numpy()].reset_index(drop=True).assign(type=first.index.to_numpy())
        self.chunk = M.safe_chunk(len(M.SLOTS))   # the full width: safe for the source widths too
        self.dist, self.log = {}, []

    MIN_PUF_RECORDS = 30

    def puf_data_shares(self, train_rows, scf):
        """A by band among filers from the PUF's training rows directly (settings training.fillin_puf_side "data"):
        survey weights, dependent filers out (a record with no age range counts with its step 0 chance of an adult
        range), by type (married, dependents, wage band: the PUF year's, as the network's PUF view reads the SCF row's
        bands). With grouped weighting the network's own answer given the type only leans toward the top AGI groups
        (it averages over groups whose sizes q has changed), so the data is used instead. Per SCF training row: the
        shares of its type ({item: [rows, bands]}), and whether the type has at least MIN_PUF_RECORDS PUF records;
        where it has fewer, the network's answer is kept for that row (refit)."""
        puf = train_rows[(train_rows['source'] == 'puf').to_numpy()]
        w = puf['weight'].to_numpy(float)
        ar = puf['age_range'].to_numpy()
        adult = np.where(ar == -1, puf[M.AGE_PROBS[:common.ADULT_RANGES]].to_numpy(float).sum(1),
                         (ar >= 0) & (ar < common.ADULT_RANGES))
        w = w * adult
        keys = [M.column(v) for v in TYPE]
        tk = puf[keys].copy()
        sk = scf[keys].reset_index(drop=True)
        cnt = tk.assign(n=(w > 0).astype(float)).groupby(keys)['n'].sum()
        # an SCF type with no PUF record has no count (0 records: the network's answer is kept)
        ok = sk.merge(cnt.rename('n').reset_index(), on=keys, how='left')['n'].fillna(0).to_numpy() >= self.MIN_PUF_RECORDS
        out = {}
        for v in MAPPED:
            nb = self.n_bands[v]
            b = puf[M.column(v)].to_numpy()
            assert (b >= 0).all() and (b < nb).all(), v
            tab = pd.DataFrame(tk).assign(b=b, w=w).groupby(keys + ['b'])['w'].sum().unstack('b', fill_value=0.0)
            tab = tab.reindex(columns=range(nb), fill_value=0.0)
            # a type with no adult weight has no shares; it is under MIN_PUF_RECORDS, so its rows keep the network's
            tab = tab.div(tab.sum(1).replace(0, np.nan), axis=0).fillna(0.0)
            m = sk.merge(tab.reset_index(), on=keys, how='left')[list(range(nb))].fillna(0.0).to_numpy()
            out[v] = torch.tensor(m, dtype=torch.float64, device=self.device)
        okt = torch.tensor(ok, device=self.device)
        wt = self.weight
        common.say(f'  fill-in PUF side from the PUF data: {len(cnt)} types; SCF weight in a type with fewer than '
                   f'{self.MIN_PUF_RECORDS} PUF records (network answer kept) '
                   f'{100 * float(wt[~okt].sum() / wt.sum()):.2f}%')
        return out, okt

    def type_view(self, vals):
        """SCF records as the PUF would see them with only their type known: PUF label and year, every slot the
        PUF lacks absent, every slot but the type's hidden."""
        v = vals.clone()
        v[:, 0], v[:, 1] = M.SOURCES.index('puf'), M.YEARS.index(common.PUF_YEAR)
        hid = torch.zeros(len(v), len(P), dtype=torch.bool, device=self.device)
        for j, sl in enumerate(P):
            if 'puf' not in M.PRESENT_ON[sl]:
                v[:, C + j] = -1
            else:
                v[:, C + j] = v[:, C + j].clamp(min=0)
                hid[:, j] = sl not in TYPE
        return v, hid

    def cps_view(self, vals, cps_year):
        """SCF records as the CPS would see a non-filer with only its type known: CPS label, the nearest CPS year, every
        slot the CPS lacks absent, the filing flag 0, every other slot but the type's hidden (the shares present only
        where married and the base are shown, as in training: model.share_presence)."""
        v = vals.clone()
        v[:, 0], v[:, 1] = M.SOURCES.index('cps'), cps_year
        hid = torch.zeros(len(v), len(P), dtype=torch.bool, device=self.device)
        for j, sl in enumerate(P):
            if 'cps' not in M.PRESENT_ON[sl]:
                v[:, C + j] = -1
            else:
                v[:, C + j] = v[:, C + j].clamp(min=0)
                hid[:, j] = sl not in TYPE and sl != 'filer'
        v[:, C + P.index('filer')] = 0
        return M.share_presence(v, hid)

    def nonfiler_probs_by_band(self, vals, cps_year):
        """A by band among non-filers, for every mapped item: CPS label, filing flag 0, given the type only."""
        lp = M.log_probs_as(self.model, *self.cps_view(vals, cps_year), 'cps')
        out = {}
        for v in MAPPED:
            if 'cps' in M.PRESENT_ON[v]:
                out[v] = lp(self.slot[v])[:, :self.n_bands[v]].exp().double()
            else:   # an item the CPS does not measure is zero for every non-filer, as generation draws them
                out[v] = torch.zeros(len(vals), self.n_bands[v], dtype=torch.float64, device=self.device)
                out[v][:, 0] = 1.0
        return out

    def puf_probs_by_band(self, vals):
        """A by band, for every mapped item, among filers: the PUF concept given the type only, over the PUF's units
        other than dependent filers (which are not tax units in any comparison with a survey): the model's chance of
        each adult age range given the type, rescaled to sum to one, times the item's distribution given the type and
        range."""
        va, ha = self.type_view(vals)
        j_age = P.index('age_range')
        age = M.log_probs_as(self.model, va, ha, 'puf')(j_age)[:, :common.ADULT_RANGES].exp().double()
        age = age / age.sum(1, keepdim=True)
        out = {v: 0.0 for v in MAPPED}
        for k in range(common.ADULT_RANGES):
            vk, hk = va.clone(), ha.clone()
            vk[:, C + j_age], hk[:, j_age] = k, False
            lp = M.log_probs_as(self.model, vk, hk, 'puf')
            for v in MAPPED:
                out[v] = out[v] + age[:, k:k + 1] * lp(self.slot[v])[:, :self.n_bands[v]].exp().double()
        return out

    def report_probs_by_band(self, vals, v):
        """C by band: the chance of reporting each band of v, as the SCF sees the record with every PUF-concept
        slot and the reported slot of v hidden."""
        hidden = torch.zeros(len(vals), len(P), dtype=torch.bool, device=self.device)
        hidden[:, self.reported_slot[v]] = True
        for u in MAPPED:
            hidden[:, self.slot[u]] = True
        lp = M.log_probs_as(self.model, vals, hidden, 'scf')
        return lp(self.reported_slot[v])[:, :self.n_bands[v]].exp().double()

    def by_range(self, probs, v):
        return torch.zeros(len(probs), self.n_ranges[v], dtype=torch.float64,
                           device=self.device).index_add_(1, self.band_range[v], probs)

    @torch.no_grad()
    def refit(self, epoch, write_log=True):
        """Each SCF record's fill-in distribution over the PUF-concept bands of each mapped item (see the top)."""
        self.model.eval()
        a_bands, c_ranges = {v: [] for v in MAPPED}, {v: [] for v in MAPPED}
        filer_ranges, nonfiler_ranges = {v: [] for v in MAPPED}, {v: [] for v in MAPPED}
        nf, nn = self.n_filer, self.n_nonfiler
        none_in_tax_data = (nf + nn) == 0                               # such a type takes the filers' share alone
        share_f = torch.where(none_in_tax_data, 1.0, nf / (nf + nn).clamp_min(1e-30))
        for a in range(0, len(self.scf_rows), self.chunk):
            rows = slice(a, a + self.chunk)
            vals = self.tr['values'][self.scf_rows[rows]]
            puf = self.puf_probs_by_band(vals)
            if self.puf_side == 'data':   # the PUF data's shares wherever the type has enough PUF records
                ok = self.data_ok[rows][:, None]
                puf = {v: torch.where(ok, self.data_a[v][rows], puf[v]) for v in MAPPED}
            non =self.nonfiler_probs_by_band(vals, self.cps_year[rows])
            for v in MAPPED:
                a_bands[v].append(share_f[rows, None] * puf[v] + (1 - share_f[rows, None]) * non[v])
                filer_ranges[v].append(self.by_range(puf[v], v))
                nonfiler_ranges[v].append(self.by_range(non[v], v))
                c_ranges[v].append(self.by_range(self.report_probs_by_band(vals, v), v))
        w, t = self.weight, self.type_of
        type_rows = []
        for v in MAPPED:
            K = self.n_ranges[v]
            puf_probs_by_band = torch.cat(a_bands[v])
            report_probs = torch.cat(c_ranges[v])
            puf_probs_by_range = self.by_range(puf_probs_by_band, v)
            filers_by_range, nonfilers_by_range = torch.cat(filer_ranges[v]), torch.cat(nonfiler_ranges[v])
            # the law of total probability: within each type, the mixed shares over the size ranges sum to one
            per_type = lambda x: (torch.zeros(self.n_types, x.shape[1], dtype=torch.float64, device=self.device)
                                  .index_add_(0, t, w[:, None] * x)
                                  / torch.zeros(self.n_types, dtype=torch.float64, device=self.device)
                                  .index_add_(0, t, w).clamp_min(1e-30)[:, None])
            mixed_t, filers_t, nonfilers_t = per_type(puf_probs_by_range), per_type(filers_by_range), per_type(nonfilers_by_range)
            seen = torch.zeros(self.n_types, dtype=torch.bool, device=self.device)
            seen[t] = True
            sum_gap = float((mixed_t[seen].sum(1) - 1).abs().max())
            num = torch.zeros(self.n_types, K, dtype=torch.float64, device=self.device).index_add_(0, t, w[:, None] * report_probs)
            den = torch.zeros(self.n_types, dtype=torch.float64, device=self.device).index_add_(0, t, w)
            report_probs_by_type = num[t] / den[t][:, None]
            excess = (puf_probs_by_range / report_probs_by_type.clamp_min(1e-12) - 1).clamp(min=0)
            excess[:, 0], excess[:, K - 1] = 0, 0
            hidden = report_probs * excess / report_probs[:, :1].clamp_min(1e-12)
            hidden_total = hidden.sum(1)
            hidden = hidden / hidden_total.clamp(min=1)[:, None]
            dist = puf_probs_by_band * (hidden / puf_probs_by_range.clamp_min(1e-15))[:, self.band_range[v]]
            dist[:, 0] = 1 - hidden.sum(1)
            self.dist[v] = dist.clamp(min=0).float()
            zero = self.tr['rep_band'][v][self.scf_rows] == 0
            m = self.puf_year_wave
            wm = lambda x, mm: float((w[mm] * x[mm]).sum() / w[mm].sum())
            row = {'pass': epoch, 'item': v,
                   'none, reported (%)': 100 * wm(zero.double(), m),
                   'none, after fill (%)': 100 * wm(torch.where(zero, 1 - hidden.sum(1), torch.zeros_like(hidden_total)), m),
                   'none, tax data at SCF types (%)': 100 * wm(puf_probs_by_range[:, 0], m),
                   'none, PUF at SCF types (%)': 100 * wm(filers_by_range[:, 0], m),
                   'none, CPS non-filers at SCF types (%)': 100 * wm(nonfilers_by_range[:, 0], m),
                   'non-filers in the tax data at SCF types (%)': 100 * wm(1 - share_f, m),
                   'SCF units in a type with no tax-data unit (%)': 100 * wm(none_in_tax_data.double(), m),
                   'largest gap from 1, sum of mixed shares in a type': sum_gap,
                   'zero-reporters over 100% (%)': 100 * wm((hidden_total > 1).double(), m & zero)}
            n_type = lambda x: torch.zeros(self.n_types, dtype=torch.float64, device=self.device).index_add_(0, t, x) \
                / torch.zeros(self.n_types, dtype=torch.float64, device=self.device).index_add_(0, t, torch.ones_like(x))
            lab = self.type_labels
            type_rows.append(lab.assign(item=v, filers_in_tax_data=n_type(nf).cpu().numpy()[lab.type],
                                        nonfilers_in_tax_data=n_type(nn).cpu().numpy()[lab.type],
                                        none_filers=filers_t[lab.type.to_numpy(), 0].cpu().numpy(),
                                        none_nonfilers=nonfilers_t[lab.type.to_numpy(), 0].cpu().numpy(),
                                        none_mixed=mixed_t[lab.type.to_numpy(), 0].cpu().numpy(),
                                        sum_mixed=mixed_t[lab.type.to_numpy()].sum(1).cpu().numpy()))
            self.log.append(row)
            common.say('  fill ' + '  '.join(f'{k} {x:.2f}' if isinstance(x, float) else f'{k} {x}' for k, x in row.items()))
        if write_log:
            pd.DataFrame(self.log).to_csv(os.path.join(common.MODEL, 'fill_log.csv'), index=False)
            pd.concat(type_rows, ignore_index=True).assign(**{'pass': epoch}) \
                .to_csv(os.path.join(common.MODEL, 'fill_types.csv'), index=False)
        self.model.train()

    @torch.no_grad()
    def draw(self, idx):
        """PUF-concept slot of each SCF zero-reporter among the training rows idx, drawn from its fill-in
        distribution; reporters keep the reported amount."""
        p = self.position[idx]
        scf = p >= 0
        for v in MAPPED:
            z = scf & (self.tr['rep_band'][v][idx] == 0)
            if not z.any():
                continue
            band = torch.multinomial(self.dist[v][p[z]], 1, generator=self.gen).squeeze(1)
            self.tr['values'][idx[z], C + self.slot[v]] = band

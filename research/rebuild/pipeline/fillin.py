"""Function 2, holding: the two-slot fill-in, run during training (train.py).

Surveys miss holders: units that report zero wages, interest, dividends, capital gains, pensions or business income
but hold some. Each row of the translation table with fill_in (translate.py; since 28 October on every survey: the
SCF, the CEX, the CPS and the SIPP, and on wages) gives its item two slots on that survey's rows: the reported amount
(model.REPORTED, never changed; one slot per item, on every survey that fills the item in) and the PUF-concept amount
(the shared slot), which reporters copy and zero-reporters get filled in. From the pass fill_from_pass on, at the
start of each pass, refit() gives every zero-reporter of every row its chance of a hidden holding in each size range,
from the reporting-rate formula, row by row (survey s, item v):
  type                     the survey year and the row's type variables (translate.fill_type): its survey's
                           variables with type_for_fill_in (married, dependents, the wage band) less the item itself
                           (wages is not in its own type), plus the row's type_extra (the SCF's Schedule E rest: the
                           partnership flag). The type is read from the table's bands, which hold the reported amounts.
  report_probs (C_k)       the unit's chance of reporting range k: label s, the record as the survey has it, the
                           reported slot of v and every PUF-concept slot the survey fills in hidden (so no fill-in ever
                           feeds the formula);
  report_probs_by_type (B_k)  C_k averaged over the survey's units of the same type;
  puf_probs_by_range (A_k) the tax data's share given the type only: the count-weighted average of the filers' share
                           and the non-filers' share (the law of total probability),
                             A_k = (N_f A_k(filers) + N_n A_k(non-filers)) / (N_f + N_n),
                           N_f and N_n the filers and non-filers of the type in the tax data for the survey year
                           (build_table.py, tax_data; data/tax_data_<year>.parquet). Filers: with settings
                           training.fillin_puf_side "data", the PUF training rows' own survey-weighted shares by type
                           (dependent filers out, a record with no age range at its step 0 chance of an adult range);
                           the network's answer (PUF label, the PUF year, the type shown, mixed over the six adult age
                           ranges by the network's chance of each) for a type with fewer than 30 PUF records, or with
                           "model" everywhere. The amount compared is translate.against(row): the row's own variable,
                           or its rank_against total (the CPS's dividends against ordinary dividends; its pensions
                           against taxable pensions plus taxable IRA distributions), each on the slot's PUF-year cut
                           points (the network's answer is the slot's own variable: an approximation, counted in the
                           log). Non-filers: CPS label, the CPS year nearest the survey year, the filing flag 0,
                           everything else but the type hidden; an item the CPS does not measure (interest) has no
                           non-filer holder. A type with no unit in the tax data takes the filers' share alone;
  excess (e_k)             max(A_k / B_k - 1, 0): how many more holders the tax data has than the survey reports, per
                           reporter;
  hidden (h_k)             C_k * e_k / C_0, capped at 100% in total.
Within a range, bands follow A. Then draw() redraws, in each batch, the PUF-concept slot of the row's zero-reporters
from those chances. A depends on the type only, so it is computed on one record of each type. The formula assumes a
holder's chance of reporting depends only on its type and true amount, not on anything else on the record.

A row fills in only in the survey years from its first_year on (translate.fills_in): a year the survey does not ask
the item (the CPS's capital gains before tax year 2018) has no reporters; since 29 October the item is missing there
(absent, not shown in training), so there is nothing to fill in.

Writes common.MODEL/fill_log.csv: per pass and row, on the survey's year nearest the target year, the share with none
as reported, after the fill-in, in the tax data at the survey's types (the mixed share), among filers only and among
non-filers only; the share of zero-reporters capped; the largest gap from 1 of the mixed shares' sum over size ranges
in any type; the survey weight in a type whose filers' share is the network's. fill_ranges.csv: per pass, row, year
and size range, the survey's reported share, its expected share after the fill-in, and the tax data's at its types.
fill_types.csv: the last refit's shares of none by row and type. report_translation.py calls refit() on a trained
model for the report's stage after function 2.
"""
import math
import os

import numpy as np
import pandas as pd
import torch

import common
import model as M
import translate as TR

ROWS = TR.filled()
KEYS = [(r['survey'], r['variable']) for r in ROWS]
ROW = dict(zip(KEYS, ROWS))
MAPPED = list(dict.fromkeys(v for _, v in KEYS))       # the items with a reported slot
SURVEYS = list(dict.fromkeys(s for s, _ in KEYS))
TYPE = {k: TR.fill_type(r) for k, r in ROW.items()}
C = len(M.CONTEXT)
P = M.PREDICTED
CPS_YEARS = [y for y in common.SETTINGS['cps']['years'] if y in M.YEARS]
RANGES = {k: [(-math.inf if lo is None else lo, math.inf if hi is None else hi) for lo, hi in r['fill_in']['size_ranges']]
          for k, r in ROW.items()}
for _k in KEYS:
    for _v in TYPE[_k]:
        assert _v in M.PREDICTED, f'{_k}: type variable {_v} is not a slot'
        assert _k[0] in M.PRESENT_ON[_v] and 'puf' in M.PRESENT_ON[_v], f'{_k}: type variable {_v} not on the PUF and {_k[0]}'
MIN_PUF_RECORDS = 30


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


def type_keys(df, survey, y, type_vars, banded):
    """The type columns of df (one survey year): categories as they are; an amount as its band, read from the band
    column if banded, else put on the survey year's cut points from its dollars."""
    out = {}
    for v in type_vars:
        if v in M.CATEGORICAL:
            out[v] = df[v].to_numpy(np.int64)
        elif banded:
            out[v] = df[v + '_band'].to_numpy(np.int64)
        else:
            out[v] = band_on_edge(df[v].to_numpy(float), common.edges()[f'{survey}/{y}/{v}'],
                                  M.BAND_LAYOUT[v]['first_positive'])
    return pd.DataFrame(out, index=df.index)


def range_of_band(key):
    """Size range of every band of the row's item by its PUF-year median: 0 = zero, 1..K = the ranges, K + 1 = a band
    with no PUF records."""
    v = key[1]
    out = []
    for bd in common.edges()[f'puf/{common.PUF_YEAR}/{v}']['bands']:
        if bd['band'] == 0:
            out.append(0)
        elif bd['median'] is None:
            out.append(len(RANGES[key]) + 1)
        else:
            hit = [i for i, (lo, hi) in enumerate(RANGES[key]) if in_range(bd['median'], lo, hi)]
            assert len(hit) == 1, (key, bd)
            out.append(hit[0] + 1)
    return np.array(out, dtype=np.int64)


def against_bands(df, key):
    """Bands of the row's tax-data total (translate.against) on the item's PUF-year cut points, from df's dollars."""
    r = ROW[key]
    x = sum(df[a].to_numpy(float) for a in TR.against(r))
    b = band_on_edge(x, common.edges()[f'puf/{common.PUF_YEAR}/{key[1]}'], M.BAND_LAYOUT[key[1]]['first_positive'])
    if TR.against(r) == [key[1]]:
        assert (b == df[key[1] + '_band'].to_numpy()).all(), f'{key}: bands from the cut points differ from the table\'s'
    return b


class View:
    """The units of one survey grouped by one list of type variables (the rows of the survey sharing it): each unit's
    type, one record per type, and each type's filers and non-filers in the tax data."""

    def __init__(self, survey, type_vars, frame, device):
        self.survey, self.type_vars = survey, type_vars
        keys = ['year'] + [M.column(v) for v in type_vars]
        g = frame.groupby(keys, sort=False)
        self.type_of_np = g.ngroup().to_numpy()
        self.type_of = torch.tensor(self.type_of_np, device=device)
        self.n_types = int(self.type_of_np.max()) + 1
        first = pd.Series(np.arange(len(frame))).groupby(self.type_of_np).first().sort_index()
        self.first = torch.tensor(first.to_numpy(), device=device)       # one survey record of each type
        self.labels = frame[keys].iloc[first.to_numpy()].reset_index(drop=True).assign(type=np.arange(self.n_types))
        self.year = self.labels['year'].to_numpy()
        self.n_filer, self.n_nonfiler = self.tax_data_counts(frame)

    def tax_data_counts(self, frame):
        """Per type: the filers and the non-filers of the type in the tax data for its survey year."""
        n_f, n_n = np.zeros(self.n_types), np.zeros(self.n_types)
        for y in sorted(frame['year'].unique()):
            sel = (frame['year'] == y).to_numpy()
            d = frame[sel]
            own = type_keys(d, self.survey, y, self.type_vars, banded=True)
            again = type_keys(d, self.survey, y, self.type_vars, banded=False)
            for v in self.type_vars:   # the bands put on the cut points from the dollars must be the table's own
                if v not in M.CATEGORICAL:
                    assert (again[v].to_numpy() == own[v].to_numpy()).all(), f'{self.survey} {y} {v}: bands differ'
            T = pd.read_parquet(f'{common.DATA}/tax_data_{y}.parquet')
            assert T['filer'].isin([0, 1]).all() and (T['weight'] >= 0).all()
            tk = type_keys(T, self.survey, y, self.type_vars, banded=False)
            tk['filer'], tk['weight'] = T['filer'].to_numpy(), T['weight'].to_numpy(float)
            counts = tk.groupby(self.type_vars + ['filer'])['weight'].sum().unstack('filer', fill_value=0.0)
            lab = self.labels[self.labels.year == y]
            lab_keys = lab[[M.column(v) for v in self.type_vars]].set_axis(self.type_vars, axis=1)
            m = lab_keys.merge(counts, left_on=self.type_vars, right_index=True, how='left').fillna(0.0)
            n_f[lab['type'].to_numpy()] = m[1].to_numpy() if 1 in m else 0.0
            n_n[lab['type'].to_numpy()] = m[0].to_numpy() if 0 in m else 0.0
            w = d['weight'].to_numpy(float)
            t = self.type_of_np[sel]
            common.say(f'  fill-in {self.survey} {y} type {self.type_vars}: {T.loc[T.filer == 1, "weight"].sum() / 1e6:.2f}M '
                       f'filers, {T.loc[T.filer == 0, "weight"].sum() / 1e6:.2f}M non-filers in the tax data; survey units '
                       f'in a type with no tax-data unit {100 * w[(n_f[t] + n_n[t]) == 0].sum() / w.sum():.2f}%')
        return n_f, n_n


class FillIn:
    """The fill-in for the training records tr (train.py's encoded training split; train_rows its table rows)."""

    def __init__(self, model, tr, train_rows, gen):
        self.model, self.tr, self.gen = model, tr, gen
        device = tr['values'].device
        self.device = device
        self.n_bands = {v: M.N_CHOICES[v] for v in MAPPED}
        self.slot = {v: P.index(v) for v in MAPPED}                      # the PUF-concept slot
        self.reported_slot = {v: P.index(v + '_reported') for v in MAPPED}
        self.band_range = {k: torch.tensor(range_of_band(k), device=device) for k in KEYS}
        self.n_ranges = {k: len(RANGES[k]) + 2 for k in KEYS}
        self.puf_side = common.SETTINGS['training'].get('fillin_puf_side') or 'model'
        assert self.puf_side in ('model', 'data'), self.puf_side
        self.recs, self.weight, self.frame, self.cps_year = {}, {}, {}, {}
        self.position = torch.full((len(tr['values']),), -1, dtype=torch.long, device=device)   # row -> survey position
        for s in SURVEYS:
            self.recs[s] = tr[s].nonzero().squeeze(1)
            self.position[self.recs[s]] = torch.arange(len(self.recs[s]), device=device)
            self.frame[s] = train_rows[(train_rows['source'] == s).to_numpy()].reset_index(drop=True)
            # the population weights (survey weights, no q: qgroups.py) for the averages over a type and the log
            self.weight[s] = tr['pop_weight'][self.recs[s]].double()
        self.views, self.view_of = {}, {}
        for k in KEYS:
            vk = (k[0], tuple(TYPE[k]))
            if vk not in self.views:
                self.views[vk] = View(k[0], list(TYPE[k]), self.frame[k[0]], device)
            self.view_of[k] = vk
        # per row: which survey records fill in (their year), which of those are zero-reporters, and the data shares
        self.active, self.zero, self.zpos = {}, {}, {}
        for k in KEYS:
            s, v = k
            yr = self.frame[s]['year'].to_numpy()
            act = np.array([TR.fills_in(ROW[k], int(y)) for y in yr])
            self.active[k] = torch.tensor(act, device=device)
            # the item is present on every record of the years it fills in; absent only in a year the survey does not ask
            # it (translate.not_asked: the CPS's capital gains before tax year 2018, since 29 October)
            assert bool(((tr['rep_band'][v][self.recs[s]] >= 0) | ~self.active[k]).all()), \
                f'{k}: the item is absent on some {s} records of a year it fills in'
            self.zero[k] = self.active[k] & (tr['rep_band'][v][self.recs[s]] == 0)
            z = self.zero[k].nonzero().squeeze(1)
            self.zpos[k] = torch.full((len(self.recs[s]),), -1, dtype=torch.long, device=device)
            self.zpos[k][z] = torch.arange(len(z), device=device)
        if self.puf_side == 'data':
            self.data_a, self.data_ok = {}, {}
            puf = train_rows[(train_rows['source'] == 'puf').to_numpy()]
            for k in KEYS:
                self.data_a[k], self.data_ok[k] = self.puf_data_shares(puf, k)
        self.totals_setup(train_rows)
        self.chunk = M.safe_chunk(len(M.SLOTS))   # the full width: safe for the source widths too
        self.dist, self.log, self.range_log = {}, [], []
        for s in SURVEYS:
            common.say(f'  fill-in {s}: {len(self.recs[s]):,} training records; rows '
                       + ', '.join(f'{v} (type {TYPE[(s, v)]}, zero-reporters {int(self.zero[(s, v)].sum()):,})'
                                   for ss, v in KEYS if ss == s))

    def totals_setup(self, train_rows):
        """The computed totals on a survey's rows with a filled-in part (28 October; the CPS: ordinary dividends from
        qualified dividends, capital gains from long-term gains, Schedules C and F and business income from Schedule
        C), recomputed after each draw so a record's total agrees with its filled-in parts (draw): each part's band as
        dollars (the survey year's own median in the band, else the PUF-year median grown to the year), summed with
        the total's signs, cut on the survey year's cut points of the total. self.totals[s] = {total: parts};
        self.dollars[(s, part)] = [years, bands]; self.cuts[(s, total)] = per year (negative cuts, positive cuts)."""
        edges, growth = common.edges(), common.meta()['growth_factors']
        self.totals, self.dollars, self.cuts = {}, {}, {}
        filled_on = {s: {v for ss, v in KEYS if ss == s} for s in SURVEYS}
        for s in SURVEYS:
            years = sorted(int(y) for y in self.frame[s]['year'].unique())
            tots = {t: p for t, p in M.COMPUTED.items() if s in M.PRESENT_ON[t] and t not in filled_on[s]
                    and set(p) & filled_on[s] and all(s in M.PRESENT_ON[q] for q in p)}
            if not tots:
                continue
            self.totals[s] = tots
            missing = 0
            for part in sorted({q for p in tots.values() for q in p}):
                nb = M.N_CHOICES[part]
                tab = torch.zeros(len(M.YEARS), nb, dtype=torch.float64)
                puf_med = [b['median'] for b in edges[f'puf/{common.PUF_YEAR}/{part}']['bands']]
                for y in years:
                    fp, fn = growth[str(y)].get(part, (1.0, 1.0))
                    for b in edges[f'{s}/{y}/{part}']['bands'][1:]:
                        k = b['band']
                        m = b['median'] if b['records'] else (None if puf_med[k] is None else
                                                              puf_med[k] * (fp if puf_med[k] > 0 else fn))
                        missing += m is None
                        tab[M.YEARS.index(y), k] = 0.0 if m is None else m
                self.dollars[(s, part)] = tab.to(self.device)
            for t in tots:
                self.cuts[(s, t)] = {M.YEARS.index(y): (torch.tensor(edges[f'{s}/{y}/{t}']['negative']['cuts'], dtype=torch.float64, device=self.device),
                                                       torch.tensor(edges[f'{s}/{y}/{t}'].get('positive', {}).get('cuts', []), dtype=torch.float64, device=self.device))
                                     for y in years}
            common.say(f'  fill-in {s}: totals recomputed from filled-in parts: {list(tots)}; part bands with no record '
                       f'in the survey year or the PUF (read as zero dollars): {missing}')

    def recompute_totals(self, ii, s):
        """The computed totals of survey s's records ii from their parts' bands (see totals_setup). A total with a
        missing part (band -1) stays missing (band -1): the dollar table is never read at band -1, which Python would
        read as the top band (the bug of 29 October, explore/2026-10-29/diagnose/, section 3)."""
        v = self.tr['values']
        yr = v[ii, 1]
        for t, parts in self.totals[s].items():
            pb = {q: v[ii, C + P.index(q)] for q in parts}
            missing = torch.zeros(len(ii), dtype=torch.bool, device=self.device)
            for b in pb.values():
                missing |= b < 0
            x = sum(sg * self.dollars[(s, q)][yr, pb[q].clamp(min=0)] for q, sg in parts.items())
            band = torch.zeros(len(ii), dtype=torch.long, device=self.device)
            fp = M.BAND_LAYOUT[t]['first_positive']
            for y in torch.unique(yr).tolist():
                neg, pos = self.cuts[(s, t)][y]
                m = yr == y
                xm = x[m]
                bm = torch.zeros(len(xm), dtype=torch.long, device=self.device)
                if len(neg):
                    bm = torch.where(xm < 0, 1 + torch.searchsorted(neg, xm.contiguous(), side='left'), bm)
                if len(pos):
                    bm = torch.where(xm > 0, fp + torch.searchsorted(pos, xm.contiguous(), side='left'), bm)
                band[m] = bm
            band = torch.where(missing, torch.full_like(band, -1), band)
            assert bool((band[~missing] >= 0).all()), f'{s} {t}: a negative band index on a total with every part present'
            v[ii, C + P.index(t)] = band

    def puf_data_shares(self, puf, key):
        """The filers' share by band from the PUF's training rows directly (settings training.fillin_puf_side "data"):
        survey weights, dependent filers out (a record with no age range counts with its step 0 chance of an adult
        range), by the row's type (as the network's PUF view reads the survey row's bands), of the row's tax-data total
        (against_bands). Per type of the row's view: the shares [types, bands], and whether the type has at least
        MIN_PUF_RECORDS PUF records (elsewhere refit keeps the network's answer)."""
        view = self.views[self.view_of[key]]
        w = puf['weight'].to_numpy(float)
        ar = puf['age_range'].to_numpy()
        adult = np.where(ar == -1, puf[M.AGE_PROBS[:common.ADULT_RANGES]].to_numpy(float).sum(1),
                         (ar >= 0) & (ar < common.ADULT_RANGES))
        w = w * adult
        keys = [M.column(v) for v in view.type_vars]
        lab = view.labels[keys]
        nb = self.n_bands[key[1]]
        b = against_bands(puf, key)
        assert (b >= 0).all() and (b < nb).all(), key
        if keys:
            tk = puf[keys].reset_index(drop=True)
            cnt = tk.assign(n=(w > 0).astype(float)).groupby(keys)['n'].sum()
            ok = lab.merge(cnt.rename('n').reset_index(), on=keys, how='left')['n'].fillna(0).to_numpy() >= MIN_PUF_RECORDS
            tab = tk.assign(b=b, w=w).groupby(keys + ['b'])['w'].sum().unstack('b', fill_value=0.0)
            tab = tab.reindex(columns=range(nb), fill_value=0.0)
            tab = tab.div(tab.sum(1).replace(0, np.nan), axis=0).fillna(0.0)
            m = lab.merge(tab.reset_index(), on=keys, how='left')[list(range(nb))].fillna(0.0).to_numpy()
        else:
            ok = np.full(len(lab), (w > 0).sum() >= MIN_PUF_RECORDS)
            m = np.tile(np.bincount(b, weights=w, minlength=nb) / w.sum(), (len(lab), 1))
        return torch.tensor(m, dtype=torch.float64, device=self.device), torch.tensor(ok, device=self.device)

    def reported_values(self, idx):
        """The training records idx with every PUF-concept slot of a filled item set back to the reported band, so no
        fill-in drawn in an earlier batch is read by the formula."""
        vals = self.tr['values'][idx].clone()
        for v in MAPPED:
            vals[:, C + self.slot[v]] = torch.where(vals[:, C + self.slot[v]] >= 0, self.tr['rep_band'][v][idx],
                                                    vals[:, C + self.slot[v]])
        return vals

    def type_view(self, vals, type_vars):
        """Records as the PUF would see them with only their type known: PUF label and year, every slot the PUF lacks
        absent, every slot but the type's hidden."""
        v = vals.clone()
        v[:, 0], v[:, 1] = M.SOURCES.index('puf'), M.YEARS.index(common.PUF_YEAR)
        hid = torch.zeros(len(v), len(P), dtype=torch.bool, device=self.device)
        for j, sl in enumerate(P):
            if 'puf' not in M.PRESENT_ON[sl]:
                v[:, C + j] = -1
            else:
                v[:, C + j] = v[:, C + j].clamp(min=0)
                hid[:, j] = sl not in type_vars
        return v, hid

    def cps_view(self, vals, cps_year, type_vars):
        """Records as the CPS would see a non-filer with only its type known: CPS label, the nearest CPS year, every
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
                hid[:, j] = sl not in type_vars and sl != 'filer'
        v[:, C + P.index('filer')] = 0
        return M.share_presence(v, hid)

    def nonfiler_probs_by_band(self, vals, cps_year, type_vars, items):
        """A by band among non-filers, for each item: CPS label, filing flag 0, given the type only."""
        lp = M.log_probs_as(self.model, *self.cps_view(vals, cps_year, type_vars), 'cps')
        out = {}
        for v in items:
            if 'cps' in M.PRESENT_ON[v]:
                out[v] = lp(self.slot[v])[:, :self.n_bands[v]].exp().double()
            else:   # an item the CPS does not measure is zero for every non-filer, as generation draws them
                out[v] = torch.zeros(len(vals), self.n_bands[v], dtype=torch.float64, device=self.device)
                out[v][:, 0] = 1.0
        return out

    def puf_probs_by_band(self, vals, type_vars, items):
        """A by band among filers, for each item: the PUF concept given the type only, over the PUF's units other than
        dependent filers: the network's chance of each adult age range given the type, rescaled to sum to one, times
        the item's distribution given the type and range."""
        va, ha = self.type_view(vals, type_vars)
        j_age = P.index('age_range')
        age = M.log_probs_as(self.model, va, ha, 'puf')(j_age)[:, :common.ADULT_RANGES].exp().double()
        age = age / age.sum(1, keepdim=True)
        out = {v: 0.0 for v in items}
        for k in range(common.ADULT_RANGES):
            vk, hk = va.clone(), ha.clone()
            vk[:, C + j_age], hk[:, j_age] = k, False
            lp = M.log_probs_as(self.model, vk, hk, 'puf')
            for v in items:
                out[v] = out[v] + age[:, k:k + 1] * lp(self.slot[v])[:, :self.n_bands[v]].exp().double()
        return out

    def report_probs_by_band(self, vals, key):
        """C by band: the chance of reporting each band of the row's item, as the survey sees the record with every
        PUF-concept slot it fills in and the item's reported slot hidden, and also (28 October) every slot that would
        give a filled-in amount away: a computed total with a filled-in part (the CPS's ordinary dividends, capital
        gains, Schedules C and F, business income) hidden, and a CPS share whose base is filled in (the spouse's share
        of the couple's wages) left out, as model.gate_shares does when its base is hidden."""
        s, v = key
        hidden = torch.zeros(len(vals), len(P), dtype=torch.bool, device=self.device)
        hidden[:, self.reported_slot[v]] = True
        for ss, u in KEYS:
            if ss == s:
                hidden[:, self.slot[u]] = True
        for tot in self.totals.get(s, {}):
            hidden[:, P.index(tot)] = vals[:, C + P.index(tot)] >= 0
        vals = vals.clone()
        for j, (_, b) in M.SHARE_GATES.items():
            if hidden[0, b]:
                vals[:, C + j], hidden[:, j] = -1, False
        lp = M.log_probs_as(self.model, vals, hidden, s)
        return lp(self.reported_slot[v])[:, :self.n_bands[v]].exp().double()

    def by_range(self, probs, key):
        return torch.zeros(len(probs), self.n_ranges[key], dtype=torch.float64,
                           device=self.device).index_add_(1, self.band_range[key], probs)

    def type_shares(self, vk):
        """Per type of a view, for each of its rows: the filers' share (network or data), the non-filers' share
        (network) and the mixed share A by band; and the survey weight in types whose filers' share is the network's."""
        view = self.views[vk]
        s = vk[0]
        rows = [k for k in KEYS if self.view_of[k] == vk]
        items = list(dict.fromkeys(k[1] for k in rows))
        reps = self.recs[s][view.first]
        filers = {k: [] for k in rows}
        non = {k: [] for k in rows}
        for a in range(0, len(reps), self.chunk):
            sl = slice(a, a + self.chunk)
            vals = self.reported_values(reps[sl])
            cy = torch.tensor([M.YEARS.index(nearest_cps_year(int(y))) for y in view.year[sl]], device=self.device)
            nf = self.nonfiler_probs_by_band(vals, cy, view.type_vars, items)
            need = self.puf_side == 'model' or any(not bool(self.data_ok[k][sl].all()) for k in rows)
            pf = self.puf_probs_by_band(vals, view.type_vars, items) if need else None
            for k in rows:
                if self.puf_side == 'data':
                    ok = self.data_ok[k][sl][:, None]
                    f = self.data_a[k][sl] if pf is None else torch.where(ok, self.data_a[k][sl], pf[k[1]])
                else:
                    f = pf[k[1]]
                filers[k].append(f)
                non[k].append(nf[k[1]])
        nf_, nn_ = (torch.tensor(x, dtype=torch.float64, device=self.device) for x in (view.n_filer, view.n_nonfiler))
        none_in_tax_data = (nf_ + nn_) == 0                       # such a type takes the filers' share alone
        share_f = torch.where(none_in_tax_data, 1.0, nf_ / (nf_ + nn_).clamp_min(1e-30))
        out = {}
        for k in rows:
            f, n = torch.cat(filers[k]), torch.cat(non[k])
            out[k] = {'filers': f, 'nonfilers': n, 'mixed': share_f[:, None] * f + (1 - share_f[:, None]) * n,
                      'share_f': share_f, 'none_in_tax_data': none_in_tax_data,
                      'network': (~self.data_ok[k]) if self.puf_side == 'data'
                      else torch.ones(view.n_types, dtype=torch.bool, device=self.device)}
        return out

    @torch.no_grad()
    def refit(self, epoch, write_log=True):
        """Each zero-reporter's fill-in distribution over the PUF-concept bands of each row's item (see the top)."""
        self.model.eval()
        type_rows = []
        for vk in self.views:
            shares = self.type_shares(vk)
            view = self.views[vk]
            s = vk[0]
            t = view.type_of
            w = self.weight[s]
            for k in [k for k in KEYS if self.view_of[k] == vk]:
                v, K = k[1], self.n_ranges[k]
                c = torch.cat([self.by_range(self.report_probs_by_band(self.reported_values(self.recs[s][a:a + self.chunk]), k), k)
                               for a in range(0, len(self.recs[s]), self.chunk)])
                per_type = lambda x: (torch.zeros(view.n_types, x.shape[1], dtype=torch.float64, device=self.device)
                                      .index_add_(0, t, w[:, None] * x)
                                      / torch.zeros(view.n_types, dtype=torch.float64, device=self.device)
                                      .index_add_(0, t, w).clamp_min(1e-30)[:, None])
                a_bands = shares[k]['mixed']                                # [types, bands]
                a_range = self.by_range(a_bands, k)                         # [types, ranges]
                b_range = per_type(c)                                       # [types, ranges]
                excess = (a_range / b_range.clamp_min(1e-12) - 1).clamp(min=0)
                excess[:, 0], excess[:, K - 1] = 0, 0
                hidden = c * excess[t] / c[:, :1].clamp_min(1e-12)
                hidden = torch.where(self.active[k][:, None], hidden, torch.zeros_like(hidden))
                hidden_total = hidden.sum(1)
                hidden = hidden / hidden_total.clamp(min=1)[:, None]
                z = self.zero[k]
                ab = a_bands[t[z]]
                dist = ab * (hidden[z] / a_range[t[z]].clamp_min(1e-15))[:, self.band_range[k]]
                dist[:, 0] = 1 - hidden[z].sum(1)
                self.dist[k] = dist.clamp(min=0).float()
                # the log: the survey's year nearest the target year
                # the reported band's size range; a record whose reported item is missing (a year the row does not
                # fill in) is never looked up at band -1, and is left out of the log
                rb = self.tr['rep_band'][v][self.recs[s]]
                known = rb >= 0
                assert bool((known | ~self.active[k]).all()), f'{k}: a negative band index in a year the row fills in'
                rep_range = self.band_range[k][rb.clamp(min=0)]
                rep_onehot = torch.zeros(len(rep_range), K, dtype=torch.float64, device=self.device)
                rep_onehot[torch.arange(len(rep_range), device=self.device), rep_range] = known.double()
                after = torch.where(z[:, None], hidden, rep_onehot)
                after[:, 0] = torch.where(z, 1 - hidden.sum(1), after[:, 0])
                years = sorted(set(view.year.tolist()))
                log_year = min(years, key=lambda y: (abs(y - common.TARGET_YEAR), -y))
                yr = torch.tensor(self.frame[s]['year'].to_numpy(), device=self.device)
                wm = lambda x, mm: float((w[mm] * x[mm]).sum() / w[mm].sum()) if bool(mm.any()) else float('nan')
                m = (yr == log_year) & known
                sh = shares[k]
                row = {'pass': epoch, 'survey': s, 'item': v, 'year': log_year,
                       'none, reported (%)': 100 * wm((rep_range == 0).double(), m),
                       'none, after fill (%)': 100 * wm(after[:, 0], m),
                       'none, tax data at survey types (%)': 100 * wm(a_range[t, 0], m),
                       'none, PUF filers at survey types (%)': 100 * wm(self.by_range(sh['filers'], k)[t, 0], m),
                       'none, CPS non-filers at survey types (%)': 100 * wm(self.by_range(sh['nonfilers'], k)[t, 0], m),
                       'non-filers in the tax data at survey types (%)': 100 * wm(1 - sh['share_f'][t], m),
                       'survey units in a type with no tax-data unit (%)': 100 * wm(sh['none_in_tax_data'][t].double(), m),
                       'survey units whose filers\' share is the network\'s (%)': 100 * wm(sh['network'][t].double(), m),
                       'largest gap from 1, sum of mixed shares in a type': float((a_range.sum(1) - 1).abs().max()),
                       'zero-reporters over 100% (%)': 100 * wm((hidden_total > 1).double(), m & z)}
                self.log.append(row)
                common.say('  fill ' + '  '.join(f'{a} {x:.2f}' if isinstance(x, float) else f'{a} {x}' for a, x in row.items()))
                labels = ['none'] + [f'{lo:g} to {hi:g}' for lo, hi in RANGES[k]] + ['band with no PUF record']
                for y in years:
                    my = (yr == y) & self.active[k]
                    if not bool(my.any()):
                        continue
                    for j in range(K):
                        self.range_log.append({'pass': epoch, 'survey': s, 'item': v, 'year': y, 'range': labels[j],
                                               'reported (%)': 100 * wm(rep_onehot[:, j], my),
                                               'after fill-in (%)': 100 * wm(after[:, j], my),
                                               'tax data at survey types (%)': 100 * wm(a_range[t, j], my)})
                lab = view.labels
                type_rows.append(lab.assign(survey=s, item=v, filers_in_tax_data=view.n_filer, nonfilers_in_tax_data=view.n_nonfiler,
                                            none_filers=self.by_range(sh['filers'], k)[:, 0].cpu().numpy(),
                                            none_nonfilers=self.by_range(sh['nonfilers'], k)[:, 0].cpu().numpy(),
                                            none_mixed=a_range[:, 0].cpu().numpy(),
                                            none_reported=b_range[:, 0].cpu().numpy()))
        if write_log:
            pd.DataFrame(self.log).to_csv(os.path.join(common.MODEL, 'fill_log.csv'), index=False)
            pd.DataFrame(self.range_log).to_csv(os.path.join(common.MODEL, 'fill_ranges.csv'), index=False)
            pd.concat(type_rows, ignore_index=True).assign(**{'pass': epoch}) \
                .to_csv(os.path.join(common.MODEL, 'fill_types.csv'), index=False)
        self.model.train()

    @torch.no_grad()
    def draw(self, idx):
        """The PUF-concept slot of each zero-reporter among the training rows idx, for every row, drawn from its
        fill-in distribution; reporters keep the reported amount."""
        for k in KEYS:
            s, v = k
            ii = idx[self.tr[s][idx]]
            if not len(ii):
                continue
            zp = self.zpos[k][self.position[ii]]
            z = zp >= 0
            if not z.any():
                continue
            band = torch.multinomial(self.dist[k][zp[z]], 1, generator=self.gen).squeeze(1)
            self.tr['values'][ii[z], C + self.slot[v]] = band
        for s in self.totals:   # totals with a filled-in part, on the records with any filled-in part redrawn
            ii = idx[self.tr[s][idx]]
            if not len(ii):
                continue
            pos = self.position[ii]
            redrawn = torch.zeros(len(ii), dtype=torch.bool, device=self.device)
            for (ss, v) in KEYS:
                if ss == s and any(v in p for p in self.totals[s].values()):
                    redrawn |= self.zpos[(ss, v)][pos] >= 0
            if redrawn.any():
                self.recompute_totals(ii[redrawn], s)

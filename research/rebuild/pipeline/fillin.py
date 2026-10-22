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
  puf_probs_by_range (A_k) the PUF concept given the type only (PUF label, the
                           PUF year), over the PUF's units other than dependent
                           filers: the item given the type and each adult age
                           range, mixed by the model's chance of that range;
  excess (e_k)             max(A_k / B_k - 1, 0): how many more holders the PUF
                           has than the survey reports, per reporter;
  hidden (h_k)             C_k * e_k / C_0, capped at 100% in total.
Within a range, bands follow A. Then draw() redraws, in each batch, the
PUF-concept slot of its zero-reporters from those chances. The formula assumes
a holder's chance of reporting depends only on its type and true amount, not
on anything else on the record.

Writes common.MODEL/fill_log.csv: per pass and item, the share with none as
reported, after the fill-in and on the PUF at the SCF's types (the PUF-year
wave), and the share of zero-reporters capped. report_translation.py calls
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
RANGES = {v: [(-math.inf if lo is None else lo, math.inf if hi is None else hi) for lo, hi in r['fill_in']['size_ranges']]
          for v, r in ROWS.items()}


def in_range(x, lo, hi):
    """Small positive ranges are (0, hi); every other range is [lo, hi)."""
    return 0 < x < hi if lo == 0 else lo <= x < hi


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
        self.weight = tr['weight'][self.scf_rows].double()
        self.puf_year_wave = torch.tensor(train_rows['year'].to_numpy() == common.PUF_YEAR, device=device)[self.scf_rows]
        self.chunk = M.safe_chunk(len(M.SLOTS))   # the full width: safe for the source widths too
        self.dist, self.log = {}, []

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

    def puf_probs_by_band(self, vals):
        """A by band, for every mapped item: the PUF concept given the type only, over the PUF's units other than
        dependent filers (which are not tax units in any comparison with a survey): the model's chance of each adult
        age range given the type, rescaled to sum to one, times the item's distribution given the type and range."""
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
        for a in range(0, len(self.scf_rows), self.chunk):
            vals = self.tr['values'][self.scf_rows[a:a + self.chunk]]
            puf = self.puf_probs_by_band(vals)
            for v in MAPPED:
                a_bands[v].append(puf[v])
                c_ranges[v].append(self.by_range(self.report_probs_by_band(vals, v), v))
        w, t = self.weight, self.type_of
        for v in MAPPED:
            K = self.n_ranges[v]
            puf_probs_by_band = torch.cat(a_bands[v])
            report_probs = torch.cat(c_ranges[v])
            puf_probs_by_range = self.by_range(puf_probs_by_band, v)
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
                   'none, PUF at SCF types (%)': 100 * wm(puf_probs_by_range[:, 0], m),
                   'zero-reporters over 100% (%)': 100 * wm((hidden_total > 1).double(), m & zero)}
            self.log.append(row)
            common.say('  fill ' + '  '.join(f'{k} {x:.2f}' if isinstance(x, float) else f'{k} {x}' for k, x in row.items()))
        if write_log:
            pd.DataFrame(self.log).to_csv(os.path.join(common.MODEL, 'fill_log.csv'), index=False)
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

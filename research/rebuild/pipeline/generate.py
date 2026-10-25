"""Generation: PUF-style records drawn from the trained model.

Two batches (step 2, 23 October): filers drawn from the PUF label at the PUF
year (filing flag 1), and non-filers drawn from the CPS label at the target
year (filing flag 0). The filing flag is the batch's own: set before anything
is drawn. Every later phase (exact ages, SCF, CEX, the CPS's person items)
runs on both batches alike.

One record is drawn slot by slot:
  1. the batch's own items, every variable the PUF has, each from the model
     under the batch's label given the slots drawn so far: first the type
     (married, dependents, the age range) in a random order per record, then
     full filing status, child credit children and EITC children in that
     order, then every amount in a random order per record. Full filing
     status is joint exactly when married (separate returns have married 0,
     as in the data). Under the CPS label
     the age range is one of the six adult ranges (the CPS has no dependent
     filers). In the filer batch the SCF-only slots are absent meanwhile, as
     on a real PUF record; in the non-filer batch a PUF amount the CPS does not
     measure (interest, John's ruling of 23 October) is zero on every record,
     nothing drawn for it, and every slot the CPS has is present and hidden
     until its turn, except the CPS's two shares, absent
     until married and the share's base are drawn (model.gate_shares);
  2. the computed totals: line-13 capital gains from its four pieces, and
     business income and its two subtotals (Schedules C and F, Schedule E)
     from its eighteen parts. Each part gets a dollar value inside its band
     (one of 32 equally spaced weighted quantile points of the PUF records in
     that band), the signed sum is placed in the total's band on the PUF's
     cut points. Then the computed flag k1_owner: any partnership or
     S corporation part nonzero;
  (On a dependent filer, every survey slot below is zero or none: see structural.)
  3. the exact ages of the primary filer and the spouse (model.EXACT_AGES), in
     a random order per record, under the CPS label and year (settings "cps",
     generation_year) with every slot the CPS lacks left out: the CPS covers
     every tax unit, filers and non-filers, in every year, and its view holds
     the filing flag. The primary filer's age lies inside its age range; the
     spouse's exists exactly when married (cps_structure). The SCF, CEX and CPS
     phases below all see these ages;
  4. the SCF-only slots (the balance sheet, the unrealized gains and
     owns_business) in a random order per record, except that the home
     mortgage, carried as a share of the home's value, comes after the home.
     Each is drawn under the SCF label with the PUF-only slots left out, as
     the SCF would see the household;
  5. the CEX-only slots (the 8 consumption categories) in a random order per
     record, each under the CEX label and year (settings "cex",
     generation_year) with every slot the CEX lacks left out: given married,
     dependents, the age range, the exact ages, wages, gross Social Security,
     self-employment income and the categories drawn so far;
  6. the CPS-only slots (dependents' ages, sex, the spouse's shares of the couple's
     wages and self-employment income, weeks and hours worked, childcare) in a
     random order per record, each under the CPS label and year (settings
     "cps", generation_year) with every slot the CPS lacks left out. They
     follow the record's PUF items (cps_structure): the spouse's items only
     when married, the k-th dependent's age only with k or more dependents.
Each step runs the network only on the slots present in its phase and
computes only the output layers it needs.

Consistency rules (settings.json, "consistency"). Each rule: when any of
its items is nonzero on the record, at least one of its holdings must be
nonzero. The one that must be nonzero is chosen when the first of them comes
up, in proportion to the model's own chance that each is nonzero (before the
offset's factors, below, so the choice does not move with them); the others are drawn
freely. Ruling out zero sets the chance of zero to nothing and rescales the
other bands; the model still decides the size. Also: with no home, the
mortgage share is zero. No rule applies to a dependent filer: it holds no
survey item at all.

The offset. The rules add holders and take none away, so each ruled holding
ends with more holders than the model alone would draw. Generation therefore
runs more than once ("offset_generations"). After each run, for each ruled
holding, one factor on the odds of holding it is found such that

  forced holders + expected holders among the rest = the model's own expected holders

(all weighted; a record's own expected count is the model's chance that the
holding is nonzero at its turn, before rules and factors). The next run
multiplies every record's odds of holding by that factor before the rules
apply; on a forced record the rule overrides it, so in effect only the
untouched records are scaled. The last run is the one kept. Items drawn
later condition on earlier ones, so a run's own figures are reported and
written to consistency.csv: what is left of the excess after the last run.

The filer batch by strata (settings "generation", design "strata"; TARGETS.md S4). The network has learned three
labels (build_table.strata_labels): on the PUF the AGI class (AGI zero or negative; below p90; p90-99; p99-99.9;
p99.9-99.99; p99.99 and over, by weighted rank among the PUF year's returns) and the business flag (Schedule C, a
partnership or S corporation part, or farm income nonzero); on the SCF the net worth class (below p90; p90-99;
p99-99.9; p99.9 and over). The settings give the records drawn in each of the 12 AGI class x business strata
(generation.strata.records), each model drawing an equal share of every stratum. Each record starts with its AGI
class and business flag set; everything else is drawn as above. In the SCF phase the net worth class is drawn first
(the balance sheet given it), each class's chance multiplied by its boost (nw_boost) and the draw corrected by p / q,
the model's chance over the boosted one. A record's weight is the model's own chance of its stratum (its
first-step probabilities under the PUF label at the PUF year: the AGI class with nothing known, then the business
flag given the class) over the stratum's share of the sample, times p / q. The labels are checked, not trusted
(label_check): the record's AGI is computed from its drawn parts and its net worth from its drawn balance sheet, in
dollars as dollars.py draws them from the PUF year's maps, and put on the labels' own cut points; the business flag
from its parts' bands. Every record is kept and the check is only reported (settings generation.strata,
drop_mismatches false, since 25 October). With drop_mismatches true, kept for comparison, a record whose computed
AGI class, business flag or net worth class differs from its label is dropped; the share dropped in each stratum is
written to strata_check.csv and folded into the weights within each net worth class of the stratum: the kept
records of a class carry the sum of p / q of all its draws. Each model's filer weights are then scaled to sum to
its share of the batch's records. Design "unstratified" draws the labels
like any item, first, with no boost and every weight equal (a check on the strata: same model, no design).

The non-filer batch, under every design, is one part with no oversampling (stratifier "nonfiler"), every weight 1.
Its number of records (settings "generation", nonfiler_records; null: in proportion) is the filer batch's records
times the CPS's expected non-filers in the target year (the sum of weight x (1 - filer_p)) over SOI's returns for
that year, so the stack's weights carry the two batches in their real proportion (the tilt sets their levels).

Design "top_parts", from before S4, kept for comparison (with the AGI thinning below, also before S4): the filer batch is a stack of four parts of 100,000 records. Each part oversamples
the top 1% of one variable: that variable is drawn first, from the model
with nothing else known, with the bands covering the top 1% of its positive
weight drawn ten times as often (for an SCF-only slot: drawn in its usual
place, from the boosted conditional); each record's weight p / q undoes the
boost exactly. Each part's weights are scaled to sum to 100,000.

Several trained models pooled (TARGETS.md section 8; settings "generation",
models). Every stratum (or part) is drawn in equal shares from each model: with two
models, each draws half the records of each and half the non-filer batch, so the
stack has as many records as from one model. Each model's
records in a part carry an equal share of the part's weight, so in each batch
(filers, non-filers) every model carries the same share of the weight. Each
model has its own draws: its seed_shift is added to every generation seed
(the parts' and the non-filers'), so its draws are reproducible and differ
from the other models'. Each record says which model drew it (column model,
the network's training seed). Record ids are unique across models: model k
in the list (counting from 0) has the ids from k x 10,000,000. The offset
below is found for each model on its own records, since it brings each
model's holders back to that model's own expected holders.

Oversampling by rough AGI (settings "generation", agi_oversampling; design "top_parts" only, off since S4;
TARGETS.md, "The method stays reweighting"). In each filer part the batch's own items
(steps 1 and 2) are drawn for a pilot of pilot_factor times the part's
records, exactly as above. Each pilot record gets a rough AGI: the sum, with
AGI's signs (dollars.py), of the average target-year dollars of the dollar
map's records in each of its AGI parts' bands (wages, interest, dividends,
the four capital gains pieces, the drawn remainder of other AGI, the
eighteen business parts and the other items folded into AGI). Every pilot
record at or above the line is kept; among the rest a simple random sample,
without replacement, fills the part to its usual number of records. Only the
kept records are finished (steps 3 to 6). Given the pilot, a record's chance
of being kept is known exactly: 1 at or above the line, else the sample's
size over the pilot records below the line. Its weight is divided by that
chance, so the weights undo the thinning exactly, as p / q undoes the boost.
The pilot draws are today's draws, so the distribution the stack represents
does not change; only where its records lie does. The non-filer batch is
not thinned.

Writes common.GEN/stack.parquet (one row per record: the band of every slot,
weight, stratifier, p_band, q_band, model, p_keep (its chance of being kept
from the pilot, 1 when not thinned), agi_proxy (its rough AGI), record_id)
and, under the strata designs, the labels and the label check: agi_class, business, nw_class, agi_check and nw_check
(PUF-year dollars), their classes and label_ok), common.GEN/strata_check.csv (design "strata": per model and stratum,
the model's chance of it, records drawn and kept, the share dropped and why) and common.GEN/consistency.csv (per run, model and ruled holding: the
model's own expected holders, the holders with the rules, the factor used
and the next one).
"""
import json
import os

import numpy as np
import pandas as pd
import torch

import common
import model as M
import targets as TG

G = common.SETTINGS['generation']
C = common.SETTINGS['consistency']
NC = len(M.CONTEXT)
P = M.PREDICTED
COMPUTED = M.COMPUTED                     # {total: {part: sign}}
FLAGS = M.COMPUTED_FLAGS                  # {flag: parts}
PUF_SLOTS = [v for v in P if 'puf' in M.PRESENT_ON[v] and v not in COMPUTED and v not in FLAGS]
SCF_SLOTS = M.SCF_SLOTS
CEX_SLOTS = M.CEX_SLOTS
CEX_YEAR = common.SETTINGS['cex']['generation_year']
CPS_SLOTS = M.CPS_SLOTS
CPS_YEAR = common.SETTINGS['cps']['generation_year']
CPS_STRUCTURE = M.META['cps_structure']
AGE_SLOTS = M.EXACT_AGES                   # drawn after the PUF items, under the CPS label (see the top)
SPOUSE = torch.tensor([P.index(v) for v in CPS_STRUCTURE['spouse']])
DEP_AGES = [P.index(v) for v in CPS_STRUCTURE['dependent_ages']]          # the k-th: present when n_dep >= k
PRIMARY_AGE = P.index(CPS_STRUCTURE['primary_age'])
CPS_CATEGORICAL = torch.tensor([P.index(v) for v in M.PERSON_CATEGORIES])
# The CPS's shares (the spouse's share of the couple's wages, of its self-employment income) are recorded only on
# married couples with some of the item; elsewhere the slot is absent on the training rows. Absence is itself seen by
# the network (a present slot, even hidden, says "a couple with some of this"), so generation leaves the slot absent
# on the same records, and gives them a share of zero once the CPS phase is over.
CPS_SHARES = {slot: base for slot, (_, base) in M.RATIOS.items() if slot in CPS_SLOTS}
# the primary filer's age range of each age value (value 0 is none, value a + 1 is age a)
RANGE_OF_AGE_VALUE = torch.tensor([-1] + [int(r) for r in np.searchsorted([26, 35, 45, 55, 65],
                                                                          np.arange(M.PERSON_CATEGORIES[CPS_STRUCTURE['primary_age']] - 1),
                                                                          side='right')])
REP_SLOTS = torch.tensor([P.index(v) for v in M.REPORTED])
RULES = C['rules']
for _r in RULES:
    assert all(M.OWNED_BY.get(v, 'puf') != 'scf' for v in _r['when_any_nonzero']), _r['name']
    assert all(v in SCF_SLOTS for v in _r['then_at_least_one_nonzero']), _r['name']
RULE_ITEMS = [torch.tensor([P.index(v) for v in r['when_any_nonzero']]) for r in RULES]
RULE_HOLDINGS = [torch.tensor([P.index(v) for v in r['then_at_least_one_nonzero']]) for r in RULES]
RULED = sorted({v for r in RULES for v in r['then_at_least_one_nonzero']}, key=P.index)   # the offset's holdings
DEPENDENT_RANGES = torch.tensor(common.DEPENDENT_RANGES)
SURVEY_ONLY = torch.tensor([P.index(v) for v in AGE_SLOTS + SCF_SLOTS + CEX_SLOTS + CPS_SLOTS])
POINTS = 32
TYPE_SLOTS = M.TYPE_SLOTS                  # drawn first, in a random order per record
STATUS_SLOTS = list(M.TAX_STATUS)          # then these, in this order (mars, n_ctc, n_eitc)
MARS, MARRIED, AGE_RANGE = P.index('mars'), P.index('married'), P.index('age_range')
# The trained models pooled (see the top): each a run under OUT/model and the shift added to its generation seeds.
# null: the run's own model (common.MODEL) alone, with today's seeds.
MODELS = G.get('models', [{'run': 'seed20261016', 'seed_shift': 0}, {'run': 'seed20261017', 'seed_shift': 1000}])
# Oversampling by rough AGI (see the top): pilot_factor pilot records per kept record, and the line in target-year
# dollars at and above which every pilot record is kept. null: no oversampling.
THIN = G.get('agi_oversampling', {'pilot_factor': 12, 'line': 1e6})
ID_BLOCK = 10_000_000                       # model k's record ids start at k x ID_BLOCK
# The filer batch's design (settings "generation", design; see the top): 'strata' (TARGETS.md S4), 'top_parts' (the
# four top-1% parts of before S4, with the AGI thinning if agi_oversampling is set; needs the models trained before
# S4 and settings strata_labels null) or 'unstratified' (nothing preset, no boost: a check on the strata).
# A settings file from before S4 has no design and its four parts under "strata".
DESIGN = G.get('design', 'top_parts')
assert DESIGN in ('strata', 'top_parts', 'unstratified'), DESIGN
TOP_PARTS = G.get('top_parts', G.get('strata') if DESIGN == 'top_parts' else None)
assert DESIGN == 'top_parts' or M.STRATA, 'the strata designs need the table with the strata labels (strata_labels)'
assert DESIGN != 'strata' or THIN is None, 'the strata design replaces the AGI thinning: agi_oversampling null'


def filer_records():
    """The filer batch's records over all models: the strata's, the unstratified draw's, or the four parts'."""
    if DESIGN == 'strata':
        return int(sum(n for _, _, n in strata_cells()))
    if DESIGN == 'unstratified':
        return int(G['unstratified_records'])
    return G['records_per_file'] * len(TOP_PARTS)


def cps_expected_nonfilers(year):
    """The CPS's expected non-filers in year: the sum over its units of weight x (1 - filer_p)."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight', 'filer_p'])
    t = t[(t.source == 'cps') & (t.year == year)]
    assert len(t), f'no CPS units for {year}: the non-filer batch is drawn from the CPS at the target year'
    return float((t['weight'] * (1 - t['filer_p'])).sum())


# ---- computed totals ---------------------------------------------------------
_points = {}


def part_points(total, device):
    """[bands, 32] dollar points per band of each part of a total, and the total's cut points."""
    if total in _points:
        return _points[total]
    parts = list(COMPUTED[total])
    t = pd.read_parquet(common.TABLE, columns=['source', 'weight'] + parts + [v + '_band' for v in parts])
    t = t[t.source == 'puf']
    w = t['weight'].to_numpy(float)
    out = {}
    for v in parts:
        x, b = t[v].to_numpy(float), t[v + '_band'].to_numpy()
        pts = np.zeros((M.N_CHOICES[v], POINTS))
        for k in range(M.N_CHOICES[v]):
            sel = b == k
            if sel.any():
                o = np.argsort(x[sel], kind='stable')
                xs, F = x[sel][o], np.cumsum(w[sel][o]) / w[sel].sum()
                pts[k] = xs[np.minimum(np.searchsorted(F, (np.arange(POINTS) + 0.5) / POINTS), len(xs) - 1)]
        out[v] = torch.tensor(pts, dtype=torch.float64, device=device)
    e = common.edges()[f'puf/{common.PUF_YEAR}/{total}']
    out['neg'] = torch.tensor(e['negative']['cuts'], dtype=torch.float64, device=device)
    out['pos'] = torch.tensor(e['positive']['cuts'], dtype=torch.float64, device=device)
    _points[total] = out
    return out


def computed_band(values, gen, total):
    """The band of a computed total from its parts' bands."""
    pts = part_points(total, values.device)
    s = torch.zeros(len(values), dtype=torch.float64, device=values.device)
    for v, sign in COMPUTED[total].items():
        b = values[:, NC + P.index(v)]
        assert (b >= 0).all(), f'{v} not drawn before {total}'
        k = torch.randint(0, POINTS, (len(values),), generator=gen, device=values.device)
        s += float(sign) * pts[v][b, k]
    band = torch.zeros(len(values), dtype=torch.long, device=values.device)
    neg, pos = s < 0, s > 0
    band[neg] = 1 + torch.searchsorted(pts['neg'], s[neg].contiguous(), right=False)
    band[pos] = M.BAND_LAYOUT[total]['first_positive'] + torch.searchsorted(pts['pos'], s[pos].contiguous(),
                                                                             right=False)
    return band


# ---- rough AGI ---------------------------------------------------------------------
# AGI's parts as dollars.py adds them up: wages, interest, dividends, the Schedule D net (kg_st + kg_lt + kg_other),
# and other AGI, which is the drawn remainder plus kg_dist plus the items folded back in (business income as the
# signed sum of its eighteen parts). Each is a slot drawn in the batch's own phase.
FOLD = common.meta()['agi_signs']
AGI_PROXY = {v: 1 for v in ['wages', 'interest', 'dividends', 'kg_st', 'kg_lt', 'kg_other', 'kg_dist', 'other_agi']}
for _item, _sign in FOLD.items():
    for _part, _s in (COMPUTED[_item].items() if _item in COMPUTED else [(_item, 1)]):
        AGI_PROXY[_part] = AGI_PROXY.get(_part, 0) + _sign * _s
assert all(v in PUF_SLOTS for v in AGI_PROXY), [v for v in AGI_PROXY if v not in PUF_SLOTS]
_band_dollars = {}


def band_dollars(device):
    """{AGI part: [bands] the average dollars of the target-year dollar map's records in each band} (dollars.py's map,
    each record by its weight); NaN for a band the map lacks."""
    if not _band_dollars:
        m = pd.read_parquet(f'{common.MAPS}/map_puf_{common.TARGET_YEAR}.parquet',
                            columns=['variable', 'band', 'value', 'weight'])
        m = m[m.variable.isin(list(AGI_PROXY))]
        for v, g in m.groupby('variable'):
            tab = np.full(M.N_CHOICES[v], np.nan)
            avg = (g['value'] * g['weight']).groupby(g['band']).sum() / g['weight'].groupby(g['band']).sum()
            tab[avg.index.to_numpy(int)] = avg.to_numpy(float)
            _band_dollars[v] = torch.tensor(tab, dtype=torch.float64, device=device)
        assert set(_band_dollars) == set(AGI_PROXY), set(AGI_PROXY) - set(_band_dollars)
    return _band_dollars


def rough_agi(values):
    """Each record's rough AGI in target-year dollars: its AGI parts at their bands' average dollars (see the top)."""
    tabs = band_dollars(values.device)
    s = torch.zeros(len(values), dtype=torch.float64, device=values.device)
    for v, sign in AGI_PROXY.items():
        b = values[:, NC + P.index(v)]
        assert (b >= 0).all(), f'{v} not drawn before the rough AGI'
        s += float(sign) * tabs[v][b]
    assert not torch.isnan(s).any(), 'a record in a band the dollar map lacks'
    return s


def thin_pilot(proxy, n, line, seed):
    """Which pilot records are kept, n in all: every one whose rough AGI is at or above the line, and a simple random
    sample without replacement of the rest; and each kept record's chance of being kept given the pilot."""
    top, rest = np.flatnonzero(proxy >= line), np.flatnonzero(proxy < line)
    m = n - len(top)
    assert 0 < m <= len(rest), (f'{len(top):,} pilot records at or above the line, room for {n:,}: '
                                f'lower the pilot factor or raise the line')
    kept = np.sort(np.r_[top, np.random.default_rng([seed, 1]).choice(rest, m, replace=False)])
    p_keep = np.where(proxy[kept] >= line, 1.0, m / len(rest))
    common.say(f'    pilot {len(proxy):,}: {len(top):,} at or above ${line:,.0f} of rough AGI, all kept; '
               f'{m:,} of the other {len(rest):,} kept (chance {m / len(rest):.4f})')
    return kept, p_keep


# ---- rules on a draw -------------------------------------------------------------
def scf_order(size, gen):
    """A random order of the SCF-only slots per record, each ratio slot moved after its base; the net worth class
    (S4) first, so the balance sheet is drawn given it."""
    labels = torch.tensor([P.index(v) for v in M.SCF_LABELS], dtype=torch.long)
    scf = torch.tensor([P.index(v) for v in SCF_SLOTS if v not in M.SCF_LABELS])
    perm = torch.cat([labels.expand(size, -1), scf[torch.rand(size, len(scf), generator=gen).argsort(1)]], 1)
    rows = torch.arange(size)
    for slot, (_, base) in M.RATIOS.items():
        if slot not in SCF_SLOTS:
            continue                                # the CPS's ratios: their base is a PUF slot, drawn earlier
        i, ib = P.index(slot), P.index(base)
        pos, pos_b = (perm == i).int().argmax(1), (perm == ib).int().argmax(1)
        swap = pos < pos_b
        perm[rows[swap], pos[swap]], perm[rows[swap], pos_b[swap]] = ib, i
    return perm


def dependent(values):
    """Records whose primary filer is in a dependent-filer age range."""
    return torch.isin(values[:, NC + P.index('age_range')], DEPENDENT_RANGES.to(values.device))


def only(probs, rows, allowed=None):
    """On rows, the distribution cut to the allowed values ([rows, choices] booleans) and rescaled; allowed None:
    value 0 for certain. If the model gives every allowed value no chance at all, they are taken as equally likely."""
    if allowed is None:
        probs[rows] = 0.0
        probs[rows, 0] = 1.0
        return probs
    q = probs[rows] * allowed
    total = q.sum(1, keepdim=True)
    q = torch.where(total > 0, q / total.clamp_min(1e-30), allowed / allowed.sum(1, keepdim=True).clamp_min(1))
    probs[rows] = q
    return probs


def cps_structure(values, slot, probs):
    """The CPS's person items follow the record's PUF items: the spouse's items are none or zero unless married,
    and a married couple's spouse has an age and a sex; the k-th dependent has an age exactly when there are at
    least k dependents; the primary filer's exact age lies in its age range."""
    married = values[:, NC + P.index('married')] == 1
    n_dep = values[:, NC + P.index('n_dep')]
    on_spouse = torch.isin(slot, SPOUSE.to(slot.device))
    categorical = torch.isin(slot, CPS_CATEGORICAL.to(slot.device))
    not_none = torch.ones(probs.shape[1], dtype=torch.bool, device=probs.device)
    not_none[0] = False
    probs = only(probs, on_spouse & ~married)
    sel = on_spouse & married & categorical
    probs = only(probs, sel, not_none.expand(int(sel.sum()), -1).float())
    for k, j in enumerate(DEP_AGES, start=1):
        probs = only(probs, (slot == j) & (n_dep < k))
        sel = (slot == j) & (n_dep >= k)
        probs = only(probs, sel, not_none.expand(int(sel.sum()), -1).float())
    sel = slot == PRIMARY_AGE
    if sel.any():
        rng = RANGE_OF_AGE_VALUE.to(probs.device)
        allowed = torch.zeros(int(sel.sum()), probs.shape[1], device=probs.device)
        allowed[:, :len(rng)] = (rng[None, :] == values[sel, NC + P.index('age_range')][:, None]).float()
        probs = only(probs, sel, allowed)
    return probs


def structural(values, slot, probs):
    """A ratio slot whose base is zero is zero (no home: no mortgage share; no wages: no spouse's share of them).
    The CPS's person items follow the PUF items (cps_structure). A dependent filer holds no survey-only item
    (balance sheet, unrealized gains, business ownership, consumption, the CPS's person items and childcare): the
    surveys count it in the household that claims it, so on its own return every one is zero or none, and its
    wages and self-employment income are all its own."""
    for slot_name, (_, base) in M.RATIOS.items():
        no_base = (slot == P.index(slot_name)) & (values[:, NC + P.index(base)] == 0)
        probs[no_base] = 0.0
        probs[no_base, 0] = 1.0
    probs = cps_structure(values, slot, probs)
    dep = dependent(values) & torch.isin(slot, SURVEY_ONLY.to(values.device))
    probs[dep] = 0.0
    probs[dep, 0] = 1.0
    return probs


def scale_odds(probs, slot, factors):
    """On the records drawing a ruled holding, its odds of being nonzero multiplied by its factor."""
    for v, f in factors.items():
        if f != 1.0:
            s = probs.clone()
            s[:, 1:] *= f
            s /= s.sum(-1, keepdim=True)
            probs = torch.where((slot == P.index(v))[:, None], s, probs)
    return probs


def apply_rules(slot, filled, nonzero, probs, triggered, resolved, must, gen):
    """The current slot's distribution under the consistency rules (see the top), and which rows were forced.
    nonzero: every slot's chance of being nonzero, the model's own (before the offset's factors), so which
    holding a rule forces does not depend on the factors."""
    rows = torch.arange(len(slot), device=slot.device)
    for r in range(len(RULES)):
        g = RULE_HOLDINGS[r].to(slot.device)
        first = triggered[:, r] & ~resolved[:, r] & torch.isin(slot, g)
        if not first.any():
            continue
        resolved[:, r] |= first
        weight = (nonzero[:, g] * ~filled[:, g]).double().clamp_min(0) + 1e-12 * (~filled[:, g])
        pick = torch.multinomial(weight[first], 1, generator=gen).squeeze(1)
        must[rows[first], g[pick]] = True
    q = probs.clone()
    q[:, 0] = 0
    ok = must[rows, slot] & (q.sum(1) > 1e-12)
    return torch.where(ok[:, None], q / q.sum(1, keepdim=True).clamp_min(1e-30), probs), ok


# ---- one chunk of records ----------------------------------------------------------
class Chunk:
    """size records being drawn: every slot's band (-1 absent), which slots are drawn, and the rules' and the
    boost's bookkeeping. Each step runs the network only on the slots present in its phase (model.SOURCE_SLOTS:
    the PUF's, then the CPS's view for the exact ages, then the SCF's, the CEX's and the CPS's views) and computes only the output layers it needs."""

    def __init__(self, model, size, order, preset, start, factors, boost, gen, label='puf', year=None):
        device = order.device
        self.model, self.order, self.factors, self.boost, self.gen = model, order, factors, boost, gen
        self.label = label                                           # the batch's label for its own items
        self.rows = torch.arange(size, device=device)
        self.values = torch.full((size, len(M.SLOTS)), -1, dtype=torch.long, device=device)
        self.values[:, 0] = M.SOURCES.index(label)
        self.values[:, 1] = M.YEARS.index(common.PUF_YEAR if year is None else year)
        self.values[self.rows[:, None], NC + order] = 0              # present, not yet drawn
        for v in AGE_SLOTS + SCF_SLOTS + CEX_SLOTS + CPS_SLOTS:
            self.values[:, NC + P.index(v)] = -1                      # absent until their turn
        if label == 'cps':        # the CPS's own slots: present and hidden from the start, as on a CPS training row
            for v in AGE_SLOTS + CPS_SLOTS:
                self.values[:, NC + P.index(v)] = 0
        for tot in list(COMPUTED) + list(FLAGS):
            self.values[:, NC + P.index(tot)] = 0                    # present and hidden until computed
        self.filled = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        for var, bands in preset.items():
            self.values[:, NC + P.index(var)] = bands[start:start + size].to(device)
            self.filled[:, P.index(var)] = True
        self.triggered = torch.zeros(size, len(RULES), dtype=torch.bool, device=device)
        self.resolved = self.triggered.clone()
        self.must = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        self.own_chance = torch.zeros(size, len(RULED), dtype=torch.float64, device=device)
        self.forced = torch.zeros(size, len(RULED), dtype=torch.bool, device=device)
        self.p_drawn = torch.full((size,), float('nan'), dtype=torch.float64, device=device)
        self.q_drawn = self.p_drawn.clone()
        self.ruled = torch.tensor([P.index(v) for v in RULED], device=device)
        self.column = {}                                             # per source: each of SLOTS' column, -1 if absent
        for src, slots in M.SOURCE_SLOTS.items():
            self.column[src] = torch.full((len(M.SLOTS),), -1, dtype=torch.long, device=device)
            self.column[src][list(slots)] = torch.arange(len(slots), device=device)
        self.not_asked = {}
        if label == 'cps':
            self.gate_shares()

    def gate_shares(self):
        """Non-filer batch, own phase: each CPS share present (hidden) once married and its base are drawn and the unit
        is married with some of the base; absent before that, and where not asked (model.gate_shares)."""
        for s, (m, b) in M.SHARE_GATES.items():
            known = self.filled[:, m] & self.filled[:, b]
            asked = known & (self.values[:, NC + m] == 1) & (self.values[:, NC + b] > 0)
            self.values[:, NC + s] = torch.where(asked, 0, -1)

    def own_rules(self, slot, probs):
        """Full filing status joint exactly when married (separate returns are not married, as in the data); under the
        CPS label, an adult age range."""
        sel = slot == MARS
        if sel.any():
            assert self.filled[sel, MARRIED].all(), 'married is drawn before full filing status'
            married = self.values[sel, NC + MARRIED] == 1
            allowed = torch.zeros(int(sel.sum()), probs.shape[1], device=probs.device)
            allowed[married, 2] = 1.0
            allowed[~married, 1] = allowed[~married, 3] = allowed[~married, 4] = 1.0
            probs = only(probs, sel, allowed)
        sel = slot == AGE_RANGE
        if self.label == 'cps' and sel.any():
            allowed = torch.zeros(int(sel.sum()), probs.shape[1], device=probs.device)
            allowed[:, :common.ADULT_RANGES] = 1.0
            probs = only(probs, sel, allowed)
        return probs

    def last_layer(self, values, hidden, src):
        """The network on the source's slots, all of which must be present."""
        slots = M.SOURCE_SLOTS[src]
        may_be_absent = [M.SOURCE_SLOTS[src].index(NC + P.index(v)) for v in CPS_SHARES if src == 'cps']
        present = values[:, M.slot_tensor(slots, values.device)] >= 0
        present[:, may_be_absent] = True
        assert present.all(), f'a {src} slot is absent'
        return self.model.last_layer(*M.pack(values, hidden, slots), slots)

    def take(self, probs, slot):
        """Draw slot from probs (boosted at the boost's variable), and record it."""
        draw_from, boosted = probs, None
        if self.boost is not None:
            var, multipliers = self.boost
            boosted = slot == P.index(var)
            q = probs * torch.as_tensor(multipliers, dtype=probs.dtype, device=probs.device)[None, :]
            draw_from = torch.where(boosted[:, None], q / q.sum(1, keepdim=True), probs)
        draw = torch.multinomial(draw_from, 1, generator=self.gen).squeeze(1)
        if boosted is not None and boosted.any():
            self.p_drawn[boosted] = probs[self.rows, draw][boosted].double()
            self.q_drawn[boosted] = draw_from[self.rows, draw][boosted].double()
        self.values[self.rows, NC + slot] = draw
        self.filled[self.rows, slot] = True

    def puf_step(self, slot):
        """One of the batch's own items per record (a PUF variable), under the batch's label, given the slots drawn
        so far."""
        hidden = (self.values[:, NC:] >= 0) & ~self.filled
        h = self.last_layer(self.values, hidden, self.label)
        probs = self.model.slot_probs(h[self.rows, self.column[self.label][NC + slot]], slot)
        self.take(self.own_rules(slot, probs), slot)
        if self.label == 'cps':
            self.gate_shares()

    def compute_totals(self):
        """The computed totals and flags from their parts; which rules each record triggers (none for a
        dependent filer, which holds no survey item); the survey slots made present. Returns the rule counts."""
        v = self.values
        for tot in COMPUTED:
            v[:, NC + P.index(tot)] = computed_band(v, self.gen, tot)
            self.filled[:, P.index(tot)] = True
        for flag, flag_parts in FLAGS.items():
            v[:, NC + P.index(flag)] = torch.stack([v[:, NC + P.index(p)] != 0 for p in flag_parts], 1).any(1).long()
            self.filled[:, P.index(flag)] = True
        return self.after_totals()

    def after_totals(self):
        """compute_totals' bookkeeping, which depends only on the values drawn so far (so it can be run again, on the
        records kept from a pilot): the rules each record triggers, the survey slots made present, the CPS shares'
        records with no question. Returns the rule counts."""
        v = self.values
        dep = dependent(v)
        for r in range(len(RULES)):
            self.triggered[:, r] = (v[:, NC + RULE_ITEMS[r].to(v.device)] != 0).any(1) & ~dep
        for s in AGE_SLOTS + SCF_SLOTS + CEX_SLOTS + CPS_SLOTS:
            v[:, NC + P.index(s)] = 0                                # each survey's view leaves out the others'
        v[:, NC + REP_SLOTS.to(v.device)] = 0                        # reported slots: present, never drawn (hidden)
        married = v[:, NC + P.index('married')] == 1
        self.not_asked = {P.index(sh): ~married | (v[:, NC + P.index(base)] == 0) | dep
                          for sh, base in CPS_SHARES.items()}           # the CPS shares' records with no question
        for j, m in self.not_asked.items():
            v[m, NC + j] = -1
        return {r['name']: int(self.triggered[:, k].sum()) for k, r in enumerate(RULES)}

    def survey_step(self, slot, label, year=None):
        """One survey-only slot per record, as the survey label would see the record, under the rules."""
        hidden = (self.values[:, NC:] >= 0) & ~self.filled
        view, view_hidden = M.seen_as(self.values, hidden, label, year)
        h, col = self.last_layer(view, view_hidden, label), self.column[label]
        slot_p = self.model.slot_probs(h[self.rows, col[NC + slot]], slot)
        # each ruled holding's chance of being nonzero, the model's own (no rule applies in the CEX and CPS phases)
        own_nonzero = torch.zeros(len(slot), len(P), device=slot.device)
        for v in (RULED if label == 'scf' else []):
            j = P.index(v)
            own_nonzero[:, j] = 1 - self.model.slot_probs(h[:, col[NC + j]], torch.full_like(slot, j))[:, 0]
        del h
        own_probs = structural(self.values, slot, slot_p.clone())   # the model's own, before rules
        probs, ok = apply_rules(slot, self.filled, own_nonzero,
                                structural(self.values, slot, scale_odds(slot_p.clone(), slot, self.factors)),
                                self.triggered, self.resolved, self.must, self.gen)
        k = (slot[:, None] == self.ruled[None, :])                  # rows whose slot is a ruled holding
        hit = k.any(1)
        col = k.int().argmax(1)
        self.own_chance[self.rows[hit], col[hit]] = 1 - own_probs[hit, 0].double()
        self.forced[self.rows[hit], col[hit]] = ok[hit]
        for j, m in self.not_asked.items():                         # no draw where the share is not asked
            na = m & (slot == j)
            probs[na] = 0.0
            probs[na, 0] = 1.0
        self.take(probs, slot)
        for j, m in self.not_asked.items():
            self.values[m, NC + j] = -1

    def finish(self):
        """The CPS shares not asked of a record: zero (no spouse, or none of the item, or a dependent filer)."""
        for j, m in self.not_asked.items():
            self.values[m, NC + j] = 0


@torch.no_grad()
def generate(model, n, seed, preset=None, boost=None, factors=None, chunk=50_000, label='puf', year=None, filer=1,
             thin=None, cache=None, cache_key=None):
    """n PUF-style records, and how many records each consistency rule applies to. label, year: the batch's label and
    year for its own items (default the PUF at the PUF year); filer: the batch's filing flag, set before anything is
    drawn. preset: {variable: bands, one per record drawn (the pilot's records when thinned)} set before anything is
    drawn. boost: (variable, bands, factor), drawn from q (p with those bands multiplied) at
    its turn; that draw's p and q are returned as p_band, q_band. factors: {ruled holding: factor on its odds of
    being nonzero}. thin: None, or (pilot, line): the batch's own items drawn for pilot records, n of them kept by
    their rough AGI (thin_pilot) and only those finished (see the top). The table carries each record's row in the
    pilot (pilot_row; its own row when not thinned), its chance of being kept (p_keep) and its rough AGI (agi_proxy).
    cache, cache_key: a dict kept across the offset's runs, and this part's key in it. The pilot draws only the batch's
    own items, which the offset's factors do not touch, so every run draws the same pilot: the first run stores the
    kept records and the random generator's state after the pilot, and later runs start from them, with the same
    result in a fraction of the time.
    For each ruled holding the table also carries the model's own chance it is nonzero
    (_own_<v>) and whether a rule forced it (_forced_<v>)."""
    preset, factors = dict(preset or {}), factors or {}
    n_draw = n if thin is None else int(thin[0])
    assert thin is None or label == 'puf', 'only the filer batch is thinned'
    preset['filer'] = torch.full((n_draw,), filer, dtype=torch.long)
    if label != 'puf':
        # a PUF amount the batch's source does not measure (John, 23 October: interest on the CPS) is zero on every
        # record of the batch, nothing drawn for it; a category must be there
        lacking = [v for v in PUF_SLOTS if label not in M.PRESENT_ON[v] and v not in preset and v not in M.PUF_LABELS]
        assert all(v in M.AMOUNTS for v in lacking), f'the {label} label lacks PUF categories: {lacking}'
        for v in lacking:
            preset[v] = torch.zeros(n, dtype=torch.long)
        if lacking:
            common.say(f'  {label} batch: zero on every record, not measured by the {label}: {lacking}')
    device = next(model.parameters()).device
    cpu_gen = torch.Generator().manual_seed(seed)                  # the orders
    gpu_gen = torch.Generator(device=device).manual_seed(seed)     # the draws
    # the strata labels (S4) first, in their fixed order (the AGI class, then the business flag), unless preset; on the
    # PUF label only (the other sources do not have them: absent on those records)
    labels = torch.tensor([P.index(v) for v in M.PUF_LABELS if v not in preset and label == 'puf'], dtype=torch.long)
    first = torch.tensor([P.index(v) for v in TYPE_SLOTS if v not in preset], dtype=torch.long)
    then = torch.tensor([P.index(v) for v in STATUS_SLOTS if v not in preset], dtype=torch.long)
    rest = torch.tensor([P.index(v) for v in PUF_SLOTS if v not in preset and v not in TYPE_SLOTS
                         and v not in STATUS_SLOTS and v not in M.PUF_LABELS], dtype=torch.long)
    own = torch.cat([labels, first, then, rest])
    cex = torch.tensor([P.index(v) for v in CEX_SLOTS])
    cps = torch.tensor([P.index(v) for v in CPS_SLOTS])
    age = torch.tensor([P.index(v) for v in AGE_SLOTS])
    n_own, n_age, n_scf, n_cex = len(own), len(AGE_SLOTS), len(SCF_SLOTS), len(CEX_SLOTS)
    rule_counts = {r['name']: 0 for r in RULES}

    def orders(size):
        return torch.cat([labels.expand(size, -1),
                          first[torch.rand(size, len(first), generator=cpu_gen).argsort(1)],
                          then.expand(size, -1),
                          rest[torch.rand(size, len(rest), generator=cpu_gen).argsort(1)],
                          age[torch.rand(size, len(age), generator=cpu_gen).argsort(1)],
                          scf_order(size, cpu_gen),
                          cex[torch.rand(size, len(cex), generator=cpu_gen).argsort(1)],
                          cps[torch.rand(size, len(cps), generator=cpu_gen).argsort(1)]], 1).to(device)

    def finish(d, order):
        """Steps 3 to 6 on a chunk whose own items and totals are drawn."""
        k = n_own
        for step in range(k, k + n_age):
            d.survey_step(order[:, step], 'cps', CPS_YEAR)
        k += n_age
        for step in range(k, k + n_scf):
            d.survey_step(order[:, step], 'scf', common.PUF_YEAR)
        k += n_scf
        for step in range(k, k + n_cex):
            d.survey_step(order[:, step], 'cex', CEX_YEAR)
        k += n_cex
        for step in range(k, order.shape[1]):
            d.survey_step(order[:, step], 'cps', CPS_YEAR)
        d.finish()
        return d.values.cpu(), d.p_drawn.cpu(), d.q_drawn.cpu(), d.own_chance.cpu(), d.forced.cpu()

    parts, proxy = [], []
    if thin is None:                  # every record drawn is kept: steps 1 to 6 chunk by chunk
        for start in range(0, n, chunk):
            size = min(chunk, n - start)
            order = orders(size)
            d = Chunk(model, size, order, preset, start, factors, boost, gpu_gen, label, year)
            for step in range(n_own):
                d.puf_step(order[:, step])
            for name, count in d.compute_totals().items():
                rule_counts[name] += count
            proxy.append(rough_agi(d.values).cpu())
            parts.append(finish(d, order))
        kept, p_keep, proxy = np.arange(n), np.ones(n), torch.cat(proxy).numpy()
    else:                             # the pilot: steps 1 and 2; then the kept records finished
        pilot, line = int(thin[0]), float(thin[1])
        if cache is not None and cache_key in cache:
            values, filled, ords, proxy, kept, p_keep, state = cache[cache_key]
            gpu_gen.set_state(state)
        else:
            values, filled, ords = [], [], []
            for start in range(0, pilot, chunk):
                size = min(chunk, pilot - start)
                order = orders(size)
                d = Chunk(model, size, order, preset, start, factors, boost, gpu_gen, label, year)
                for step in range(n_own):
                    d.puf_step(order[:, step])
                d.compute_totals()
                proxy.append(rough_agi(d.values).cpu())
                values.append(d.values.cpu()), filled.append(d.filled.cpu()), ords.append(order.cpu())
                del d
            proxy = torch.cat(proxy).numpy()
            kept, p_keep = thin_pilot(proxy, n, line, seed)
            rows = torch.as_tensor(kept)                            # only the kept records are needed from here
            values, filled, ords = torch.cat(values)[rows], torch.cat(filled)[rows], torch.cat(ords)[rows]
            proxy = proxy[kept]
            if cache is not None:
                cache[cache_key] = (values, filled, ords, proxy, kept, p_keep, gpu_gen.get_state())
        for start in range(0, n, chunk):
            rows = slice(start, start + chunk)
            order = ords[rows].to(device)
            d = Chunk(model, len(order), order, {}, 0, factors, boost, gpu_gen, label, year)
            d.values, d.filled = values[rows].clone().to(device), filled[rows].clone().to(device)   # the pilot's draws
            for name, count in d.after_totals().items():
                rule_counts[name] += count
            parts.append(finish(d, order))
    table = to_table(torch.cat([p[0] for p in parts]))
    if boost is not None:
        table['p_band'] = torch.cat([p[1] for p in parts]).numpy()
        table['q_band'] = torch.cat([p[2] for p in parts]).numpy()
    oc, fc = torch.cat([p[3] for p in parts]).numpy(), torch.cat([p[4] for p in parts]).numpy()
    for i, v in enumerate(RULED):
        table[f'_own_{v}'], table[f'_forced_{v}'] = oc[:, i], fc[:, i]
    table['pilot_row'], table['p_keep'], table['agi_proxy'] = kept, p_keep, proxy
    return table, rule_counts


def to_table(values):
    v = values.numpy()
    out = pd.DataFrame({'source': [M.SOURCES[i] for i in v[:, 0]], 'year': [M.YEARS[i] for i in v[:, 1]]})
    for j, var in enumerate(P):
        out[M.column(var)] = v[:, NC + j]
    out['weight'] = 1.0
    return out


# ---- the stack -----------------------------------------------------------------
@torch.no_grad()
def first_step(model, var, shown=None):
    """var's distribution for PUF records at the PUF year with every PUF slot hidden but the filing flag (1, a filer)
    and the slots in shown ({slot: one value per record}), and no SCF-only slot: [records, values of var]."""
    shown = shown or {}
    n = len(next(iter(shown.values()))) if shown else 1
    device = next(model.parameters()).device
    values = torch.full((n, len(M.SLOTS)), -1, dtype=torch.long, device=device)
    values[:, 0], values[:, 1] = M.SOURCES.index('puf'), M.YEARS.index(common.PUF_YEAR)
    for j, v in enumerate(P):
        if 'puf' in M.PRESENT_ON[v]:
            values[:, NC + j] = 0
    hidden = values[:, NC:] >= 0
    values[:, NC + P.index('filer')], hidden[:, P.index('filer')] = 1, False    # a filer
    for v, x in shown.items():
        values[:, NC + P.index(v)] = torch.as_tensor(x, dtype=torch.long, device=device)
        hidden[:, P.index(v)] = False
    lp = M.log_probs_as(model, values, hidden, 'puf')
    return lp(P.index(var)).exp().double().cpu().numpy()[:, :M.N_CHOICES[var]]


def nothing_known(model, var):
    """var's distribution for a PUF record with every PUF slot hidden and no SCF-only slot."""
    return first_step(model, var)[0]


# ---- the strata (TARGETS.md S4) ------------------------------------------------------
LABELS = M.META.get('strata_labels', {})


def strata_cells():
    """The 12 AGI class x business strata and their records, from settings generation.strata.records:
    [(AGI class, business flag, records)], in class order."""
    rec = G['strata']['records']
    return [(a, b, int(rec[an][bn])) for a, an in enumerate(LABELS['agi_class']['labels'])
            for b, bn in enumerate(LABELS['business']['labels'])]


def stratum_chances(model):
    """The model's own chance of each AGI class x business stratum, from its first-step probabilities under the PUF
    label at the PUF year, in the order the strata are drawn: the AGI class with nothing known, then the business
    flag given the class. [classes, 2]."""
    p_class = first_step(model, 'agi_class')[0]
    p_business = first_step(model, 'business', {'agi_class': np.arange(M.STRATA['agi_class'])})
    return p_class[:, None] * p_business


def nw_multipliers():
    """The boost on the net worth class (settings generation.strata, nw_boost: one multiplier per class), padded with
    ones to the network's widest output."""
    m = np.ones(M.MAX_CHOICES)
    f = G['strata']['nw_boost']
    assert len(f) == M.STRATA['nw_class'], 'one net worth boost per class'
    m[:len(f)] = f
    return m


_check_maps = {}


def label_check(part):
    """Each record's AGI class, business flag and net worth class computed from its drawn parts, against its labels.
    Dollars as dollars.py draws them, from the PUF year's maps (the PUF's and the SCF's balance sheet in the wave of
    the PUF year), so the classes are on the labels' own cut points (build_table.strata_labels): AGI from its parts
    (dollars.agi), net worth from the 20 balance-sheet items. The business flag is any of its parts' bands nonzero
    (band 0 is exactly zero). Returns the part with the columns agi_check and nw_check (PUF-year dollars),
    agi_class_check, business_check, nw_class_check, and label_ok (all three agree with the labels)."""
    import dollars as D
    if not _check_maps:
        for src in ['puf', 'scf']:
            m = pd.read_parquet(f'{common.MAPS}/map_{src}_{common.PUF_YEAR}.parquet')
            _check_maps.update({v: g for v, g in m.groupby('variable')})
    need = set(D.AGI_PARTS) | set(D.KG_PIECES) | set(D.FOLD) | set(D.META['computed_totals']['business_income']) \
        | set(D.META['scf_amounts'])
    bands = part[[f'{v}_band' for v in D.ORDER if v in need]]
    assert (bands >= 0).all().all(), 'a part not drawn on a record'
    d = D.draw_dollars(bands, _check_maps)
    agi, nw = D.agi(d), d['net_worth'].to_numpy(float)
    out = part.copy()
    out['agi_check'], out['nw_check'] = agi, nw
    out['agi_class_check'] = np.where(agi <= 0, 0, 1 + np.searchsorted(LABELS['agi_class']['cuts'], agi, side='right'))
    out['business_check'] = (part[[f'{v}_band' for v in LABELS['business']['parts']]] != 0).any(axis=1).astype(int)
    out['nw_class_check'] = np.searchsorted(LABELS['nw_class']['cuts'][str(common.PUF_YEAR)], nw, side='right')
    out['label_ok'] = ((out['agi_class_check'] == part['agi_class']) & (out['business_check'] == part['business'])
                       & (out['nw_class_check'] == part['nw_class']))
    return out


def stratified_part(model, mod, k, n_models, fac):
    """Model k's (of n_models) filer batch under the strata design (see the top): every stratum's records (an equal
    share of each stratum's in settings) with its labels preset, the rest drawn, the net worth class boosted at the SCF
    step; then the label check, the records whose computed classes differ from their labels dropped, and the weights.
    Returns the kept records, the check's table and the rule counts."""
    cells = strata_cells()
    n_cell = [equal_shares(n, n_models)[k] for _, _, n in cells]
    agi_lab = torch.tensor(np.repeat([a for a, _, _ in cells], n_cell))
    bus_lab = torch.tensor(np.repeat([b for _, b, _ in cells], n_cell))
    n_k = int(sum(n_cell))
    part, counts = generate(model, n_k, G['strata']['seed'] + mod['seed_shift'],
                            preset={'agi_class': agi_lab, 'business': bus_lab},
                            boost=('nw_class', nw_multipliers()), factors=fac)
    part = label_check(part)
    p_s = stratum_chances(model)                                     # the model's own chance of each stratum
    pq = (part['p_band'] / part['q_band']).to_numpy(float)          # the net worth boost's correction
    # settings generation.strata, drop_mismatches: false keeps every record (the check is then only reported), so the
    # weights are the model's own chance of the stratum over its share times p / q, nothing folded in
    ok = part['label_ok'].to_numpy(bool) if G['strata'].get('drop_mismatches', True) else np.ones(len(part), bool)
    w = np.zeros(len(part))
    rows = []
    for (a, b, _), n in zip(cells, n_cell):
        s = ((part['agi_class'] == a) & (part['business'] == b)).to_numpy()
        row = {'model': mod['seed'], 'AGI class': LABELS['agi_class']['labels'][a],
               'business': LABELS['business']['labels'][b], "model's chance of the stratum": p_s[a, b],
               'PUF share of returns': LABELS['agi_class']['strata_shares'].get(f'{a}/{b}', 0.0),
               'records drawn': int(s.sum()), 'records kept': int((s & ok).sum()),
               'dropped (%)': 100 * (1 - (s & ok).sum() / max(1, s.sum())),
               'dropped, AGI class differs (%)': 100 * (s & (part['agi_class_check'] != part['agi_class'])).sum() / max(1, s.sum()),
               'dropped, business differs (%)': 100 * (s & (part['business_check'] != part['business'])).sum() / max(1, s.sum()),
               'dropped, net worth class differs (%)': 100 * (s & (part['nw_class_check'] != part['nw_class'])).sum() / max(1, s.sum()),
               'dropped, weighted by p/q (%)': 100 * (1 - pq[s & ok].sum() / pq[s].sum())}
        # the drop folded into the weight within each net worth class of the stratum: the kept records of a class carry
        # what all its draws carried (sum of p / q), so the boosted classes keep their mass
        lost = 0.0
        for c in range(M.STRATA['nw_class']):
            sc = s & (part['nw_class'] == c).to_numpy()
            if not sc.any():
                continue
            if not (sc & ok).any():
                lost += pq[sc].sum()
                continue
            w[sc & ok] = pq[sc & ok] * pq[sc].sum() / pq[sc & ok].sum()
        row['mass lost, a net worth class with no record kept (%)'] = 100 * lost / pq[s].sum()
        w[s] *= p_s[a, b] * n_k / pq[s].sum()                        # the stratum's chance over its share of the sample
        rows.append(row)
    part['weight'] = w
    kept = part[ok].reset_index(drop=True)
    return kept, pd.DataFrame(rows), counts


def nonfiler_records():
    """The non-filer batch's number of records (see the top)."""
    if G.get('nonfiler_records') is not None:
        return int(G['nonfiler_records'])
    n_filers = filer_records()
    nf, returns = cps_expected_nonfilers(common.TARGET_YEAR), TG.returns(TG.load(), common.TARGET_YEAR)
    n = int(round(n_filers * nf / returns))
    common.say(f'non-filer batch: {n:,} records = {n_filers:,} x {nf / 1e6:.2f}M CPS {common.TARGET_YEAR} expected '
               f'non-filers / {returns / 1e6:.2f}M SOI returns')
    return n


def load_models(device):
    """The pooled models (settings "generation", models; see the top): per model its run, network, training seed and
    generation-seed shift."""
    entries = MODELS if MODELS is not None else [{'run': os.path.basename(common.MODEL), 'seed_shift': 0}]
    assert len({e['run'] for e in entries}) == len(entries) and len({e['seed_shift'] for e in entries}) == len(entries), \
        'each pooled model is a different run with its own seed shift'
    out = []
    for e in entries:
        folder = common.MODEL if MODELS is None else f"{common.OUT}/model/{e['run']}"
        seed = int(json.load(open(os.path.join(folder, 'train_meta.json')))['seed'])
        out.append({'run': e['run'], 'seed': seed, 'seed_shift': int(e['seed_shift']),
                    'network': M.load_model(device, os.path.join(folder, 'model.pt'))})
        common.say(f"model {e['run']} (training seed {seed}), generation seeds shifted by {e['seed_shift']}")
    return out


def equal_shares(n, k):
    """n records split over k models as evenly as can be, the first ones taking any remainder."""
    return [n // k + (i < n % k) for i in range(k)]


def multipliers(var, bands, factor):
    """A boost as one multiplier per value of var (padded to the widest output): factor on bands, else 1."""
    m = np.ones(M.MAX_CHOICES)
    m[np.asarray(bands, dtype=np.int64)] = factor
    return m


def build_stack(models, factors, cache=None):
    """The filer batch (settings "generation", design: the strata, the four top parts, or unstratified) and the
    non-filer batch, each drawn in equal shares from every model and stacked; how many records each rule applied to;
    and, for the strata design, the label check per model and stratum (else None). factors: {model run: its offset
    factors}."""
    n_part, n_nf = G['records_per_file'], nonfiler_records()
    rule_counts = {r['name']: 0 for r in RULES}
    stacks, checks = [], []
    for k, (mod, n, n_nf_k) in enumerate(zip(models, equal_shares(n_part, len(models)),
                                             equal_shares(n_nf, len(models)))):
        model, shift, fac, parts = mod['network'], mod['seed_shift'], factors[mod['run']], []
        if DESIGN == 'strata':
            part, check, counts = stratified_part(model, mod, k, len(models), fac)
            part['weight'] *= filer_records() / len(models) / part['weight'].sum()   # each model: an equal share
            part['stratifier'] = 'strata'
            parts.append(part)
            checks.append(check)
            rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
            common.say(f"{mod['run']}: strata done; records dropped by the label check: "
                       f"{int(check['records drawn'].sum() - check['records kept'].sum()):,} of "
                       f"{int(check['records drawn'].sum()):,}")
        elif DESIGN == 'unstratified':
            n_u = equal_shares(int(G['unstratified_records']), len(models))[k]
            part, counts = generate(model, n_u, G.get('unstratified_seed', 51) + shift, factors=fac)
            part = label_check(part)
            part['weight'], part['p_band'], part['q_band'] = filer_records() / len(models) / n_u, np.nan, np.nan
            part['stratifier'] = 'unstratified'
            parts.append(part)
            rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
            common.say(f"{mod['run']}: unstratified done; label check fails on {int((~part['label_ok']).sum()):,} "
                       f"of {len(part):,} (kept: the unstratified draw drops nothing)")
        thin = None if THIN is None else (int(round(THIN['pilot_factor'] * n)), float(THIN['line']))
        for var, stratum in (TOP_PARTS.items() if DESIGN == 'top_parts' else []):
            seed = stratum['seed'] + shift
            top = M.positive_bands(var, 1 - G['top_share'], 1.0)
            if M.OWNED_BY.get(var) == 'scf':
                part, counts = generate(model, n, seed, boost=(var, multipliers(var, top, G['boost'])), factors=fac,
                                        thin=thin, cache=cache, cache_key=(mod['run'], var))
                part['weight'] = part['p_band'] / part['q_band']
            else:
                p = nothing_known(model, var)
                q = p.copy()
                q[top] *= G['boost']
                q /= q.sum()
                bands = torch.multinomial(torch.tensor(q), n if thin is None else thin[0], replacement=True,
                                          generator=torch.Generator().manual_seed(seed))
                part, counts = generate(model, n, seed, preset={var: bands}, factors=fac, thin=thin,
                                        cache=cache, cache_key=(mod['run'], var))
                b = bands.numpy()[part['pilot_row'].to_numpy()]
                part['weight'], part['p_band'], part['q_band'] = p[b] / q[b], p[b], q[b]
            part['weight'] /= part['p_keep']                    # undoes the thinning (1 when not thinned)
            part['weight'] *= n_part / len(models) / part['weight'].sum()   # each model: an equal share of the part
            part['stratifier'] = var
            parts.append(part)
            rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
            common.say(f"{mod['run']}: {var} done")
        part, counts = generate(model, n_nf_k, G.get('nonfiler_seed', 35) + shift, factors=fac, label='cps',
                                year=common.TARGET_YEAR, filer=0)
        part = part.copy()
        part['weight'], part['p_band'], part['q_band'], part['stratifier'] = n_nf / len(models) / n_nf_k, np.nan, \
            np.nan, 'nonfiler'                                  # each model: an equal share of the batch
        parts.append(part)
        rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
        common.say(f"{mod['run']}: nonfiler done")
        s = pd.concat(parts, ignore_index=True).drop(columns='pilot_row')
        s['model'] = mod['seed']
        assert len(s) < ID_BLOCK
        s['record_id'] = k * ID_BLOCK + np.arange(len(s))
        stacks.append(s)
    stack = pd.concat(stacks, ignore_index=True)
    by = stack[['model', 'weight']].assign(batch=np.where(stack['filer'] == 1, 'filers', 'non-filers'))
    share = by.groupby(['batch', 'model'])['weight'].sum() / by.groupby('batch')['weight'].sum()
    common.say('records and share of each batch\'s weight, by model:\n' + pd.DataFrame(
        {'records': by.groupby(['batch', 'model']).size(), 'weight share': share}).to_string())
    assert np.allclose(share.to_numpy(), 1 / len(models)), 'a model does not carry its equal share of a batch'
    common.say('consistency rules: records each applies to, over the whole stack:')
    common.say(pd.Series(rule_counts).to_string())
    return stack, (pd.concat(checks, ignore_index=True) if checks else None)


# ---- the offset -----------------------------------------------------------------
def held(m, f):
    """The chance of holding, the model's chance m with its odds multiplied by f."""
    return f * m / (f * m + (1 - m))


def offset(stack, factors, run):
    """Per ruled holding: the model's own expected holders, the holders with the rules, and the factor
    on the untouched records' odds that brings the second back to the first."""
    w = stack['weight'].to_numpy(float)
    rows, new = [], {}
    for v in RULED:
        m, forced = stack[f'_own_{v}'].to_numpy(float), stack[f'_forced_{v}'].to_numpy(bool)
        f0 = factors.get(v, 1.0)
        target = (w * m).sum()
        expected = (w * np.where(forced, 1.0, held(m, f0))).sum()
        fixed, rest = w[forced].sum(), ~forced
        lo, hi = -30.0, 30.0                                # bisection on the log of the factor
        for _ in range(100):
            mid = (lo + hi) / 2
            if fixed + (w[rest] * held(m[rest], np.exp(mid))).sum() > target:
                hi = mid
            else:
                lo = mid
        new[v] = float(np.exp((lo + hi) / 2))
        rows.append({'run': run, 'holding': v, "model's own expected holders": target,
                     'expected holders with the rules': expected, 'excess (%)': 100 * (expected / target - 1),
                     'forced draws (weighted)': fixed,
                     'forced draws the model would have made zero (weighted)': (w[forced] * (1 - m[forced])).sum(),
                     'factor used': f0, 'next factor': new[v]})
    return pd.DataFrame(rows), new


def main():
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    torch.manual_seed(common.SEED)
    models = load_models(device)
    os.makedirs(common.GEN, exist_ok=True)
    factors, report, cache = {m['run']: {} for m in models}, [], {}   # cache: the pilots, kept across runs
    runs = int(C['offset_generations'])
    for run in range(1, runs + 1):
        stack, check = build_stack(models, factors, cache)
        if check is not None:              # the last run's is kept
            check.insert(0, 'run', run)
            check.to_csv(os.path.join(common.GEN, 'strata_check.csv'), index=False)
            common.say(f'run {run} of {runs}, the label check by stratum:\n'
                       + check.drop(columns=['run']).to_string(index=False, float_format=lambda x: f'{x:.4g}'))
        nxt = {}
        for m in models:                    # each model's offset on its own records (see the top)
            table, nxt[m['run']] = offset(stack[stack['model'] == m['seed']], factors[m['run']], run)
            table.insert(1, 'model', m['seed'])
            report.append(table)
            common.say(f"run {run} of {runs}, model {m['run']}:\n" + table.drop(columns=['run', 'model']).to_string(index=False))
        if run < runs:
            factors = nxt
    pd.concat(report).to_csv(os.path.join(common.GEN, 'consistency.csv'), index=False)
    stack.drop(columns=[c for c in stack if c.startswith(('_own_', '_forced_'))]) \
        .to_parquet(os.path.join(common.GEN, 'stack.parquet'), index=False)


if __name__ == '__main__':
    main()

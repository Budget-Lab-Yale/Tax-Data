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
     as in the data). In the filer batch the AGI group label is preset
     before anything is drawn (below). Under the CPS label
     the age range is one of the six adult ranges (the CPS has no dependent
     filers). In the filer batch the SCF-only slots are absent meanwhile, as
     on a real PUF record; in the non-filer batch a PUF amount the CPS does not
     measure (interest, John's ruling of 23 October) is zero on every record,
     nothing drawn for it, and every slot the CPS has is present and hidden
     until its turn, except the CPS's two shares, absent
     until married and the share's base are drawn (model.gate_shares);
  2. the computed totals: capital gains from its three pieces (short and long term before the loss limit,
     distributions; since 29-30 October),
     business income and its two subtotals (Schedules C and F, Schedule E)
     from its eighteen parts, and (since 27 October) ordinary dividends from
     qualified and other dividends and the adjustments from their eleven parts. Each part gets a dollar value inside its band
     (one of 32 equally spaced weighted quantile points of the PUF records in
     that band), the signed sum is placed in the total's band on the PUF's
     cut points. Then the computed flag k1_owner: any partnership or
     S corporation part nonzero;
  2b. (since 4 November; earn_split.py) under the batch's label, the spouse's share of the couple's
     self-employment income (asked only of a married couple with Schedules C or F or part_se:
     absent until then and where not asked, zero at the end),
     then the earnings split code (0 unless married; 1-3 where the couple has wages; 0 where it has
     neither wages nor positive self-employment income), each given everything drawn so far. Neither
     is drawn among the amounts above, so no PUF item is drawn with them known: the filer batch's
     share comes from the PUF's own Schedule SE split, the non-filers' from the CPS's;
  (On a dependent filer, every survey slot below is zero or none: see structural.)
  3. the exact ages of the primary filer and the spouse (model.EXACT_AGES), in
     a random order per record, under the CPS label and the record's own CPS year (below) with every slot the CPS lacks left out: the CPS covers
     every tax unit, filers and non-filers, in every year, and its view holds
     the filing flag. The primary filer's age lies inside its age range; the
     spouse's exists exactly when married (cps_structure). The SCF, CEX and CPS
     phases below all see these ages;
  4. the SCF-only slots (the balance sheet, the unrealized gains and
     owns_business, and since 28 October the elective deferrals and the
     spouse's share of them) in a random order per record, except that the
     home mortgage, carried as a share of the home's value, comes after the
     home, and the spouse's share of the deferrals after the deferrals.
     Deferrals are zero without wages, the spouse's share zero unless married
     (deferral_structure).
     Each is drawn under the SCF label with the PUF-only slots left out, as
     the SCF would see the household, at the income year of the record's own
     SCF wave (below);
  5. the CEX-only slots (the 8 consumption categories) in a random order per
     record, each under the CEX label and the record's own CEX year (below) with every slot the CEX lacks left out: given married,
     dependents, the age range, the exact ages, wages, gross Social Security,
     self-employment income and the categories drawn so far;
  6. the CPS-only slots (dependents' ages, sex, the spouse's shares of the couple's
     wages and self-employment income, weeks and hours worked, childcare) in a
     random order per record, then (since 28 October) each adult's overtime pay
     in a random order (the primary filer's zero without wages, the spouse's
     without a spouse's share of wages above zero; each held to that person's
     wages less tips when bands become dollars, dollars.py; the tips are drawn
     later in the order, under the SIPP label), each under the CPS label and the record's own CPS year (below) with every slot the CPS lacks left out. They
     follow the record's PUF items (cps_structure): the spouse's items only
     when married, the k-th dependent's age only with k or more dependents.
     The spouse's share of the couple's wages (since 4 November) only in the bands
     that reach the range the earnings split code allows given the couple's wages and
     self-employment split, at each band's median PUF dollars (wage_share_bands);
     dollars.py then holds the exact share inside the exact range;
  7. the SIPP-only slots (since 26 October): each adult's tips in dollars,
     in a random order, then the two leisure and
     hospitality flags, in a random order, each under the SIPP label and the record's own SIPP year (below) with every slot the SIPP lacks left
     out: given married, dependents, the age range, the exact ages, wages and
     the spouse's share of the couple's wages drawn in step 6 (absent where
     not asked, as on the SIPP's rows). Rules (sipp_structure): the spouse's
     items are zero unless married; the primary filer's tips are zero
     without wages, the spouse's without a spouse's share of wages above zero;
     a flag is 0 where its tips are zero. Each person's tips are then held to
     at most that person's wages when bands become dollars (dollars.py).
Blindness is not drawn here: since the second retrain of 29 October it is a rule in the non-microdata step
(non_micro_imputations.blindness).
Each step runs the network only on the slots present in its phase and
computes only the output layers it needs.

The CEX, CPS and SIPP years (TARGETS.md section 7, 27 October; built 28 October). Each record is given one year of
each of the three surveys at random, with the shares in the target year's configuration (prior, "consumption", "cps
items", "sipp tips"; for 2019 the 2019 year 73.11% and the 2017 year 26.89%, survey_mix.py), each survey by a random
generator of its own (settings generation.survey_year_seeds and the batch's generation seed), so no other draw moves.
Its CPS phases (exact ages and the CPS-only items), its CEX phase and its SIPP phase run under that survey's label at
that year, and dollars.py turns its bands into dollars with that year's own map, grown to the target year. The stack
carries cex_year, cps_year and sipp_year. The non-filer batch's own items stay under the CPS label at the target year.

The SCF waves (TARGETS.md section 7). The balance sheet is drawn from a mix of SCF waves: the target year's
configuration file gives each wave's share (targets_config/<target year>/prior.yaml, "balance sheet"; for 2019
the 2019 wave 54.65%, the 2022 wave 33.15%, the 2016 wave 12.20%, by the rule of 24 October). Each record, filers and
non-filers alike, is given one wave at random with those shares, by a random generator of its own (seeded by settings
generation.scf_wave_seed and the batch's generation seed), so no other draw moves. Its SCF phase runs under the SCF
label at that wave's income year (the 2019 wave is income year 2018 in the table), and dollars.py turns its balance
sheet into dollars with that wave's own map. The stack carries scf_wave (the wave's survey year) and scf_year (its
income year). A dependent filer gets a wave too, but holds no balance sheet.

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

The filer batch: the AGI group first (the label design; qgroups.py). The network has learned the
PUF record's band-AGI group (agi_group, 12 groups) as a category, always shown in training. label_records filer
records over all models are split across the 12 groups in proportion to the PUF's records per group (which, with
grouped weighting, are also each group's shares of the PUF's training weight), each model drawing an equal share. Each
record starts with its group preset; everything else is drawn given it. Its weight is the group's PUF-year returns
over the records drawn in the group: 1 / q of the group, which undoes the up-counting of training exactly. Every
drawn record is kept. The label is checked, not trusted: each record's band-AGI group is computed from its drawn bands
(qgroups.band_agi) and compared with its label; the share that differs, and whether lower or higher, is reported per
model and group (label_check.csv) and kept in the stack (q_group, label_ok). It is only reported: the weight follows
the label.

The non-filer batch is one part (stratifier "nonfiler"), every weight 1.
Its number of records is the filer batch's records
times the CPS's expected non-filers in the target year (the sum of weight x (1 - filer_p)) over SOI's returns for
that year, so the stack's weights carry the two batches in their real proportion (the tilt sets their levels).

Several trained models pooled (TARGETS.md section 8; settings "generation",
models). Every group is drawn in equal shares from each model: with two
models, each draws half the records of each and half the non-filer batch, so the
stack has as many records as from one model. Each model's
records carry an equal share of its batch's weight, so in each batch
(filers, non-filers) every model carries the same share of the weight. Each
model has its own draws: its seed_shift is added to every generation seed
(the label part's and the non-filers'), so its draws are reproducible and differ
from the other models'. Each record says which model drew it (column model,
the network's training seed). Record ids are unique across models: model k
in the list (counting from 0) has the ids from k x 10,000,000. The offset
below is found for each model on its own records, since it brings each
model's holders back to that model's own expected holders.

Stored picks (dollars.py). For each record and each amount, one uniform number in [0, 1) is stored in the stack
(u_<variable>, seeded by the dollars seed, the model's position and the variable's position in dollars.ORDER). It
picks the record's real dollar amount within its band in every later step, so a record has one set of dollars, and
its target-year dollars are its PUF-year dollars grown.

Writes common.GEN/stack.parquet (one row per record: the band of every slot, weight, stratifier ('label' or
'nonfiler'), scf_wave and scf_year, the label check (q_group, label_ok; -1 and true on non-filers), model, record_id
and the stored picks), common.GEN/label_check.csv (per model and group: records drawn, the PUF's share, the weight
per record, and the share whose band group differs from the label) and common.GEN/consistency.csv (per run, model
and ruled holding: the model's own expected holders, the holders with the rules, the factor used and the next one).
"""
import json
import os

import numpy as np
import pandas as pd
import torch

from shared import common
from pretrain import model as M
from pretrain import qgroups as Q
from prep import soi as TG

G = common.SETTINGS['generation']
C = common.SETTINGS['consistency']
NC = len(M.CONTEXT)
P = M.PREDICTED
COMPUTED = M.COMPUTED                     # {total: {part: sign}}
FLAGS = M.COMPUTED_FLAGS                  # {flag: parts}
PUF_SLOTS = [v for v in P if 'puf' in M.PRESENT_ON[v] and v not in COMPUTED and v not in FLAGS]
# (4 November) drawn after the PUF items and their totals, in this order, under the batch's label (step 2b at the top)
LATE_SLOTS = [v for v in M.PUF_SHARES + list(M.SHARED_CATEGORIES) if v in P]
# (29 October; two groups since 6 November) the QBI items under the SCF label (build_table, meta scf_structure): each
# group's outside workers drawn after its industry group and zero with none; the industry group never none on a record
# with any of the group's PUF parts nonzero
SCF_STRUCTURE = M.META.get('scf_structure', {'after': {}, 'industry_parts': {}})
SCF_AFTER = {P.index(v): P.index(b) for v, b in SCF_STRUCTURE['after'].items()}
INDUSTRY_PARTS = {P.index(v): torch.tensor([P.index(q) for q in parts]) for v, parts in SCF_STRUCTURE['industry_parts'].items()}
SCF_SLOTS = M.SCF_SLOTS
CEX_SLOTS = M.CEX_SLOTS
CPS_SLOTS = M.CPS_SLOTS
CPS_STRUCTURE = M.META['cps_structure']
SIPP_SLOTS = M.SIPP_SLOTS
SIPP_STRUCTURE = M.META['sipp_structure']
SIPP_SPOUSE = torch.tensor([P.index(v) for v in SIPP_STRUCTURE['spouse']])
SIPP_FLAGS = list(SIPP_STRUCTURE['lh_of'])                    # drawn after the tips
SIPP_TIPS = [v for v in SIPP_SLOTS if v not in SIPP_FLAGS]
AGE_SLOTS = M.EXACT_AGES                   # drawn after the PUF items, under the CPS label (see the top)
SPOUSE = torch.tensor([P.index(v) for v in CPS_STRUCTURE['spouse']])
# (28 October) each adult's overtime: drawn after the other CPS slots, so the spouse's share of wages is in place
CPS_OVERTIME = [v for v in CPS_STRUCTURE.get('overtime', []) if v in CPS_SLOTS]
CPS_FIRST = [v for v in CPS_SLOTS if v not in CPS_OVERTIME]
CPS_ZERO_WITHOUT = CPS_STRUCTURE.get('zero_without', {})
DEP_AGES = [P.index(v) for v in CPS_STRUCTURE['dependent_ages']]          # the k-th: present when n_dep >= k
# (28 October) the dependents' Social Security number status: the k-th present when n_dep >= k, as the ages
DEP_SSN = [P.index(v) for v in CPS_STRUCTURE.get('dependent_ssn', [])]
PRIMARY_SSN = [P.index(v) for v in CPS_STRUCTURE.get('primary_ssn', [])]        # the primary filer's: never none
PRIMARY_AGE = P.index(CPS_STRUCTURE['primary_age'])
SEX_PRIMARY = P.index('sex_primary')
# the spouse's exact age (value 0 none, value a + 1 age a). A spouse, and a non-filer unit's head, is an adult: 18 or
# over (John, 26 October; cps_structure)
SPOUSE_AGE = P.index(next(v for v in CPS_STRUCTURE['spouse'] if v in CPS_STRUCTURE['ages']))
ADULT_AGE = 18
CPS_CATEGORICAL = torch.tensor([P.index(v) for v in M.PERSON_CATEGORIES])
# Every gated share (model.SHARE_GATES; since 4 November also the PUF's self-employment share): {slot index: [base
# indices]}. A share is asked only of a married couple with some of a base (a band above zero: band 0 is zero, -1 absent)
GATED = {s: list(bases) for s, (_, bases) in M.SHARE_GATES.items()}
ES_SLOT = P.index('earn_split') if 'earn_split' in P else None
WAGE_SHARE = P.index('spouse_wage_share') if 'spouse_wage_share' in P else None
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
SURVEY_ONLY = torch.tensor([P.index(v) for v in AGE_SLOTS + SCF_SLOTS + CEX_SLOTS + CPS_SLOTS + SIPP_SLOTS])
POINTS = 32
TYPE_SLOTS = M.TYPE_SLOTS                  # drawn first, in a random order per record
STATUS_SLOTS = list(M.TAX_STATUS)          # then these, in this order (mars, n_dep_ctc, n_dep_eitc)
MARS, MARRIED, AGE_RANGE = P.index('filing_status'), P.index('married'), P.index('age_range')
# The trained models pooled (see the top): each a run under OUT/model and the shift added to its generation seeds.
# null: the run's own model (common.MODEL) alone, with today's seeds.
MODELS = G['models']
ID_BLOCK = 10_000_000                       # model k's record ids start at k x ID_BLOCK
REAL_PUF = common.REAL_PUF                 # settings generation.filers 'real_puf': real_puf_part in place of the filer batch
# The filer batch's design (see the top): the AGI group preset on every record.
LABEL = 'agi_group'
assert M.PUF_LABELS == [LABEL], 'the label design needs the AGI group label in the table (build_table.agi_group_label)'
LABEL_META = M.META['labels'][LABEL]


def filer_records():
    """The filer batch's records over all models."""
    return int(G['label_records'])


def cps_expected_nonfilers(year):
    """The CPS's expected non-filers in year: the sum over its units of weight x (1 - filer_p)."""
    from reweight import target_list as TLS
    return TLS.nonfiler_count(year)   # a year with no CPS units: the geometric mean of the CPS years either side


def nonfiler_years():
    """[(CPS year, share)] the non-filer batch is drawn at: the target year alone when the CPS has it; otherwise
    (an even year, 30 October) the year's CPS mix (prior "cps items": the CPS years either side, half each), each
    year's part of the batch drawn under the CPS label at that year."""
    from shared import survey_mix
    from reweight import target_list as TLS
    if common.TARGET_YEAR in TLS.cps_years():
        return [(common.TARGET_YEAR, 1.0)]
    return survey_mix.mix('cps')


# ---- the SCF waves (TARGETS.md section 7) ------------------------------------------------------
def scf_waves():
    """The waves the balance sheet is drawn from, from the target year's configuration (prior, "balance sheet"):
    [(wave survey year, income year, share)], by survey year."""
    from reweight import target_list as TLS
    prior = TLS.resolve(common.TARGET_YEAR)['prior']['balance sheet']
    where = f'targets_config/{common.TARGET_YEAR}/prior.yaml, prior "balance sheet"'
    out = []
    for key, share in prior.items():
        src, wave = str(key).split()
        assert src == 'scf', f'{where}: {key} is not an SCF wave'
        income_year = int(wave) - 1
        assert income_year in M.YEARS, f'{where}: no {wave} wave (income year {income_year}) in the table'
        assert float(share) > 0, f'{where}: share {share} for {key}'
        out.append((int(wave), income_year, float(share)))
    total = sum(s for _, _, s in out)
    assert abs(total - 1) < 1e-6, f'{where}: shares sum to {total}'
    return sorted((w, y, s / total) for w, y, s in out)


SCF_WAVES = scf_waves()
SCF_YEAR_INDEX = np.array([M.YEARS.index(y) for _, y, _ in SCF_WAVES])


def assign_waves(n, seed):
    """Each of n records' wave (an index into SCF_WAVES), drawn with the shares by a generator of its own."""
    u = np.random.default_rng([int(G['scf_wave_seed']), int(seed)]).random(n)
    k = np.searchsorted(np.cumsum([s for _, _, s in SCF_WAVES]), u, side='right')
    return np.minimum(k, len(SCF_WAVES) - 1)


# ---- the CEX, CPS and SIPP years (TARGETS.md section 7, 27 October) ----------------------------
def survey_years():
    """{survey: [(tax year, share)]} for the CEX, CPS and SIPP, from the target year's configuration (prior,
    "consumption", "cps items", "sipp tips"; survey_mix.py). Every year must be one the model saw: the training table
    must hold rows of that survey in that year, and the year must be one of the model's years."""
    from shared import survey_mix
    t = pd.read_parquet(common.TABLE, columns=['source', 'year'])
    out = {}
    for src in survey_mix.SOURCES:
        out[src] = survey_mix.mix(src)
        seen = set(t.loc[t.source == src, 'year'])
        for y, share in out[src]:
            assert y in seen and y in M.YEARS, \
                f'{src} {y} is in the year mix but the training table has {src} years {sorted(seen)}: a year the model never saw cannot be drawn'
    return out


SURVEY_YEARS = survey_years()
SURVEY_YEAR_INDEX = {src: np.array([M.YEARS.index(y) for y, _ in m]) for src, m in SURVEY_YEARS.items()}


def assign_years(src, n, seed):
    """Each of n records' year of the survey src (an index into SURVEY_YEARS[src]), drawn with the shares by a generator
    of its own for that survey (settings generation.survey_year_seeds), so no other draw moves."""
    u = np.random.default_rng([int(G['survey_year_seeds'][src]), int(seed)]).random(n)
    k = np.searchsorted(np.cumsum([s for _, s in SURVEY_YEARS[src]]), u, side='right')
    return np.minimum(k, len(SURVEY_YEARS[src]) - 1)


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


# ---- rules on a draw -------------------------------------------------------------
def scf_order(size, gen):
    """A random order of the SCF-only slots per record, each ratio slot moved after its base."""
    scf = torch.tensor([P.index(v) for v in SCF_SLOTS])
    perm = scf[torch.rand(size, len(scf), generator=gen).argsort(1)]
    rows = torch.arange(size)
    pairs = [(P.index(slot), P.index(base)) for slot, (_, base) in M.RATIOS.items() if slot in SCF_SLOTS]
    # (the CPS's ratios: their base is a PUF slot, drawn earlier); the QBI items' workers after their industry group
    for i, ib in pairs + list(SCF_AFTER.items()):
        pos, pos_b = (perm == i).int().argmax(1), (perm == ib).int().argmax(1)
        swap = pos < pos_b
        perm[rows[swap], pos[swap]], perm[rows[swap], pos_b[swap]] = ib, i
    # (autoresearch, round 13) the QBI items (each group's industry group and its outside workers) after every other
    # SCF slot, keeping their order among themselves. qbi_structure gives an industry group to every record with the
    # group's PUF parts nonzero, while in the SCF only owners of a business have one; drawn before owns_business and
    # the balance sheet, a given industry made the record an owner (the 2019 file: 32 million units with a Schedule C
    # industry against the SCF's 7 million; ownership 16.8% against 11.6%, 13.6% in the bottom income quarter
    # against 3.4%).
    qbi = torch.tensor(sorted(set(INDUSTRY_PARTS) | set(SCF_AFTER)))
    key = torch.arange(perm.shape[1]).expand(size, -1) + perm.shape[1] * torch.isin(perm, qbi).long()
    perm = torch.gather(perm, 1, key.argsort(1))
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
    and a married couple's spouse has an age and a sex, the age 18 or over (every record, filers and non-filers;
    ages under 18 get no chance and the other ages are rescaled); the primary filer's sex is a man or a woman, never
    none (a dependent filer's is set to none after, in structural); the k-th dependent has an age, and a Social
    Security number status, exactly when there are at least k dependents (the primary filer always has a status); the
    primary filer's exact age lies in its age range, and a non-filer unit's head (the
    CPS-label batch, filing flag 0) is 18 or over (a 17-year-old head falls in none of the non-filer people targets'
    bands, adults from 18 and children under 17). Filers keep their PUF ranges: dependent filers can be teenagers."""
    married = values[:, NC + P.index('married')] == 1
    n_dep = values[:, NC + P.index('n_dep')]
    on_spouse = torch.isin(slot, SPOUSE.to(slot.device))
    categorical = torch.isin(slot, CPS_CATEGORICAL.to(slot.device))
    not_none = torch.ones(probs.shape[1], dtype=torch.bool, device=probs.device)
    not_none[0] = False
    probs = only(probs, on_spouse & ~married)
    sel = on_spouse & married & categorical
    probs = only(probs, sel, not_none.expand(int(sel.sum()), -1).float())
    sel = slot == SEX_PRIMARY    # the primary filer's sex is a man or a woman, never none (29 October: two records of the 20261019 model had an age and no sex)
    probs = only(probs, sel, not_none.expand(int(sel.sum()), -1).float())
    sel = (slot == SPOUSE_AGE) & married
    adult = torch.zeros(probs.shape[1], device=probs.device)
    adult[ADULT_AGE + 1:M.PERSON_CATEGORIES[P[SPOUSE_AGE]]] = 1.0   # value a + 1 is age a
    probs = only(probs, sel, adult.expand(int(sel.sum()), -1))
    for k, j in list(enumerate(DEP_AGES, start=1)) + list(enumerate(DEP_SSN, start=1)):
        probs = only(probs, (slot == j) & (n_dep < k))
        sel = (slot == j) & (n_dep >= k)
        probs = only(probs, sel, not_none.expand(int(sel.sum()), -1).float())
    for j in PRIMARY_SSN:
        sel = slot == j
        probs = only(probs, sel, not_none.expand(int(sel.sum()), -1).float())
    # overtime (28 October): the primary filer's is zero without wages, the spouse's without a spouse's share of the
    # couple's wages above zero (absent, -1, or zero); each is then held to the person's wages less tips in dollars (dollars.py)
    for item, base in CPS_ZERO_WITHOUT.items():
        probs = only(probs, (slot == P.index(item)) & (values[:, NC + P.index(base)] <= 0))
    sel = slot == PRIMARY_AGE
    if sel.any():
        rng = RANGE_OF_AGE_VALUE.to(probs.device)
        allowed = torch.zeros(int(sel.sum()), probs.shape[1], device=probs.device)
        allowed[:, :len(rng)] = (rng[None, :] == values[sel, NC + P.index('age_range')][:, None]).float()
        nonfiler = values[sel, NC + P.index('filer')] == 0
        allowed[nonfiler, :ADULT_AGE + 1] = 0.0                       # value a + 1 is age a; value 0 is none
        probs = only(probs, sel, allowed)
    return probs


def positive(values, v):
    """Whether amount slot v is drawn above zero (a positive band)."""
    return values[:, NC + P.index(v)] >= M.BAND_LAYOUT[v]['first_positive']


def earn_structure(values, slot, probs):
    """The earnings split code (4 November; earn_split.py): 0 unless married; on a married couple, 1-3 where it has
    wages (the PUF has code 0 with earnings on 0.02% of joint returns' weight), 0 where it has neither wages nor
    positive Schedules C and F or part_se; otherwise as the model draws."""
    if ES_SLOT is None:
        return probs
    sel = slot == ES_SLOT
    if not bool(sel.any()):
        return probs
    married = values[:, NC + MARRIED] == 1
    wages = positive(values, 'wages')
    se = torch.zeros_like(married)
    for b in GATED.get(P.index('spouse_se_share'), []):
        se = se | (values[:, NC + b] >= M.BAND_LAYOUT[P[b]]['first_positive'])
    probs = only(probs, sel & (~married | (~wages & ~se)))
    some = sel & married & wages
    allowed = torch.zeros(int(some.sum()), probs.shape[1], device=probs.device)
    allowed[:, 1:4] = 1.0
    return only(probs, some, allowed)


_BAND_TABLES = {}


def se_earnings_t(x):
    """Schedule SE net earnings from self-employment income x (a tensor): 92.35% of it where that is $400 or more."""
    e = float(M.META['earn_split']['se_factor']) * x
    return torch.where(e >= float(M.META['earn_split']['se_floor']), e, torch.zeros_like(e))


def capped_wage_share_range(W, e1, e2, a, b, cap):
    """The spouse's wage shares (low, high) that put the primary's share of earnings in [a, b], each person's earnings
    counted up to the room under the wage base (dollars.wage_share_bounds; the primary's share falls as the spouse's
    wage share rises, so each end is found by halving). Where no share reaches the range, the nearest share, 0 or 1."""
    def primary_share(s):
        w1, w2 = W * (1 - s), W * s
        p1 = w1 + torch.minimum(e1, (cap - w1).clamp(min=0))
        p2 = w2 + torch.minimum(e2, (cap - w2).clamp(min=0))
        return p1 / (p1 + p2).clamp(min=1e-9)
    s0, s1 = torch.zeros_like(W), torch.ones_like(W)
    t0, t1 = s0.clone(), s1.clone()
    for _ in range(40):
        m = (s0 + s1) / 2
        ok = primary_share(m) <= b                      # the smallest share at or below the top of the range
        s1, s0 = torch.where(ok, m, s1), torch.where(ok, s0, m)
        m = (t0 + t1) / 2
        ok = primary_share(m) >= a                      # the largest share at or above the bottom of the range
        t0, t1 = torch.where(ok, m, t0), torch.where(ok, t1, m)
    lo = torch.where(primary_share(torch.zeros_like(W)) <= b, torch.zeros_like(W), s1)
    hi = torch.where(primary_share(torch.ones_like(W)) >= a, torch.ones_like(W), t0)
    return lo, hi


def band_tables(device):
    """For wage_share_bands: each band's median PUF-year dollars (the band edges' record; an empty band, the middle of
    its cut points) of wages, the self-employment share's bases and the share itself; the earnings split codes' ranges
    of the primary's share; and, per year index, each band's range of the spouse's share of wages on that CPS year's
    cut points (band 0 the share 0; loss bands never)."""
    key = str(device)
    if key in _BAND_TABLES:
        return _BAND_TABLES[key]
    E = common.edges()
    bases = [P[b] for b in GATED.get(P.index('spouse_se_share'), [])] if 'spouse_se_share' in P else []
    med = {}
    for name in ['wages', 'spouse_se_share'] + bases:
        e = E[f'puf/{common.PUF_YEAR}/{name}']
        n, fp = M.BAND_LAYOUT[name]['n_bands'], M.BAND_LAYOUT[name]['first_positive']
        pos, neg = list(e['positive']['cuts']), list(e['negative']['cuts'])
        x = np.zeros(n)
        for k, rec in enumerate(e['bands']):
            if rec['median'] is not None:
                x[k] = rec['median']
            elif k >= fp:
                i = k - fp
                lo = pos[i - 1] if i >= 1 else 0.0
                hi = pos[i] if i < len(pos) else lo
                x[k] = (lo + hi) / 2
            elif 1 <= k < fp and neg:
                i = k - 1
                lo = neg[i - 1] if i >= 1 else neg[0]
                hi = neg[i] if i < len(neg) else 0.0
                x[k] = (lo + hi) / 2
        med[name] = torch.tensor(x, dtype=torch.float64, device=device)
    rng = M.META['earn_split']['ranges']
    low = torch.tensor([0.0] + [rng[str(k)][0] for k in (1, 2, 3)], dtype=torch.float64, device=device)
    high = torch.tensor([0.0] + [rng[str(k)][1] for k in (1, 2, 3)], dtype=torch.float64, device=device)
    n = M.N_CHOICES['spouse_wage_share']
    fp = M.BAND_LAYOUT['spouse_wage_share']['first_positive']
    wlo = np.full((len(M.YEARS), n), 2.0)
    whi = np.full((len(M.YEARS), n), -1.0)
    for i, y in enumerate(M.YEARS):
        e = E.get(f'cps/{y}/spouse_wage_share')
        if e is None or 'positive' not in e:
            continue
        cuts = list(e['positive']['cuts'])
        wlo[i, 0] = whi[i, 0] = 0.0
        for k in range(fp, n):
            j = k - fp
            wlo[i, k] = cuts[j - 1] if j >= 1 else 0.0
            whi[i, k] = cuts[j] if j < len(cuts) else 1.0
    _BAND_TABLES[key] = {'median': med, 'se_bases': bases, 'low': low, 'high': high,
                         'wage_lo': torch.tensor(wlo, dtype=torch.float64, device=device),
                         'wage_hi': torch.tensor(whi, dtype=torch.float64, device=device)}
    return _BAND_TABLES[key]


def sipp_structure(values, slot, probs):
    """The SIPP's items follow the record's: the spouse's tips and flag are zero unless married; the primary filer's
    tips are zero without wages, the spouse's without a spouse's share of wages above zero (not asked, -1, or zero); a
    leisure and hospitality flag is 0 where its tips are zero (the tips are drawn first)."""
    married = values[:, NC + MARRIED] == 1
    probs = only(probs, torch.isin(slot, SIPP_SPOUSE.to(slot.device)) & ~married)
    for share, base in SIPP_STRUCTURE['zero_without'].items():
        probs = only(probs, (slot == P.index(share)) & (values[:, NC + P.index(base)] <= 0))
    for flag, share in SIPP_STRUCTURE['lh_of'].items():
        sel = slot == P.index(flag)
        if sel.any():
            assert (values[sel, NC + P.index(share)] >= 0).all(), f'{share} not drawn before {flag}'
        probs = only(probs, sel & (values[:, NC + P.index(share)] == 0))
    return probs


def deferral_structure(values, slot, probs):
    """Elective deferrals (decided 27 October): the unit's are zero without wages (none without wages, as for tips;
    dollars.py then holds each person's to their own wages and to the legal limit), and the spouse's share of them
    is zero unless married (with no deferrals it is zero by the ratio rule in structural)."""
    if 'elective_deferrals' in P:
        probs = only(probs, (slot == P.index('elective_deferrals')) & (values[:, NC + P.index('wages')] <= 0))
    if 'spouse_deferral_share' in P:
        probs = only(probs, (slot == P.index('spouse_deferral_share')) & (values[:, NC + MARRIED] != 1))
    return probs


def qbi_structure(values, slot, probs):
    """The QBI items (29 October): a group's outside workers are zero where its industry group is none (drawn first,
    scf_order); its industry group is not none where any of the group's PUF parts is nonzero (the record has such a
    business: the model's chances of the 8 groups, rescaled)."""
    for j, ind in SCF_AFTER.items():
        probs = only(probs, (slot == j) & (values[:, NC + ind] == 0))
    for j, parts in INDUSTRY_PARTS.items():
        sel = slot == j
        if not bool(sel.any()):
            continue
        has = (values[:, NC + parts.to(values.device)] != 0).any(1) & sel
        not_none = torch.ones(probs.shape[1], device=probs.device)
        not_none[0] = 0.0
        probs = only(probs, has, not_none.expand(int(has.sum()), -1))
    return probs


def structural(values, slot, probs):
    """A ratio slot whose base is zero is zero (no home: no mortgage share; no wages: no spouse's share of them; no
    deferrals: no spouse's share of them). Elective deferrals follow wages and marriage (deferral_structure).
    The CPS's person items follow the PUF items (cps_structure). A dependent filer holds no survey-only item
    (balance sheet, unrealized gains, business ownership, consumption, the CPS's person items and childcare): the
    surveys count it in the household that claims it, so on its own return every one is zero or none, and its
    wages and self-employment income are all its own."""
    for slot_name, (_, base) in M.RATIOS.items():
        if P.index(slot_name) in GATED:     # a gated share: zero where not asked (Chunk.not_asked), not by one base
            continue
        no_base = (slot == P.index(slot_name)) & (values[:, NC + P.index(base)] == 0)
        probs[no_base] = 0.0
        probs[no_base, 0] = 1.0
    probs = earn_structure(values, slot, probs)
    probs = cps_structure(values, slot, probs)
    probs = sipp_structure(values, slot, probs)
    probs = deferral_structure(values, slot, probs)
    probs = qbi_structure(values, slot, probs)
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
    """size records being drawn: every slot's band (-1 absent), which slots are drawn, and the rules'
    bookkeeping. Each step runs the network only on the slots present in its phase (model.SOURCE_SLOTS:
    the PUF's, then the CPS's view for the exact ages, then the SCF's, the CEX's and the CPS's views) and computes only the output layers it needs.
    scf_year: each record's index into model.YEARS for its SCF phase (its wave's income year); survey_year: the same
    for the CEX, CPS and SIPP phases, per survey."""

    def __init__(self, model, size, order, preset, start, factors, gen, scf_year, survey_year, label='puf', year=None,
                 adult_only=False):
        device = order.device
        self.adult_only = adult_only                                 # an adult age range under any label (real PUF)
        self.model, self.order, self.factors, self.gen = model, order, factors, gen
        self.scf_year = scf_year.to(device)
        self.survey_year = {src: y.to(device) for src, y in survey_year.items()}   # per survey: each record's year index
        self.label = label                                           # the batch's label for its own items
        self.rows = torch.arange(size, device=device)
        self.values = torch.full((size, len(M.SLOTS)), -1, dtype=torch.long, device=device)
        self.values[:, 0] = M.SOURCES.index(label)
        self.values[:, 1] = M.YEARS.index(common.PUF_YEAR if year is None else year)
        self.values[self.rows[:, None], NC + order] = 0              # present, not yet drawn
        for v in AGE_SLOTS + SCF_SLOTS + CEX_SLOTS + CPS_SLOTS + SIPP_SLOTS:
            self.values[:, NC + P.index(v)] = -1                      # absent until their turn
        if label == 'cps':        # the CPS's own slots: present and hidden from the start, as on a CPS training row
            for v in AGE_SLOTS + CPS_SLOTS + [r for r in M.REPORTED if 'cps' in M.PRESENT_ON[r]]:
                self.values[:, NC + P.index(v)] = 0     # (and the CPS's reported slots of its filled-in items, 28 Oct.)
        for j in GATED:           # a share is absent until married and its bases are drawn (gate_shares, after_totals)
            self.values[:, NC + j] = -1
        for tot in list(COMPUTED) + list(FLAGS):
            self.values[:, NC + P.index(tot)] = 0                    # present and hidden until computed
        self.filled = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        for var, bands in preset.items():
            self.values[:, NC + P.index(var)] = bands[start:start + size].to(device)
            self.filled[:, P.index(var)] = True
            if P.index(var) in GATED:
                # (run6, 5 November; filers real_puf) a gated share preset on some records only: -1 is no preset, and
                # those records are asked or not by the generator's own rule and drawn in step 2b; a preset share
                # stands, whether or not the rule would ask it
                self.filled[:, P.index(var)] = bands[start:start + size].to(device) >= 0
        self.triggered = torch.zeros(size, len(RULES), dtype=torch.bool, device=device)
        self.resolved = self.triggered.clone()
        self.must = torch.zeros(size, len(P), dtype=torch.bool, device=device)
        self.own_chance = torch.zeros(size, len(RULED), dtype=torch.float64, device=device)
        self.forced = torch.zeros(size, len(RULED), dtype=torch.bool, device=device)
        self.ruled = torch.tensor([P.index(v) for v in RULED], device=device)
        self.column = {}                                             # per source: each of SLOTS' column, -1 if absent
        for src, slots in M.SOURCE_SLOTS.items():
            self.column[src] = torch.full((len(M.SLOTS),), -1, dtype=torch.long, device=device)
            self.column[src][list(slots)] = torch.arange(len(slots), device=device)
        self.not_asked = {}
        self.bounded = 0                                             # wage shares drawn inside an earnings split range
        self.gate_shares()

    def gate_shares(self):
        """Own phase: each share not yet drawn present (hidden) once married and its bases are drawn and the unit is
        married with some of a base; absent before that, and where not asked (model.gate_shares). In the filer batch the
        self-employment share's base Schedules C and F is a computed total, so the share stays absent until the totals."""
        for s, (m, bases) in M.SHARE_GATES.items():
            known = self.filled[:, m].clone()
            some = torch.zeros_like(known)
            for b in bases:
                known &= self.filled[:, b]
                some |= self.values[:, NC + b] > 0
            asked = known & (self.values[:, NC + m] == 1) & some
            self.values[:, NC + s] = torch.where(self.filled[:, s], self.values[:, NC + s],
                                                 torch.where(asked, 0, -1))

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
        if (self.label == 'cps' or self.adult_only) and sel.any():
            allowed = torch.zeros(int(sel.sum()), probs.shape[1], device=probs.device)
            allowed[:, :common.ADULT_RANGES] = 1.0
            probs = only(probs, sel, allowed)
        return probs

    def last_layer(self, values, hidden, src):
        """The network on the source's slots, all of which must be present."""
        slots = M.SOURCE_SLOTS[src]
        may_be_absent = [M.SOURCE_SLOTS[src].index(NC + j) for j in GATED if src in M.PRESENT_ON[P[j]]]
        present = values[:, M.slot_tensor(slots, values.device)] >= 0
        present[:, may_be_absent] = True
        assert present.all(), f'a {src} slot is absent'
        return self.model.last_layer(*M.pack(values, hidden, slots), slots)

    def take(self, probs, slot):
        """Draw slot from probs, and record it."""
        draw = torch.multinomial(probs, 1, generator=self.gen).squeeze(1)
        self.values[self.rows, NC + slot] = draw
        self.filled[self.rows, slot] = True

    def puf_step(self, slot):
        """One of the batch's own items per record (a PUF variable), under the batch's label, given the slots drawn
        so far."""
        hidden = (self.values[:, NC:] >= 0) & ~self.filled
        h = self.last_layer(self.values, hidden, self.label)
        probs = self.model.slot_probs(h[self.rows, self.column[self.label][NC + slot]], slot)
        self.take(self.own_rules(slot, probs), slot)
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
        """compute_totals' bookkeeping, which depends only on the values drawn so far: the rules each record triggers,
        the survey slots made present, the CPS shares' records with no question. Returns the rule counts."""
        v = self.values
        dep = dependent(v)
        for r in range(len(RULES)):
            self.triggered[:, r] = (v[:, NC + RULE_ITEMS[r].to(v.device)] != 0).any(1) & ~dep
        for s in AGE_SLOTS + SCF_SLOTS + CEX_SLOTS + CPS_SLOTS + SIPP_SLOTS:
            v[:, NC + P.index(s)] = 0                                # each survey's view leaves out the others'
        v[:, NC + REP_SLOTS.to(v.device)] = 0                        # reported slots: present, never drawn (hidden)
        married = v[:, NC + P.index('married')] == 1
        self.not_asked = {}                                         # the shares' records with no question
        for j, bases in GATED.items():
            some = torch.zeros_like(married)
            for b in bases:
                some |= v[:, NC + b] > 0
            self.not_asked[j] = (~married | ~some | dep) & ~self.filled[:, j]   # (run6) a preset share stands
            v[~self.not_asked[j] & ~self.filled[:, j], NC + j] = 0   # asked, not yet drawn: present and hidden
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
        if label == 'cps' and WAGE_SHARE is not None and ES_SLOT is not None:
            probs = self.wage_share_bands(slot, probs)
        # (run6) a slot preset on some records only (a real PUF record's share) keeps its preset value; in a synthetic
        # batch no survey step's slot is filled before its own step, so nothing changes there
        keep = self.filled[self.rows, slot]
        before = self.values[self.rows, NC + slot].clone()
        self.take(probs, slot)
        if bool(keep.any()):
            self.values[self.rows[keep], NC + slot[keep]] = before[keep]
        for j, m in self.not_asked.items():
            self.values[m, NC + j] = -1

    def wage_share_bands(self, slot, probs):
        """The spouse's share of the couple's wages (CPS phase): on a married couple with wages and an earnings split
        code of 1-3, only the bands of the record's CPS year that reach the shares putting the primary's share of
        earnings in the code's range (earn_split.wage_share_bounds), with the couple's wages, Schedules C and F,
        part_se and self-employment share at their bands' median PUF-year dollars, and each person's earnings counted
        up to the PUF year's wage base, as E30400 counts them. Where the model gives every such band no chance, they are
        taken as equally likely (only). dollars.py holds the exact share inside the exact range."""
        sel = (slot == WAGE_SHARE) & ~self.not_asked.get(WAGE_SHARE, torch.zeros_like(slot, dtype=torch.bool))
        code = self.values[:, NC + ES_SLOT]
        sel = sel & (code >= 1) & (code <= 3)
        if not bool(sel.any()):
            return probs
        B = band_tables(probs.device)
        v = self.values[sel]
        med = lambda name: B['median'][name][v[:, NC + P.index(name)].clamp(min=0)]
        W = med('wages')
        X = torch.zeros_like(W)
        for name in B['se_bases']:
            X = X + torch.where(v[:, NC + P.index(name)] >= 0, med(name), torch.zeros_like(W))
        sh = torch.where(v[:, NC + P.index('spouse_se_share')] >= 0, med('spouse_se_share'), torch.zeros_like(W))
        e2, e1 = se_earnings_t(sh * X), se_earnings_t((1 - sh) * X)
        c = code[sel]
        lo, hi = capped_wage_share_range(W, e1, e2, B['low'][c], B['high'][c], float(M.META['earn_split']['se_wage_base']))
        yi = self.survey_year['cps'][sel]
        blo, bhi = B['wage_lo'][yi], B['wage_hi'][yi]                # [n, bands]: each band's share range, that year
        allowed = torch.zeros(len(lo), probs.shape[1], device=probs.device)   # (the choices past the share's bands: 0)
        allowed[:, :blo.shape[1]] = ((bhi >= lo[:, None] - 1e-9) & (blo <= hi[:, None] + 1e-9)).float()
        none = allowed.sum(1) == 0
        allowed[none] = 1.0                                          # (not expected: the bands cover 0 to 1)
        self.bounded += int(sel.sum())
        return only(probs, sel, allowed)

    def finish(self):
        """The CPS shares not asked of a record: zero (no spouse, or none of the item, or a dependent filer). Called
        after the SIPP phase, which sees them absent where not asked, as on the SIPP's rows."""
        for j, m in self.not_asked.items():
            self.values[m, NC + j] = 0


@torch.no_grad()
def generate(model, n, seed, preset=None, factors=None, chunk=50_000, label='puf', year=None, filer=1,
             totals_preset=False, adult_only=False):
    """n PUF-style records, and how many records each consistency rule applies to. label, year: the batch's label and
    year for its own items (default the PUF at the PUF year); filer: the batch's filing flag, set before anything is
    drawn. preset: {variable: bands, one per record} set before anything is drawn (the filer batch: the AGI group
    label). factors: {ruled holding: factor on its odds of being nonzero}. Each record's SCF wave is drawn first, by a
    generator of its own (assign_waves). For each ruled holding the table also carries the model's own chance it is
    nonzero (_own_<v>) and whether a rule forced it (_forced_<v>).
    totals_preset (filers real_puf, 4 November): the computed totals and flags are in preset (a real PUF record's
    own bands) and are kept as they are; only the bookkeeping that follows them is done (Chunk.after_totals).
    adult_only: an age range drawn under the PUF label is one of the six adult ranges (the PUF's aggregate records,
    which are no one's dependents)."""
    preset, factors = dict(preset or {}), factors or {}
    if totals_preset:
        missing = [v for v in list(COMPUTED) + list(FLAGS) if v not in preset]
        assert not missing, f'totals_preset: the computed totals and flags must be preset, missing {missing}'
    assert label != 'puf' or all(v in preset for v in M.PUF_LABELS), \
        'the filer batch presets the AGI group label (the network never sees it hidden on a PUF record)'
    preset['filer'] = torch.full((n,), filer, dtype=torch.long)
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
    wave_k = assign_waves(n, seed)                                 # each record's SCF wave (see the top)
    scf_year = torch.as_tensor(SCF_YEAR_INDEX[wave_k])
    svy_k = {src: assign_years(src, n, seed) for src in SURVEY_YEARS}      # each record's CEX, CPS and SIPP year
    svy_year = {src: torch.as_tensor(SURVEY_YEAR_INDEX[src][k]) for src, k in svy_k.items()}
    first = torch.tensor([P.index(v) for v in TYPE_SLOTS if v not in preset], dtype=torch.long)
    then = torch.tensor([P.index(v) for v in STATUS_SLOTS if v not in preset], dtype=torch.long)
    rest = torch.tensor([P.index(v) for v in PUF_SLOTS if v not in preset and v not in TYPE_SLOTS
                         and v not in STATUS_SLOTS and v not in M.PUF_LABELS and v not in LATE_SLOTS], dtype=torch.long)
    own = torch.cat([first, then, rest])
    # step 2b; (run6) a gated share preset on some records only is drawn here for the others
    late = torch.tensor([P.index(v) for v in LATE_SLOTS if v not in preset or P.index(v) in GATED], dtype=torch.long)
    cex = torch.tensor([P.index(v) for v in CEX_SLOTS])
    cps = torch.tensor([P.index(v) for v in CPS_FIRST])
    cps_ot = torch.tensor([P.index(v) for v in CPS_OVERTIME], dtype=torch.long)
    sipp_tips = torch.tensor([P.index(v) for v in SIPP_TIPS])
    sipp_flags = torch.tensor([P.index(v) for v in SIPP_FLAGS])
    age = torch.tensor([P.index(v) for v in AGE_SLOTS])
    n_own, n_age, n_scf, n_cex, n_cps = len(own), len(AGE_SLOTS), len(SCF_SLOTS), len(CEX_SLOTS), len(CPS_SLOTS)
    n_late = len(late)
    rule_counts = {r['name']: 0 for r in RULES}

    def orders(size):
        return torch.cat([first[torch.rand(size, len(first), generator=cpu_gen).argsort(1)],
                          then.expand(size, -1),
                          rest[torch.rand(size, len(rest), generator=cpu_gen).argsort(1)],
                          late.expand(size, -1),
                          age[torch.rand(size, len(age), generator=cpu_gen).argsort(1)],
                          scf_order(size, cpu_gen),
                          cex[torch.rand(size, len(cex), generator=cpu_gen).argsort(1)],
                          cps[torch.rand(size, len(cps), generator=cpu_gen).argsort(1)],
                          cps_ot[torch.rand(size, len(cps_ot), generator=cpu_gen).argsort(1)],
                          sipp_tips[torch.rand(size, len(sipp_tips), generator=cpu_gen).argsort(1)],
                          sipp_flags[torch.rand(size, len(sipp_flags), generator=cpu_gen).argsort(1)]], 1).to(device)

    def finish(d, order):
        """Steps 2b to 7 on a chunk whose own items and totals are drawn."""
        k = n_own
        for step in range(k, k + n_late):      # step 2b: the self-employment share and the earnings split code
            d.survey_step(order[:, step], d.label)
        k += n_late
        for step in range(k, k + n_age):
            d.survey_step(order[:, step], 'cps', d.survey_year['cps'])
        k += n_age
        for step in range(k, k + n_scf):       # each record at its own wave's income year
            d.survey_step(order[:, step], 'scf', d.scf_year)
        k += n_scf
        for step in range(k, k + n_cex):
            d.survey_step(order[:, step], 'cex', d.survey_year['cex'])
        k += n_cex
        for step in range(k, k + n_cps):
            d.survey_step(order[:, step], 'cps', d.survey_year['cps'])
        k += n_cps
        for step in range(k, order.shape[1]):
            d.survey_step(order[:, step], 'sipp', d.survey_year['sipp'])
        d.finish()
        return d.values.cpu(), d.own_chance.cpu(), d.forced.cpu()

    parts = []
    for start in range(0, n, chunk):           # steps 1 to 6, chunk by chunk
        size = min(chunk, n - start)
        order = orders(size)
        d = Chunk(model, size, order, preset, start, factors, gpu_gen, scf_year[start:start + size],
                  {src: y[start:start + size] for src, y in svy_year.items()}, label, year, adult_only)
        for step in range(n_own):
            d.puf_step(order[:, step])
        for name, count in (d.after_totals() if totals_preset else d.compute_totals()).items():
            rule_counts[name] += count
        parts.append(finish(d, order))
    table = to_table(torch.cat([p[0] for p in parts]))
    oc, fc = torch.cat([p[1] for p in parts]).numpy(), torch.cat([p[2] for p in parts]).numpy()
    for i, v in enumerate(RULED):
        table[f'_own_{v}'], table[f'_forced_{v}'] = oc[:, i], fc[:, i]
    table['scf_wave'] = np.array([w for w, _, _ in SCF_WAVES])[wave_k]
    table['scf_year'] = np.array([y for _, y, _ in SCF_WAVES])[wave_k]
    for src, k in svy_k.items():       # the year each record's CEX, CPS and SIPP phases ran at (the tax year)
        table[f'{src}_year'] = np.array([y for y, _ in SURVEY_YEARS[src]])[k]
    return table, rule_counts


def to_table(values):
    v = values.numpy()
    out = pd.DataFrame({'source': [M.SOURCES[i] for i in v[:, 0]], 'year': [M.YEARS[i] for i in v[:, 1]]})
    for j, var in enumerate(P):
        out[M.column(var)] = v[:, NC + j]
    out['weight'] = 1.0
    return out


# ---- the filer batch: the AGI group first (see the top) ---------------------------------------
def label_counts(n, records):
    """n records split across the groups in proportion to records (largest remainders)."""
    share = np.asarray(records, float) / np.sum(records)
    base = np.floor(n * share).astype(int)
    rem = n - base.sum()
    base[np.argsort(-(n * share - base), kind='stable')[:rem]] += 1
    return base


def label_part(model, mod, k, n_models, fac, group_records=None, seed=None):
    """Model k's (of n_models) filer batch: its share of label_records split across the 12 groups in proportion to the
    PUF's records per group, each record's group preset and everything else drawn given it; weight = the group's
    PUF-year returns / records drawn in it (1 / q). Then each record's band-AGI group from its drawn bands (q_group)
    and label_ok = (q_group == agi_group); every record kept. Returns the part, the check's table and the rule
    counts.
    group_records, seed (29 October, for the rebuilt aggregate records, top_hidden.py): a separate allocation of this model's
    records across the 12 groups (a group with 0 records is not drawn) and its own generation seed, in place of the
    proportional split and label_seed. The weight rule is the same, so each drawn group still carries its PUF-year
    returns. Training is unchanged."""
    rec, ret = np.asarray(LABEL_META['records'], float), np.asarray(LABEL_META['returns'], float)
    if group_records is None:
        n_k = equal_shares(filer_records(), n_models)[k]
        n_g = label_counts(n_k, rec)
    else:
        n_g = np.asarray(group_records, dtype=int)
        assert n_g.shape == rec.shape and (n_g >= 0).all() and n_g.sum() > 0, f'label_part group_records {n_g}'
        n_k = int(n_g.sum())
    g_of = np.repeat(np.arange(len(rec)), n_g)
    seed = int(G['label_seed']) + mod['seed_shift'] if seed is None else int(seed)
    part, counts = generate(model, n_k, seed, preset={LABEL: torch.tensor(g_of)}, factors=fac)
    g = part[LABEL].to_numpy()
    assert (g == g_of).all()
    part['weight'] = ret[g] / n_g[g]
    qg = Q.puf_group(Q.band_agi(part), Q.puf_groups()['cuts'])
    part['q_group'], part['label_ok'] = qg, qg == g
    rows = []
    for j, name in enumerate(LABEL_META['labels']):
        s, x = g == j, qg[g == j]
        rows.append({'model': mod['seed'], 'group': name, 'records drawn': int(s.sum()),
                     'PUF records share': rec[j] / rec.sum(), 'weight per record (returns)': ret[j] / max(1, n_g[j]),
                     'band group differs (%)': 100 * float((x != j).mean()) if s.any() else np.nan,
                     'band group lower (%)': 100 * float((x < j).mean()) if s.any() else np.nan,
                     'band group higher (%)': 100 * float((x > j).mean()) if s.any() else np.nan,
                     'band group two or more away (%)': 100 * float((np.abs(x - j) >= 2).mean()) if s.any() else np.nan})
    check = pd.DataFrame(rows)
    common.say(f"{mod['run']}: the label check, the band-AGI group of the drawn bands against the label:\n"
               + check.to_string(index=False, float_format=lambda x: f'{x:.4g}'))
    return part, check, counts


# ---- the real PUF as the filers (4 November 2026; settings generation.filers 'real_puf') -------------------------
# In place of the filer batch drawn from the networks, every 2015 PUF record keeps all its own PUF items (their bands
# preset, so the draw is conditional on them) and each pooled network draws everything else given the record: the
# exact ages, the balance sheet, consumption, the CPS's and the SIPP's items. Each record so appears once per network
# (a "copy"), at its PUF weight divided by the number of networks. The record's own 2015 amounts go with it (columns
# own_<item>); dollars.own_values grows them to the target year in place of a pick from the band's dollar map.
REAL_ID = 50_000_000            # a real record's id: REAL_ID + copy index x REAL_COPY_BLOCK + its PUF RECID
REAL_COPY_BLOCK = 1_000_000
_REAL = {}


def own_items():
    """The PUF amounts a real record carries its own 2015 value of: every PUF amount of the dollar map (the drawn
    items and the totals whose band is drawn), in dollars.ORDER."""
    from prep import build_table as BT
    from shared import dollars as D
    return [v for v in D.ORDER if v in set(BT.PUF_AMOUNTS)]


def preset_slots():
    """The slots a real PUF record presets: its own items, the computed totals and the computed flag (not the filing
    flag, which generate sets)."""
    return [v for v in PUF_SLOTS if v != 'filer'] + list(COMPUTED) + list(FLAGS)


def real_puf_records():
    """The 2015 PUF's records with their bands, their own 2015 amounts and their weights, for filers real_puf.
    'ord': the training table's PUF rows (the PUF less its four aggregate records), every slot as the table holds it
    (age_range -1 where the PUF's demographic file has none, with step 0's probabilities age_p*). 'agg': the four
    aggregate records (top_hidden.RECIDS), which the table leaves out: their amounts translated from the raw fields by
    the table's rules as Part A reads them (top_hidden.puf_items: every one an itemizer, the 10% medical floor), banded
    on the 2015 cut points (top_hidden.reband), the AGI group label from those bands (filing status taken as single for
    the label alone). Their filing status, dependents and age are zero on the PUF (disclosure protection), so those
    slots are drawn by the networks."""
    if _REAL:
        return _REAL
    from shared import dollars as D
    from generate import top_hidden as TH
    own = own_items()
    slots = preset_slots()
    cols = list(dict.fromkeys(['source', 'record_id', 'weight', 'age_range'] + M.AGE_PROBS + M.ES_PROBS
                              + [M.column(v) for v in slots] + own))
    t = pd.read_parquet(common.TABLE, columns=cols)
    o = t[(t.source == 'puf').to_numpy()].reset_index(drop=True)
    del t
    na = o[cols].isna().sum()
    assert not na.any(), f'missing values on PUF rows: {na[na > 0].to_dict()}'
    # (run6, 5 November) the earnings split's two slots (earn_split.py) may be -1 on a PUF row, as in training: the
    # spouse's share of self-employment income where the PUF does not ask it (not joint, no self-employment income, or
    # no Schedule SE earnings), kept absent; the earnings split code on joint returns with no demographics, drawn
    # from step 0's probabilities (real_puf_part), as the age range is
    may_lack = {'age_range', 'earn_split'} | set(M.PUF_SHARES)
    bad = {M.column(v): int((o[M.column(v)] < 0).sum()) for v in slots if v not in may_lack and (o[M.column(v)] < 0).any()}
    assert not bad, f'PUF rows with a slot absent: {bad}'
    if 'earn_split' in slots:
        none = o['earn_split'].to_numpy() < 0
        assert np.allclose(o.loc[none, M.ES_PROBS].sum(1), 1), 'earn_split: a PUF row with no code and no probabilities'
    o = o.rename(columns={v: D.OWN + v for v in own})
    agg_raw, _ = TH.read_raw()
    # alimony received and paid (E00800, E03500) are blank on the four records (disclosure-suppressed; top_hidden
    # SUPPRESSED_FIELDS): read as zero, so each record's AGI is still its E00100 (a choice of the test, 4 November)
    sup = list(TH.SUPPRESSED_FIELDS)
    assert agg_raw[sup].isna().all().all(), 'the suppressed fields are not blank on the aggregate records'
    agg_raw = agg_raw.copy()
    agg_raw[sup] = 0.0
    f = TH.puf_items(agg_raw, itemizer=True)
    na = f.isna().sum()
    assert not na.any(), f'aggregate records: items missing after the translation: {na[na > 0].to_dict()}'
    f['capital_gains'] = f['kg_st'] + f['kg_lt'] + f['kg_dist']       # the table's total: before the loss limit
    for tot, parts in M.META['computed_totals'].items():
        f[tot] = D.signed_sum(f, parts)
    f['other_agi'] = f['other_agi_remainder']                         # the table's other_agi is the remainder
    bands = TH.reband(f)
    a = pd.DataFrame({'record_id': agg_raw['RECID'].astype(float).to_numpy(),
                      'weight': agg_raw['S006'].to_numpy(float) / 100})
    for v in slots:
        if v in M.CATEGORICAL or v in M.PUF_LABELS or v in LATE_SLOTS:   # the late slots are drawn (real_puf_part)
            continue
        a[M.column(v)] = bands[f'{v}_band'].to_numpy()
    for flag, parts in FLAGS.items():
        a[flag] = (f[parts].to_numpy(float) != 0).any(1).astype(np.int64)
    lab = a[[c for c in a.columns if c.endswith('_band')]].assign(filing_status=1)
    a[LABEL] = Q.puf_group(Q.band_agi(lab), Q.puf_groups()['cuts'])
    a = pd.concat([a, pd.DataFrame({D.OWN + v: f[v].to_numpy(float) for v in own})], axis=1)
    _REAL.update({'ord': o, 'agg': a, 'own': own, 'slots': slots})
    common.say(f'real PUF: {len(o):,} ordinary records ({o.weight.sum() / 1e6:,.3f} million returns), '
               f'{len(a)} aggregate records ({a.weight.sum():,.2f} returns); {int((o.age_range < 0).sum()):,} ordinary '
               f'records with no age range (drawn from step 0\'s probabilities); {len(own)} own amounts per record')
    return _REAL


def real_puf_part(model, mod, k, n_models, fac):
    """Model k's (of n_models) copy of the real PUF (see above): the ordinary records with every PUF slot preset (the
    age range, where the PUF has none, one draw per record and copy from step 0's probabilities, as training draws
    it), and the four aggregate records with their amounts preset and their filing status, dependents and (adult)
    age range drawn; then every survey item drawn given the record. Weight: the PUF weight / n_models. Returns the
    part, the label check's table and the rule counts, as label_part does."""
    from shared import dollars as D
    from shared.ages import uniform_by_id
    R = real_puf_records()
    o, a, own, slots = R['ord'], R['agg'], R['own'], R['slots']
    seed = int(G['label_seed']) + mod['seed_shift'] + int(G.get('real_puf_seed_shift', 500))
    rng = o['age_range'].to_numpy().astype(np.int64)
    miss = rng < 0
    u = uniform_by_id(o.loc[miss, 'record_id'].to_numpy(), int(G.get('real_puf_age_salt', 2026110401)) + k)
    cp = np.cumsum(o.loc[miss, M.AGE_PROBS].to_numpy(float), 1)
    rng[miss] = np.minimum((cp < u[:, None]).sum(1), common.N_RANGES - 1)
    preset = {v: torch.as_tensor(o[M.column(v)].to_numpy().astype(np.int64)) for v in slots if v != 'age_range'}
    preset['age_range'] = torch.as_tensor(rng)
    if 'earn_split' in preset:
        # (run6, 5 November) the earnings split code where the PUF has none (joint returns with no demographics): one
        # draw per record and copy from step 0's probabilities (es_p*, earn_split.py), as training draws it each pass
        es = o['earn_split'].to_numpy().astype(np.int64)
        es_miss = es < 0
        ue = uniform_by_id(o.loc[es_miss, 'record_id'].to_numpy(), int(G.get('real_puf_earn_split_salt', 2026110501)) + k)
        ce = np.cumsum(o.loc[es_miss, M.ES_PROBS].to_numpy(float), 1)
        es[es_miss] = np.minimum((ce < ue[:, None]).sum(1), len(M.ES_PROBS) - 1)
        preset['earn_split'] = torch.as_tensor(es)
        common.say(f"{mod['run']}: real PUF copy {k + 1}: earnings split code drawn from step 0's probabilities on "
                   f"{int(es_miss.sum()):,} joint returns with no demographics (codes {np.bincount(es[es_miss], minlength=len(M.ES_PROBS)).tolist()})")
    part, counts = generate(model, len(o), seed, preset=preset, factors=fac, totals_preset=True)
    for v in slots:
        c = M.column(v)
        on = preset[v].numpy() >= 0 if P.index(v) in GATED else np.ones(len(part), bool)   # (run6) -1: drawn
        assert np.array_equal(part[c].to_numpy()[on], preset[v].numpy()[on]), f'real PUF: the draw moved {c}'
    for v in slots:
        if P.index(v) in GATED:
            n_drawn = int((preset[v].numpy() < 0).sum())
            n_set = int((part[M.column(v)].to_numpy()[preset[v].numpy() < 0] > 0).sum())
            common.say(f"{mod['run']}: real PUF copy {k + 1}: {v} not on the PUF for {n_drawn:,} records, which the "
                       f"generator's rule then asks or not; {n_set:,} of them drew a share above the first band")
    part['age_range_drawn'] = miss.astype(np.int64)
    part['puf_aggregate'] = 0
    drawn_a = set(TYPE_SLOTS) | set(STATUS_SLOTS) | set(LATE_SLOTS)   # (run6) the late slots drawn, as the status is
    pre_a = {v: torch.as_tensor(a[M.column(v)].to_numpy().astype(np.int64)) for v in slots if v not in drawn_a}
    part_a, counts_a = generate(model, len(a), seed + 7, preset=pre_a, factors=fac, totals_preset=True,
                                adult_only=True)
    part_a['age_range_drawn'] = 1
    part_a['puf_aggregate'] = 1
    counts = {r: counts[r] + counts_a[r] for r in counts}
    for p, src in [(part, o), (part_a, a)]:
        p['weight'] = src['weight'].to_numpy(float) / n_models
        p['puf_recid'] = src['record_id'].to_numpy().astype(np.int64)
        p['puf_copy'] = k + 1
    part, part_a = [pd.concat([p, src[[D.OWN + v for v in own]].reset_index(drop=True)], axis=1)
                    for p, src in [(part, o), (part_a, a)]]
    part = pd.concat([part, part_a], ignore_index=True)
    g = part[LABEL].to_numpy()
    qg = Q.puf_group(Q.band_agi(part), Q.puf_groups()['cuts'])
    part['q_group'], part['label_ok'] = qg, qg == g
    check = pd.DataFrame([{'model': mod['seed'], 'group': name, 'records drawn': int((g == j).sum()),
                           'band group differs (%)': 100 * float((qg[g == j] != j).mean()) if (g == j).any() else np.nan}
                          for j, name in enumerate(LABEL_META['labels'])])
    common.say(f"{mod['run']}: real PUF copy {k + 1}: {len(part):,} records; the aggregate records were drawn with "
               f"filing status {part_a['filing_status'].tolist()}, age range {part_a['age_range'].tolist()} and "
               f"dependents {part_a['n_dep'].tolist()}")
    return part, check, counts


def store_picks(s, k, stream=4242):
    """The stored picks (see the top): for each record and each amount with a band column, one uniform number in
    [0, 1) (float32) in column u_<variable>, seeded by the dollars seed, the stream (4242 for the stack, 4343 for
    Part A's candidates, top_hidden.py), the model's position k and the variable's position in dollars.ORDER, so the
    same in every offset run."""
    from shared import dollars as D
    seed = int(common.SETTINGS['dollars']['seed'])
    cols = {f'u_{v}': np.random.default_rng([seed, stream, k, i]).random(len(s), dtype=np.float32)
            for i, v in enumerate(D.ORDER) if f'{v}_band' in s}
    return pd.concat([s, pd.DataFrame(cols, index=s.index)], axis=1)


def nonfiler_records():
    """The non-filer batch's number of records (see the top)."""
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


def build_stack(models, factors):
    """The filer batch (the AGI group first) and the non-filer batch, each drawn in equal shares from every model and
    stacked; how many records each rule applied to; and the label check per model and group. factors: {model run: its
    offset factors}."""
    n_nf = nonfiler_records()
    rule_counts = {r['name']: 0 for r in RULES}
    stacks, checks = [], []
    for k, (mod, n_nf_k) in enumerate(zip(models, equal_shares(n_nf, len(models)))):
        model, shift, fac = mod['network'], mod['seed_shift'], factors[mod['run']]
        if REAL_PUF:      # filers real_puf: every 2015 PUF record, its survey items drawn given it (see above)
            part, check, counts = real_puf_part(model, mod, k, len(models), fac)
        else:
            part, check, counts = label_part(model, mod, k, len(models), fac)
        part['weight'] *= filer_records() / len(models) / part['weight'].sum()   # each model: an equal share
        part['stratifier'] = 'label'
        checks.append(check)
        rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
        common.say(f"{mod['run']}: filers done")
        nf_parts, counts = [], {r: 0 for r in rule_counts}
        nf_years = nonfiler_years()
        sizes = [int(round(n_nf_k * s)) for _, s in nf_years]
        sizes[-1] = n_nf_k - sum(sizes[:-1])
        for j, ((ny, _), size) in enumerate(zip(nf_years, sizes)):
            # one year (the target year): the seed as before; several (a year with no CPS units): a seed per year
            part_nf, c = generate(model, size, G['nonfiler_seed'] + shift + 100 * j, factors=fac, label='cps',
                                  year=ny, filer=0)
            if len(nf_years) > 1:
                common.say(f"  non-filers drawn at CPS {ny}: {size:,} records")
            nf_parts.append(part_nf)
            counts = {r: counts[r] + c[r] for r in counts}
        nf = pd.concat(nf_parts, ignore_index=True)
        nf['weight'], nf['stratifier'] = n_nf / len(models) / n_nf_k, 'nonfiler'   # each model: an equal share
        nf['q_group'], nf['label_ok'] = -1, True                                     # no label on a non-filer
        rule_counts = {r: rule_counts[r] + counts[r] for r in rule_counts}
        common.say(f"{mod['run']}: nonfiler done")
        if REAL_PUF:
            nf['puf_recid'], nf['puf_copy'], nf['puf_aggregate'], nf['age_range_drawn'] = -1, -1, 0, 0
        s = pd.concat([part, nf], ignore_index=True)
        s['model'] = mod['seed']
        assert len(s) < ID_BLOCK
        s['record_id'] = k * ID_BLOCK + np.arange(len(s))
        if REAL_PUF:      # a real record's id from its RECID and copy, so the package can key on them
            r = s['puf_recid'].to_numpy() >= 0
            s.loc[r, 'record_id'] = REAL_ID + k * REAL_COPY_BLOCK + s.loc[r, 'puf_recid'].to_numpy()
            assert s['record_id'].is_unique and (s.loc[~r, 'record_id'] < REAL_ID).all()
        stacks.append(store_picks(s, k))
    stack = pd.concat(stacks, ignore_index=True)
    by = stack[['model', 'weight']].assign(batch=np.where(stack['filer'] == 1, 'filers', 'non-filers'))
    share = by.groupby(['batch', 'model'])['weight'].sum() / by.groupby('batch')['weight'].sum()
    common.say('records and share of each batch\'s weight, by model:\n' + pd.DataFrame(
        {'records': by.groupby(['batch', 'model']).size(), 'weight share': share}).to_string())
    assert np.allclose(share.to_numpy(), 1 / len(models)), 'a model does not carry its equal share of a batch'
    w = stack.groupby('scf_wave')['weight'].sum() / stack['weight'].sum()
    common.say('SCF waves of the balance sheet, share of records and of the starting weight against the prior:\n'
               + pd.DataFrame({'records': stack.groupby('scf_wave').size(),
                               'share of records': stack.groupby('scf_wave').size() / len(stack),
                               'share of weight': w,
                               'prior': pd.Series({wv: s for wv, _, s in SCF_WAVES})}).to_string())
    for src, m in SURVEY_YEARS.items():     # the CEX, CPS and SIPP years, records and starting weight against the prior
        cnt, ws = stack.groupby(f'{src}_year').size(), stack.groupby(f'{src}_year')['weight'].sum() / stack['weight'].sum()
        common.say(f'{src.upper()} years, share of records and of the starting weight against the prior:\n'
                   + pd.DataFrame({'records': cnt, 'share of records': cnt / len(stack), 'share of weight': ws,
                                   'prior': pd.Series(dict(m))}).to_string())
    common.say('consistency rules: records each applies to, over the whole stack:')
    common.say(pd.Series(rule_counts).to_string())
    return stack, pd.concat(checks, ignore_index=True)


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
    common.say('survey years: ' + '; '.join(f"{src} " + ', '.join(f'{y} {100 * s:.2f}%' for y, s in m)
                                           for src, m in SURVEY_YEARS.items()))
    common.say('SCF waves of the balance sheet: ' + ', '.join(f'{w} wave (income year {y}) {100 * s:.2f}%'
                                                              for w, y, s in SCF_WAVES))
    factors, report = {m['run']: {} for m in models}, []
    runs = int(C['offset_generations'])
    for run in range(1, runs + 1):
        stack, check = build_stack(models, factors)
        check.insert(0, 'run', run)
        check.to_csv(os.path.join(common.GEN, 'label_check.csv'), index=False)   # the last run's is kept
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

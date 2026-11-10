"""The CPS filing stage: which CPS tax units file a tax return.

The CPS does not ask whether anyone filed a return, so this stage gives every CPS tax unit a chance of filing. It
runs in three steps. Step `units` builds the CPS's tax units by the shared rules, leaving out people in group
quarters. filing_requirement.R then marks whether the law requires each unit to file. Step `calibrate` fits a filing
rule for each year. Under the rule, the log odds of filing (the logarithm of the chance of filing over the chance of
not filing) depend on the head's age band and on having a child under 17. For units required to file they also
depend on gross income and on having business or farm income. The rule has 11 numbers, and they are solved so that
11 targets hold exactly: non-filing adults in 7 age bands, non-filing children, and three facts about required
non-filers from IRS Publication 5785. The step then splits some joint couples into separate returns and writes the
returns, the persons and a set of checks under data/filing/. FEATURES.md, "The CPS", gives the rule and its targets.
"""
import argparse
import os
import sys

import numpy as np
import pandas as pd
from scipy.optimize import least_squares, root

from shared import common
from prep import tax_units as T
from prep import soi as TG

S = common.SETTINGS['filing']
CPS = common.SETTINGS['cps']
DIR = os.path.join(common.DATA, CPS['step1_dir'])
UNITS, REQ, RETURNS, CHECKS = (f'{DIR}/units', f'{DIR}/requirement', os.path.join(DIR, CPS['step1_out']),
                               f'{DIR}/checks')
REPO = common.REPO
SOI = common.RAW['soi_mirror']
PUF = os.path.dirname(common.RAW['puf'])
BANDS = ['18_25', '26_34', '35_44', '45_54', '55_64', '65_74', '75p']
BREAKS = [18, 26, 35, 45, 55, 65, 75, 200]
AGE_LIM = 17                                   # children are those under 17
# Pub 5785 Table 3, averages of 2014-16: units required to file that did not file; those with wages, and their
# wages; those with business or farm income, and that income; and their total income (millions of units, billions of
# dollars). The shares in settings "filing" are computed from these.
PUB5785_T3 = {'units': 11.19, 'wages_n': 6.90, 'wages_bn': 232.1, 'bus_n': 4.99, 'bus_bn': 100.2, 'totinc_bn': 409.4}
WAGE_BINS = [('$0', -np.inf, 0), ('$1-25k', 0, 25e3), ('$25-50k', 25e3, 50e3), ('$50-100k', 50e3, 100e3),
             ('$100-200k', 100e3, 200e3), ('$200k+', 200e3, np.inf)]
# EITC parameters by tax year: the income at which the credit ends for 0, 1, 2 and 3 or more children (unmarried
# filers, then joint filers), the limit on investment income, and the age range for a filer with no children. The
# values are from the Revenue Procedures; 2021 is the year of the American Rescue Plan Act.
EITC = {2015: ([14820, 39131, 44454, 47747], [20330, 44651, 49974, 53267], 3400, (25, 64)),
        2017: ([15010, 39617, 45007, 48340], [20600, 45207, 50597, 53930], 3450, (25, 64)),
        2019: ([15570, 41094, 46703, 50162], [21370, 47440, 52493, 55952], 3600, (25, 64)),
        2021: ([21430, 42158, 47915, 51464], [27380, 48108, 53865, 57414], 10000, (19, 200)),
        2023: ([17640, 46560, 52918, 56838], [24210, 53120, 59478, 63398], 11000, (25, 64))}
# The earned income above which the child credit is refundable: $3,000 in 2015-2017, $2,500 in 2018-2020 and
# 2022-2023 (the default below), and none in 2021
CTC_FLOOR = {2015: 3000, 2016: 3000, 2017: 3000, 2021: 0}
AGES = [('under 5', 0, 5), ('5-12', 5, 13), ('13-16', 13, 17), ('17-18', 17, 19), ('19-23', 19, 24), ('24+', 24, 200)]
# The solver's starting values of the rule's 11 numbers, near each year's solution: the seven age band terms a,
# then r, k, s and b (see filing_chances)
X0 = [2.0, 0.6, 0.2, 0.5, 0.7, 1.0, 0.8, 2.2, 2.3, 1.6, -3.3]


# ---- Small helpers ----

def band_of(age):
    """The age band (0 to 6) of each age; -1 under 18."""
    a = np.asarray(age)
    b = np.searchsorted(BREAKS, a, side='right') - 1
    return np.where(a >= 18, b, -1)


def by_band(values, bands):
    """The sum of `values` within each of the 7 age bands, with 0 for an empty band. Ages under 18 (band -1) are left
    out."""
    return pd.Series(values).groupby(bands).sum().reindex(range(7)).fillna(0).to_numpy()


def expit(z):
    """The chance that corresponds to log odds z."""
    return 1 / (1 + np.exp(-z))


def nacount(df, name):
    """Prints the number of missing values in each column of df that has any, and returns their total."""
    n = df.isna().sum()
    n = n[n > 0]
    if len(n):
        print(f'NA counts in {name}: {n.to_dict()}')
    return int(n.sum())


def eitc_eligible(u, nkids, year):
    """Whether each unit could claim the EITC under the year's law: it has earnings, investment income within the
    limit, income below the point where the credit ends for its number of children, an eligible age if it has no
    children, and a filing status other than married filing separately."""
    unm, jnt, inv_lim, (lo, hi) = EITC[year]
    earned = u.wage + u.selfemp
    agi = u.wage + u.selfemp + u.interest + u.dividends + u.rents + u.est_trust + u.pension + u.uc
    inv = u.interest + u.dividends + np.maximum(u.rents, 0)
    k = np.minimum(nkids, 3).astype(int)
    joint = (u.filing_status == 'joint').to_numpy()
    end = np.where(joint, np.array(jnt)[k], np.array(unm)[k])
    ok_age = lambda a: (a >= lo) & (a <= hi)
    age_ok = (k > 0) | ok_age(u.head_age) | (joint & ok_age(u.spouse_age))
    sep = (u.filing_status == 'separate').to_numpy()
    return ((earned > 0) & (inv <= inv_lim) & (np.maximum(earned, agi) < end) & age_ok & ~sep).to_numpy()


def tag(year):
    """The name tax_units.save_cps gives the year's files."""
    return f'{year}_cbo_nogq'


# ---- Step `units`: the CPS's tax units ----

def build_units(year):
    """The CPS's tax units by the shared rules, with people in group quarters left out."""
    from prep import roster_cps
    r = roster_cps.read_roster(year)
    persons, units = T.build_units(r[~r.gq].reset_index(drop=True), year)
    T.save_cps(persons, units, UNITS, tag(year))


# ---- Step `calibrate`: the filing rule, year by year ----

def calibrate(year):
    """Solves the filing rule for one year, splits some couples into separate returns, and writes the returns, the
    persons and the checks."""
    os.makedirs(RETURNS, exist_ok=True); os.makedirs(CHECKS, exist_ok=True)
    log = []
    say = lambda s: (print(s), log.append(s))
    # 1. Read the units, the people, the filing requirements and the counts of people by age.
    M, P, nd, sd, shape, pep_kids = read_inputs(year)
    # 2. Set the 11 targets, and gather what the rule needs to know about each unit.
    t = rule_inputs(year, M, P, nd, sd, shape, say)
    # 3. Check that each target can be reached, and show what each group of people holds.
    check_targets(year, t, say)
    # 4. Solve the rule's 11 numbers so that the 11 targets hold exactly, and give each unit its filing chance.
    p_cal = solve(year, t, M, say, log)
    # 5. Split some joint couples into separate returns.
    chosen, R = separate_returns(year, M, P, p_cal, t['has_kid'], say)
    # 6. Write the units file and the person file.
    out = write_units_file(year, M, R, chosen)
    pk = write_person_file(year, P, out, chosen, p_cal, sd, t['p_own'])
    # 7. Write the checks and the log.
    write_checks(year, t, M, P, out, sd, shape, pep_kids, p_cal, pk, say)
    open(f'{CHECKS}/{year}_log.txt', 'w').write('\n'.join(log) + '\n')


def read_inputs(year):
    """The year's inputs, checked against each other.

    Returns six tables: the household units with their filing requirement (dependents' own returns left out); the
    persons; the requirement step's household units and its rows for dependents aged 15 and over; the counts by age
    band of the Census population and of SOI's adults on returns; and the Census count of children."""
    U = pd.read_parquet(f'{UNITS}/units_{tag(year)}.parquet')
    P = pd.read_parquet(f'{UNITS}/persons_{tag(year)}.parquet')
    M = U[~U.dep_return].reset_index(drop=True)
    SC = pd.read_csv(f'{REQ}/units_{year}.csv.gz')
    SP = pd.read_csv(f'{REQ}/persons_{year}.csv.gz')
    nd = SC[SC.unit_type == 'nondependent'].set_index('unit_id')
    sd = SC[SC.unit_type == 'dependent'].copy()
    assert set(nd.index) == set(M.uid), 'the requirement step\'s household units and tax_units.py\'s differ'
    # The R step's persons are the same people, in the same units.
    chk = P.merge(SP, left_on=['hh', 'pn'], right_on=['SERIAL', 'PERNUM'], how='outer', indicator=True)
    assert (chk._merge == 'both').all() and (chk.uid == chk.unit_id).all()
    assert not P[['hh', 'pn']].duplicated().any()
    M['must_file'] = nd.must_file.reindex(M.uid).to_numpy().astype(bool)
    assert np.allclose(nd.weight.reindex(M.uid).to_numpy(), M.weight.to_numpy())
    assert nacount(M[['must_file', 'weight', 'head_age']], 'household units') == 0
    shape = pd.read_csv(f'{REPO}/{S["age_shape"].format(year=year)}').set_index('band').reindex(BANDS)
    assert shape[['pep_adults', 'filing_adults']].notna().all().all()
    pep_kids = pd.read_csv(f'{REPO}/{S["pep_children"]}').set_index('year').loc[year]
    return M, P, nd, sd, shape, pep_kids


def rule_inputs(year, M, P, nd, sd, shape, say):
    """The 11 targets, and what the rule needs to know about each unit, as a dictionary.

    The targets are these. Non-filing heads and spouses in each of 7 age bands: the CPS's adults, less SOI's adults
    on returns (Table 1.6), less dependents aged 18 and over who do not file a return of their own. Non-filing
    children under 17: a fixed share of the CPS's children (settings filing.child_share). Among the units required to
    file, three targets from Pub 5785: the share that does not file, their gross income as a share of all required
    units' gross income, and the share of required non-filers with business or farm income."""
    # 1. Index each person by their unit. A head or spouse is counted in the age band of the unit's head.
    uidx = pd.Series(np.arange(len(M)), index=M.uid)
    P['ui'] = uidx.reindex(P.uid).to_numpy()
    assert P.ui.notna().all()
    P['ui'] = P.ui.astype(int)
    head_band = band_of(M.head_age.to_numpy())
    hs = P[P.tu_rel.isin([1, 2])]
    hs_ui, hs_w = hs.ui.to_numpy(), hs.weight.to_numpy()
    hs_b = head_band[hs_ui]

    # 2. The adult targets. A dependent aged 15 or over files a return of their own when required to, or when they
    #    have any wages. Dependents aged 18 and over who do not file are neither filers nor heads or spouses. They
    #    are taken off the CPS's adults along with SOI's adults on returns ("dependent netting"). People in group
    #    quarters are left out of the CPS frame here, and their term (G) is zero.
    w_d = sd.weight.to_numpy()
    p_own = (sd.must_file.astype(bool) | (sd.INCWAGE > 0)).to_numpy().astype(float)
    sd['p_own'] = p_own
    d_band = band_of(sd.age_head.to_numpy())
    dnet = by_band(w_d * (1 - p_own) * (d_band >= 0), d_band)
    pb_all = band_of(P.age.to_numpy())
    cps_adults = by_band(P.weight.to_numpy(), pb_all)
    A = cps_adults - shape.filing_adults.to_numpy(); G = np.zeros(7)
    T_adult = A - G - dnet
    say(f'TY{year}: dependents\' own returns by the rule: {(w_d * p_own).sum() / 1e6:.2f} m file of '
        f'{w_d.sum() / 1e6:.2f} m dependents aged 15+; adult netting {dnet.sum() / 1e6:.2f} m')
    # 3. The reasons a unit has to file: a filing requirement, eligibility for the EITC, or a refundable child
    #    credit. Wages alone are not a reason. Only the requirement enters the rule; the others are reported.
    earned = (M.wage + M.selfemp).to_numpy()
    r_req = M.must_file.to_numpy()
    r_eitc = eitc_eligible(M, M.n_dep_eitc, year)
    r_ctc = (M.n24.to_numpy() > 0) & ((earned > CTC_FLOOR.get(year, 2500)) if year != 2021 else True)
    r_wage = M.wage.to_numpy() > 0
    reason = r_req | r_eitc | r_ctc
    M['reason'] = reason
    say(f'reason to file: {(M.weight * reason).sum() / 1e6:.2f} m of {M.weight.sum() / 1e6:.2f} m units (required '
        f'{(M.weight * r_req).sum() / 1e6:.2f}, EITC {(M.weight * r_eitc).sum() / 1e6:.2f}, refundable child credit '
        f'{(M.weight * r_ctc).sum() / 1e6:.2f}; with wages, not a reason, {(M.weight * r_wage).sum() / 1e6:.2f})')

    # 4. The children's target, and which units have a child under 17.
    T_child = S['child_share'] * P.weight[P.age < AGE_LIM].sum()
    say(f'children target: {S["child_share"]:.6f} x CPS children under {AGE_LIM} '
        f'{P.weight[P.age < AGE_LIM].sum() / 1e6:.3f} m = {T_child / 1e6:.3f} m non-filing')
    kid = P.age < AGE_LIM
    k_ui, k_w = P.ui[kid].to_numpy(), P.weight[kid].to_numpy()
    has_kid = np.zeros(len(M), bool); has_kid[np.unique(k_ui)] = True

    # 5. The income and business terms of required units. The income term is the logarithm of gross income relative
    #    to the weighted median among required units. Gross income below the smallest filing threshold is raised to
    #    that threshold before the logarithm is taken.
    gi = M.gi.to_numpy()
    assert not np.isnan(gi).any()
    GI_FLOOR = float(nd.threshold.min())
    assert GI_FLOOR > 0 and nd.threshold.notna().all()
    wr, gr = M.weight.to_numpy()[r_req], gi[r_req]
    o = np.argsort(gr, kind='stable'); cw = np.cumsum(wr[o])
    gi_med = float(gr[o][np.searchsorted(cw, cw[-1] / 2)])
    assert gi_med > GI_FLOOR > 0
    zinc = np.where(r_req, np.log(np.maximum(gi, GI_FLOOR)) - np.log(gi_med), 0.0)
    assert not M.selfemp.isna().any()
    has_bus = (M.selfemp.to_numpy() != 0)
    bus_req = has_bus[r_req]
    # 6. The three targets from Pub 5785. The shares in the settings are Pub 5785's figures divided by the 2015 CPS's
    #    required units; in 2015 that is checked here.
    RATE, SHARE, BSHARE = S['required_nonfiling_share'], S['income_share'], S['business_share']
    INC_REQ = float((wr * gr).sum())
    T_inc = SHARE * INC_REQ
    if year == 2015:
        for name, got, want in [('required_nonfiling_share', PUB5785_T3['units'] * 1e6 / wr.sum(), RATE),
                                ('income_share', PUB5785_T3['totinc_bn'] * 1e9 / INC_REQ, SHARE),
                                ('business_share', PUB5785_T3['bus_n'] / PUB5785_T3['units'], BSHARE)]:
            assert abs(got / want - 1) < 1e-12, f'settings filing.{name} {want} is not Pub 5785 / 2015 CPS ({got})'
    say(f'business term: required units with business or farm income {wr[bus_req].sum() / 1e6:.2f} m of '
        f'{wr.sum() / 1e6:.2f} m; target {BSHARE:.4f} of required non-filers = {BSHARE * RATE * wr.sum() / 1e6:.2f} m')
    say(f'income term: required units {wr.sum() / 1e6:.2f} m, their gross income {INC_REQ / 1e9:.1f} bn, weighted '
        f'median {gi_med:,.0f}; log floored at {GI_FLOOR:,.0f} ({wr[gr < GI_FLOOR].sum() / 1e6:.3f} m required units '
        f'below it, {wr[gr <= 0].sum() / 1e6:.3f} m at zero or less). Income share target {SHARE:.5f} -> '
        f'{T_inc / 1e9:.1f} bn this year')
    wq = M.weight.to_numpy()[r_req]
    return dict(head_band=head_band, hs_ui=hs_ui, hs_w=hs_w, hs_b=hs_b, w_d=w_d, p_own=p_own, dnet=dnet,
                cps_adults=cps_adults, A=A, G=G, T_adult=T_adult, r_req=r_req, reason=reason, T_child=T_child,
                k_ui=k_ui, k_w=k_w, has_kid=has_kid, gi=gi, gi_med=gi_med, zinc=zinc, has_bus=has_bus,
                bus_req=bus_req, RATE=RATE, BSHARE=BSHARE, T_inc=T_inc, wq=wq, gr=gr)


def filing_chances(x, t):
    """Each unit's chance of filing under the rule with numbers x.

    The log odds of filing are a (by the head's age band; a head under 18 takes the 18-25 band's) + k x (a child
    under 17) + required x (r + s x the income term + b x any business or farm income). x holds the seven a's, then
    r, k, s and b. t is the dictionary from rule_inputs."""
    a = x[:7][np.maximum(t['head_band'], 0)]
    return expit(a + x[8] * t['has_kid'] + t['r_req'] * (x[7] + x[9] * t['zinc'] + x[10] * t['has_bus']))


def target_gaps(x, t):
    """What the rule with numbers x gives, less the 11 targets. The gaps in people and units are in millions, and the
    gap in income is in billions of dollars."""
    p = filing_chances(x, t)
    wq, r_req = t['wq'], t['r_req']
    r = by_band(t['hs_w'] * (1 - p[t['hs_ui']]), t['hs_b']) - t['T_adult']
    r = np.append(r, (t['k_w'] * (1 - p[t['k_ui']])).sum() - t['T_child'])
    r = np.append(r, (wq * (1 - p[r_req])).sum() - t['RATE'] * wq.sum())
    q_req = wq * (1 - p[r_req])
    r = np.append(r, (q_req * t['bus_req']).sum() - t['BSHARE'] * q_req.sum())
    r = r / 1e6
    return np.append(r, ((wq * (1 - p[r_req]) * t['gr']).sum() - t['T_inc']) / 1e9)


def check_targets(year, t, say):
    """Checks that every target lies between "every unit files" and "no unit files", and stops if one does not.
    Writes the check, and a table of the targets beside what each group of people holds."""
    hs_w, hs_ui, hs_b, k_w, k_ui = t['hs_w'], t['hs_ui'], t['hs_b'], t['k_w'], t['k_ui']
    T_adult, T_child, reason, r_req = t['T_adult'], t['T_child'], t['reason'], t['r_req']
    feas = pd.DataFrame({'band': BANDS, 'target': T_adult, 'min_possible': np.zeros(7), 'max_possible': by_band(hs_w, hs_b)})
    feas.loc[len(feas)] = ['children', T_child, 0.0, k_w.sum()]
    feas['feasible'] = (feas.target >= feas.min_possible) & (feas.target <= feas.max_possible)
    feas.to_csv(f'{CHECKS}/{year}_feasibility.csv', index=False)
    if not feas.feasible.all():
        say('INFEASIBLE targets:\n' + feas[~feas.feasible].to_string())
        raise SystemExit(f'TY{year}: a target is outside what the rule can reach; stopped')
    nr = ~reason
    diag = pd.DataFrame({'band': BANDS, 'target_nonfiling': T_adult / 1e6,
                         'heads_spouses_no_reason': by_band(hs_w * nr[hs_ui], hs_b) / 1e6,
                         'heads_spouses_reason': by_band(hs_w * reason[hs_ui], hs_b) / 1e6})
    diag['heads_spouses_required'] = by_band(hs_w * r_req[hs_ui], hs_b) / 1e6
    diag['heads_spouses_not_required'] = by_band(hs_w * ~r_req[hs_ui], hs_b) / 1e6
    diag.loc[len(diag)] = ['children under 17', T_child / 1e6, (k_w * nr[k_ui]).sum() / 1e6, (k_w * reason[k_ui]).sum() / 1e6,
                           (k_w * r_req[k_ui]).sum() / 1e6, (k_w * ~r_req[k_ui]).sum() / 1e6]
    diag.to_csv(f'{CHECKS}/{year}_groups_vs_targets.csv', index=False)
    say('--- targets against what each group holds (millions)\n' + diag.round(3).to_string())


def solve(year, t, M, say, log):
    """Solves the rule's 11 numbers so that the 11 targets hold exactly, and returns each unit's filing chance.

    A root finder searches for the numbers at which all 11 gaps (target_gaps) are zero. It tries Powell's hybrid
    method, then the Levenberg-Marquardt method if the first fails. If neither brings every gap within 1e-6, the
    closest fit by least squares is written and the run stops. Writes the gaps at the solution, and the numbers
    (each a shift in the log odds)."""
    x0 = np.array(X0, dtype=float)
    resid = lambda x: target_gaps(x, t)
    sol = root(resid, x0, method='hybr', tol=1e-12)
    if not sol.success or np.abs(resid(sol.x)).max() > 1e-6:
        sol = root(resid, x0, method='lm', tol=1e-14)
    names = BANDS + ['children under 17', 'non-filing units, required to file',
                     'non-filers with business or farm income, required to file',
                     'gross income of non-filers, required to file (billions)']
    r_req = t['r_req']
    tgt = np.append(np.append(t['T_adult'], t['T_child']), [t['RATE'] * M.weight[r_req].sum(),
                                                            t['BSHARE'] * t['RATE'] * M.weight[r_req].sum()]) / 1e6
    tgt = np.append(tgt, t['T_inc'] / 1e9)
    params = ['a ' + b for b in BANDS] + ['r (required to file)', 'k (child under 17)', 's (income slope, required)',
                                          'b (business or farm income, required)']
    if np.abs(resid(sol.x)).max() > 1e-6:
        ls = least_squares(resid, np.clip(sol.x, -29, 29) if np.all(np.isfinite(sol.x)) else x0, bounds=(-30, 30),
                           xtol=1e-12, ftol=1e-12, gtol=1e-12, max_nfev=5000)
        fit = pd.DataFrame({'item': names, 'target': tgt, 'achieved_minus_target': resid(ls.x), 'param': params,
                            'param_value': ls.x})
        fit.to_csv(f'{CHECKS}/{year}_fit.csv', index=False)
        say('NO EXACT SOLUTION; least-squares best (millions; income in billions; achieved minus target):\n'
            + fit.round(3).to_string())
        open(f'{CHECKS}/{year}_log.txt', 'w').write('\n'.join(log) + '\n')
        raise SystemExit(f'TY{year}: the rule cannot hit every target; stopped')
    pd.DataFrame({'item': names, 'target': tgt, 'achieved_minus_target': resid(sol.x)}).to_csv(
        f'{CHECKS}/{year}_fit.csv', index=False)
    x = sol.x
    M['p_cal'] = filing_chances(x, t)
    p_cal = M.p_cal.to_numpy()
    shifts = pd.DataFrame({'shift': params, 'logit_shift': x})
    shifts.loc[len(shifts)] = ['weighted median gross income, required (dollars, not a parameter)', t['gi_med']]
    shifts.to_csv(f'{CHECKS}/{year}_shifts.csv', index=False)
    say('the rule\'s settings (logit): ' + ', '.join(f'{a} {b:+.3f}' for a, b in zip(shifts['shift'], x)))
    return p_cal


def separate_returns(year, M, P, p_cal, has_kid, say):
    """Splits joint couples into married-filing-separately returns. Couples are taken in the order of a draw keyed
    by unit id until their weight times filing chance reaches half of SOI Table 1.6's separate returns. Returns the
    unit ids of the couples split, and the new returns."""
    M['has_kid_target_age'] = has_kid
    mfs_target = sum(TG.table_1_6(year)[('separate', k)] for k in range(6))
    chosen, share, R = T.mfs_split(M, P, None, mfs_target, salt=S['mfs_salt'], chance=p_cal)
    say(f'MFS: SOI Table 1.6 {mfs_target / 1e6:.3f} m separate returns; {len(chosen)} sample couples '
        f'({share:.4f} of joint couples by weight x chance); expected separate returns '
        f'{(R.weight * R.p_file).sum() / 1e6:.3f} m')
    # Each new return takes the couple's filing chance, requirement and reasons. Its id is in a block of its own.
    R = R.rename(columns={'p_file': 'p_file_cal'})
    R['couple_uid'] = R.couple_uid.astype(np.int64)
    R['uid'] = 2 * 10 ** 12 + R.couple_uid * 10 + R.person
    cu = M.set_index('uid')
    R['must_file'] = cu.must_file.reindex(R.couple_uid).to_numpy()
    for c in ['spouse_pn', 'spouse_age']:
        R[c] = -1
    R['spouse_sex'] = 0
    R['puf_child_away'] = 0
    R['has_kid_target_age'] = cu.has_kid_target_age.reindex(R.couple_uid).to_numpy()
    R['reason'] = cu.reason.reindex(R.couple_uid).to_numpy()
    return chosen, R


def write_units_file(year, M, R, chosen):
    """Writes the units file: every household unit, with the separate returns in place of the couples they split,
    each with its filing chance (p_file_cal) and EITC eligibility."""
    M['couple_uid'] = -1
    M['person'] = 0
    M['p_file_cal'] = M.p_cal
    keep = ['uid', 'couple_uid', 'person', 'hh', 'head_pn', 'spouse_pn', 'weight', 'p_file_cal',
            'must_file', 'filing_status', 'head_age', 'head_sex', 'spouse_age', 'spouse_sex', 'n_dep', 'puf_n_dep',
            'puf_child_home', 'puf_child_away', 'puf_other', 'puf_parent', 'dep_age1', 'dep_age2', 'dep_age3', 'n24',
            'n_dep_eitc', 'eic', 'has_kid_target_age', 'reason'] + T.INCOME_ITEMS
    out = pd.concat([M[~M.uid.isin(chosen)][keep], R[keep]], ignore_index=True)
    out['eitc_eligible'] = eitc_eligible(out, out.n_dep_eitc, year)
    out['year'] = year
    assert not out.uid.duplicated().any()
    assert nacount(out, 'units file') == 0
    out.to_parquet(f'{RETURNS}/units_{year}.parquet')
    out.to_csv(f'{RETURNS}/units_{year}.csv.gz', index=False)
    return out


def write_person_file(year, P, out, chosen, p_cal, sd, p_own):
    """Writes the person file: each person with the return they are on (return_uid), that return's filing chance,
    and, for a dependent aged 15 or over, whether they file a return of their own (p_own_return, -1 for others).
    On a couple split into separate returns, person 1 and the dependents are on person 1's return. Returns the
    columns written."""
    P['return_uid'] = P.uid
    pr1, pr2 = T.primary_and_spouse(P, chosen)
    ins = P.uid.isin(chosen)
    one = pd.Series(2 * 10 ** 12 + pr1.index.to_numpy() * 10 + 1, index=pr1.index)
    P.loc[ins, 'return_uid'] = one.reindex(P.uid[ins]).to_numpy()
    key2 = set(zip(pr2.hh, pr2.pn))
    is2 = np.array([(h, n) in key2 for h, n in zip(P.hh, P.pn)])
    P.loc[is2, 'return_uid'] = 2 * 10 ** 12 + P.uid[is2] * 10 + 2
    assert P.return_uid.isin(out.uid).all(), 'a person whose return is not in the units file'
    P['p_file_cal'] = p_cal[P.ui]
    po = pd.Series(p_own, index=pd.MultiIndex.from_arrays([sd.SERIAL, sd.head_pernum]))
    P['p_own_return'] = po.reindex(pd.MultiIndex.from_arrays([P.hh, P.pn])).fillna(-1).to_numpy()
    pk = ['year', 'hh', 'pn', 'age', 'sex', 'weight', 'tu_rel', 'dep_type', 'uid', 'return_uid',
          'p_file_cal', 'p_own_return', 'QC', 'QR'] + T.INCOME_ITEMS
    assert nacount(P[pk], 'person file') == 0
    P[pk].to_parquet(f'{RETURNS}/persons_{year}.parquet')
    return pk


# ---- The checks ----

def write_checks(year, t, M, P, out, sd, shape, pep_kids, p_cal, pk, say):
    """Writes the year's checks under data/filing/checks/ and prints them."""
    hs_w, hs_ui, hs_b, k_w, k_ui = t['hs_w'], t['hs_ui'], t['hs_b'], t['k_w'], t['k_ui']
    w_d, p_own, reason, r_req, has_kid, gi = t['w_d'], t['p_own'], t['reason'], t['r_req'], t['has_kid'], t['gi']
    # 1. Every person appears once, in a unit with a head, on a return in the units file, and no file has missing
    #    values.
    c1 = pd.DataFrame([
        ('people', len(P), P.weight.sum() / 1e6),
        ('people in more than one unit', int(P[['hh', 'pn']].duplicated().sum()), 0),
        ('people whose unit has no head', int((~P.uid.isin(M.uid)).sum()), 0),
        ('units with no head or two heads', int((P[P.tu_rel == 1].groupby('uid').size() != 1).sum()
                                                + len(set(M.uid) - set(P.uid[P.tu_rel == 1]))), 0),
        ('people whose return is missing', int((~P.return_uid.isin(out.uid)).sum()), 0),
        ('NA cells in the units file', int(out.isna().sum().sum()), 0),
        ('NA cells in the person file', int(P[pk].isna().sum().sum()), 0),
    ], columns=['check', 'count', 'weighted_millions'])
    c1.to_csv(f'{CHECKS}/{year}_persons_once.csv', index=False)

    # 2. The CPS's people by age band against the Census population estimates. The estimates include people in
    #    group quarters, whom the CPS leaves out.
    pb = band_of(P.age.to_numpy())
    rows = [(b, P.weight[pb == i].sum() / 1e6, shape.pep_adults[b] / 1e6) for i, b in enumerate(BANDS)]
    rows.append((f'under {AGE_LIM}', P.weight[P.age < AGE_LIM].sum() / 1e6, pep_kids[f'pep_u{AGE_LIM}'] / 1e6))
    c2 = pd.DataFrame(rows, columns=['age', 'cps_people', 'census_pep'])
    c2['difference'] = c2.cps_people - c2.census_pep
    c2.to_csv(f'{CHECKS}/{year}_people_by_age.csv', index=False)

    # 3. Non-filing adults and children against their targets. The column census_adults holds the CPS's adults, on
    #    which the targets are built. The children's row holds the Census population under 17. The reweighting
    #    stage reads this file for non-filers by age.
    after = by_band(hs_w * (1 - p_cal[hs_ui]), hs_b)
    c4 = pd.DataFrame({'band': BANDS, 'census_adults': t['cps_adults'], 'adults_on_returns_T16': shape.filing_adults.to_numpy(),
                       'anchor': t['A'], 'gq_nonfilers': t['G'], 'dependent_netting': t['dnet'],
                       'target_heads_spouses': t['T_adult'], 'after': after})
    c4.loc[len(c4)] = [f'children under {AGE_LIM}', pep_kids[f'pep_u{AGE_LIM}'], np.nan, np.nan, np.nan, np.nan,
                       t['T_child'], (k_w * (1 - p_cal[k_ui])).sum()]
    c4['after_minus_target'] = c4.after - c4.target_heads_spouses
    for c in c4.columns[1:]:
        c4[c] = c4[c] / 1e6
    c4.to_csv(f'{CHECKS}/{year}_nonfilers_vs_targets.csv', index=False)
    # The partition of the CPS's adults: adults on returns + non-filing heads and spouses + non-filers in group
    # quarters (zero) + dependent netting. Its largest gap across the bands is printed.
    part = (c4.iloc[:7].adults_on_returns_T16 + c4.iloc[:7].after + c4.iloc[:7].gq_nonfilers
            + c4.iloc[:7].dependent_netting - c4.iloc[:7].census_adults).abs().max()
    say(f'partition (CPS adults = adults on returns + non-filing heads and spouses + group quarters non-filers + '
        f'dependent netting): largest gap {part * 1e6:.3f} people')

    # 4. Expected filers and non-filers against SOI (and against the PUF in 2015).
    ttab = tables(year, out, P, sd)
    ttab.to_csv(f'{CHECKS}/{year}_filers_nonfilers.csv')

    # 5. Non-filing people with wages against Pub 5785.
    hsw = P.tu_rel.isin([1, 2]) & (P.wage > 0)
    hh_earn = (P.weight * (1 - P.p_file_cal))[hsw].sum()
    dep_earn = (w_d * (1 - p_own))[(sd.INCWAGE > 0).to_numpy()].sum()
    u15 = (P.tu_rel.isin([3, 4]) & (P.age < 15) & (P.wage > 0)).sum()
    c5 = pd.DataFrame([('heads and spouses with wages in non-filing units', hh_earn / 1e6),
                       ('dependents with wages not filing their own return', dep_earn / 1e6),
                       ('total non-filers with wages', (hh_earn + dep_earn) / 1e6),
                       ('Pub 5785 TY2015: people with an information return and no 1040, with wages', 15.77),
                       ('Pub 5785 TY2015: people with an information return and no 1040', 50.16)],
                      columns=['item', 'millions'])
    c5.to_csv(f'{CHECKS}/{year}_pub5785_wages.csv', index=False)
    say(f'Pub 5785 check: {(hh_earn + dep_earn) / 1e6:.2f} m non-filers with wages ({u15} sample dependents under 15 '
        f'with wages have no own-return row)')

    # 6. Units and children by group: whether the unit has a reason to file, whether it is required to, and whether
    #    it has a child under 17.
    wn = M.weight.to_numpy() * (1 - p_cal)
    grp = pd.DataFrame({'reason': reason, 'required_to_file': r_req, 'units': M.weight.to_numpy() / 1e6,
                        'nonfiling_units': wn / 1e6, 'filing_units': M.weight.to_numpy() * p_cal / 1e6})
    grp['child_under_17'] = has_kid
    grp = grp.groupby(['reason', 'required_to_file', 'child_under_17'])[['units', 'filing_units', 'nonfiling_units']].sum()
    kt = pd.DataFrame({'status': M.filing_status.to_numpy()[k_ui], 'reason': reason[k_ui], 'required': r_req[k_ui],
                       'children': k_w / 1e6, 'nonfiling': k_w * (1 - p_cal[k_ui]) / 1e6})
    kt = kt.groupby(['status', 'reason', 'required'])[['children', 'nonfiling']].sum()
    kt.to_csv(f'{CHECKS}/{year}_children_by_unit.csv')
    say('--- children under 17 by unit (millions)\n' + kt.round(3).to_string())
    grp['chance'] = grp.filing_units / grp.units
    grp.to_csv(f'{CHECKS}/{year}_units_by_group.csv')
    say('--- units by group (millions)\n' + grp.round(3).to_string())
    say(f'household units required to file and not filing: {(M.weight * (1 - p_cal))[r_req].sum() / 1e6:.3f} m; '
        f'share of household units filing: {(M.weight * p_cal).sum() / M.weight.sum():.4f} (by weight; separate '
        f'returns not yet split)')
    pd.set_option('display.width', 250)
    # 7. Required non-filers against Pub 5785 Table 3.
    q = out.weight.to_numpy() * (1 - out.p_file_cal.to_numpy()); rq = out.must_file.to_numpy().astype(bool)
    wg, se, g = out.wage.to_numpy(), out.selfemp.to_numpy(), out.gi.to_numpy()
    t3 = PUB5785_T3
    c6 = pd.DataFrame([
        {'rule': 'v4', 'units_m': q[rq].sum() / 1e6, 'gross_income_bn': (q * g)[rq].sum() / 1e9,
         'with_wages_m': q[rq & (wg > 0)].sum() / 1e6, 'wages_bn': (q * wg)[rq].sum() / 1e9,
         'with_business_farm_m': q[rq & (se != 0)].sum() / 1e6, 'business_farm_bn': (q * se)[rq].sum() / 1e9,
         'all_nonfilers_m': q.sum() / 1e6, 'all_nonfiler_wages_bn': (q * wg).sum() / 1e9},
        {'rule': 'Pub 5785 TY2014-16 average', 'units_m': t3['units'], 'gross_income_bn': t3['totinc_bn'],
         'with_wages_m': t3['wages_n'], 'wages_bn': t3['wages_bn'], 'with_business_farm_m': t3['bus_n'],
         'business_farm_bn': t3['bus_bn'], 'all_nonfilers_m': np.nan, 'all_nonfiler_wages_bn': np.nan}])
    c6.to_csv(f'{CHECKS}/{year}_pub5785_required.csv', index=False)
    say('--- required-to-file non-filing returns (household returns, separate returns counted singly; millions, '
        'billions of the tax year) against Pub 5785 Table 3, whose figures are TY2014-16 averages and include '
        'dependents\' own returns\n' + c6.round(2).to_string())
    # 8. Non-filers by the wages on the return.
    rows7 = []
    for name, lo, hi in WAGE_BINS:
        m = (wg <= 0) if name == '$0' else (wg > lo) & (wg <= hi)
        rows7.append((name, q[m].sum() / 1e6, (q * wg)[m].sum() / 1e9))
    rows7.append(('total', q.sum() / 1e6, (q * wg).sum() / 1e9))
    c7 = pd.DataFrame(rows7, columns=['wages', 'nonfilers_m, v4', 'wages_bn, v4']).set_index('wages')
    c7.to_csv(f'{CHECKS}/{year}_nonfilers_by_wages.csv')
    say('--- non-filing returns by the return\'s wages (head and spouse; "$0" includes negative), millions and '
        'billions\n' + c7.round(2).to_string())
    # 9. The non-filing rate of required units by gross income.
    gb = pd.cut(gi, [-np.inf, 0, 10e3, 25e3, 50e3, 100e3, 200e3, 500e3, np.inf])
    c8 = pd.DataFrame({'gi': gb[r_req], 'w': M.weight.to_numpy()[r_req], 'q': M.weight.to_numpy()[r_req] *
                       (1 - p_cal[r_req])}).groupby('gi', observed=False)[['w', 'q']].sum()
    c8['nonfiling_rate'] = c8.q / c8.w
    c8 = c8.rename(columns={'w': 'required_units', 'q': 'nonfiling'})
    c8[['required_units', 'nonfiling']] /= 1e6
    c8.to_csv(f'{CHECKS}/{year}_required_by_income.csv')
    say('--- required household units by gross income (before the separate-return split): millions, non-filing '
        'rate\n' + c8.round(4).to_string())
    # 10. Print the checks written above.
    for name, tb in [('persons once', c1), ('people by age', c2), ('non-filers vs targets', c4)]:
        say(f'--- {name}\n' + tb.round(3).to_string())
    say('--- filers and non-filers\n' + ttab.round(2).to_string())
    say('--- Pub 5785\n' + c5.round(2).to_string())


def rows_for(d, p):
    """Rows of the filers and non-filers table for returns d with filing chance p: expected filers (weight x p) and
    expected non-filers (weight x (1 - p)), in millions."""
    res = []
    for wt in [d.weight.to_numpy() * p / 1e6, d.weight.to_numpy() * (1 - p) / 1e6]:
        s = d.filing_status.to_numpy()
        r = {'units, all': wt.sum(), 'units, single': wt[s == 'single'].sum(), 'units, joint': wt[s == 'joint'].sum(),
             'units, separate': wt[s == 'separate'].sum(), 'units, head of household': wt[s == 'hoh'].sum()}
        r['dependents (PUF caps)'] = (wt * d.puf_n_dep).sum()
        r['  children at home'] = (wt * d.puf_child_home).sum()
        r['  children away'] = (wt * d.puf_child_away).sum()
        r['  other dependents'] = (wt * d.puf_other).sum()
        r['  parents'] = (wt * d.puf_parent).sum()
        a = d[['dep_age1', 'dep_age2', 'dep_age3']].to_numpy()
        for lab, lo, hi in AGES:
            r[f'first three dependents, {lab}'] = (wt[:, None] * ((a >= lo) & (a < hi))).sum()
        r['child credit children (under 17, qualifying)'] = (wt * d.n24).sum()
        el = d.eitc_eligible.to_numpy(); k = np.minimum(d.n_dep_eitc.to_numpy(), 3)
        for i, lab in enumerate(['0', '1', '2', '3+']):
            r[f'EITC returns, {lab} children'] = wt[el & (k == i)].sum()
        r['EITC children (up to 3, on EITC returns)'] = (wt * k * el).sum()
        res.append(r)
    return res


def soi_column(year):
    """SOI's counts for the filers and non-filers table, in millions: returns by filing status (Table 1.6), EITC
    returns by number of children, dependent filers, and, in 2015 and 2017, exemptions by type of dependent."""
    t16 = TG.table_1_6(year)
    s = {f'units, {a}': sum(t16[(b, k)] for k in range(6)) / 1e6 for a, b in
         [('single', 'single'), ('joint', 'joint'), ('separate', 'separate'), ('head of household', 'head_of_household')]}
    s['units, all'] = sum(s.values())
    e = pd.read_csv(f'{SOI}/bysize_eitc.csv', dtype=str)
    e = e[(e.tax_year == str(year)) & (e.row_label == 'Total')]
    for lab, pat in [('0', 'no qualifying children'), ('1', 'one qualifying child'), ('2', 'two qualifying children'),
                     ('3+', 'three or more qualifying children')]:
        v = e[e['item'] == f'returns with {pat} > number of returns'].value.astype(float)
        s[f'EITC returns, {lab} children'] = v.iloc[0] / 1e6 if len(v) else np.nan
    d = pd.read_csv(f'{SOI}/bysize_dependent_returns.csv', dtype=str)
    d = d[(d.tax_year == str(year)) & d.row_label.str.startswith('All returns') & (d.col_seq == '1')
          & (d['item'] == 'number of returns')]
    s['dependent filers'] = d.value.astype(float).iloc[0] / 1e6 if len(d) else np.nan
    # Table 1.6 counts dependents' own returns, nearly all as single. They are taken off single returns and off all
    # returns. The CPS rows count a dependent only in the unit that claims them.
    if not np.isnan(s['dependent filers']):
        s['units, single'] -= s['dependent filers']; s['units, all'] -= s['dependent filers']
    if year in (2015, 2017):
        x = pd.read_csv(f'{SOI}/bysize_exemptions.csv', dtype=str)
        x = x[(x.tax_year == str(year)) & x.row_label.str.contains('total', case=False)]
        for lab, pat in [('  children at home', 'exemptions for children at home > number of exemptions'),
                         ('  children away', 'exemptions for children away from home > number of exemptions'),
                         ('  other dependents', 'exemptions for other dependents > number of exemptions'),
                         ('  parents', 'exemptions for parents > number of exemptions')]:
            v = x[x['item'] == pat].value.astype(float)
            s[lab] = v.iloc[0] / 1e6 if len(v) else np.nan
        s['dependents (PUF caps)'] = sum(s[k] for k in ['  children at home', '  children away', '  other dependents',
                                                         '  parents'])   # all dependents; SOI's count has no caps
    return s


def puf_column():
    """The 2015 PUF's counts for the filers and non-filers table, in millions."""
    p = pd.read_csv(common.RAW['puf'], usecols=['RECID', 'S006', 'MARS', 'DSI', 'XOCAH', 'XOCAWH', 'XOODEP', 'XOPAR',
                                                'N24', 'EIC', 'E59680', 'E59700', 'E59720'])
    dm = pd.read_csv(f'{PUF}/demographics_2015.csv')
    p = p.merge(dm, on='RECID', how='left')
    w = p.S006.to_numpy() / 100 / 1e6
    nd = (p.DSI == 0).to_numpy()
    r = {'units, single': w[nd & (p.MARS == 1)].sum(), 'units, joint': w[nd & p.MARS.isin([2, 5])].sum(),
         'units, separate': w[nd & (p.MARS == 3)].sum(), 'units, head of household': w[nd & (p.MARS == 4)].sum(),
         'dependent filers': w[~nd].sum()}
    r['units, all'] = w[nd].sum()
    n = p[['XOCAH', 'XOCAWH', 'XOODEP', 'XOPAR']].sum(axis=1).to_numpy()
    r['dependents (PUF caps)'] = (w * n).sum()
    for lab, c in [('  children at home', 'XOCAH'), ('  children away', 'XOCAWH'), ('  other dependents', 'XOODEP'),
                   ('  parents', 'XOPAR')]:
        r[lab] = (w * p[c]).sum()
    # The PUF's dependent ages are codes 1-6 for the ranges in AGES, and 0 where there is no dependent or the age is
    # withheld.
    a = p[['AGEDP1', 'AGEDP2', 'AGEDP3']].fillna(0).to_numpy()
    for i, (lab, lo, hi) in enumerate(AGES):
        r[f'first three dependents, {lab}'] = (w[:, None] * (a == i + 1)).sum()
    slots = np.minimum(n, 3)[:, None] > np.arange(3)[None, :]
    r['first three dependents, age withheld'] = (w[:, None] * (slots & (a == 0))).sum()
    r['child credit children (under 17, qualifying)'] = (w * p.N24)[nd].sum()
    r['people, under 17'] = r['child credit children (under 17, qualifying)']
    eitc = ((p.E59680 + p.E59700 + p.E59720) > 0).to_numpy()
    for i, lab in enumerate(['0', '1', '2', '3+']):
        r[f'EITC returns, {lab} children'] = w[eitc & (np.minimum(p.EIC, 3).to_numpy() == i)].sum()
    r['EITC children (up to 3, on EITC returns)'] = (w * p.EIC.to_numpy() * eitc).sum()
    return r


def tables(year, out, P, sd):
    """The filers and non-filers table: expected filers and non-filers among the returns and among people, beside
    SOI's counts and, in 2015, the PUF's."""
    f_after, n_after = rows_for(out, out.p_file_cal.to_numpy())
    f_after['dependent filers'] = (sd.weight * sd.p_own).sum() / 1e6
    pf = P['p_file_cal'].to_numpy(); w = P.weight.to_numpy() / 1e6
    ad = (P.age >= 18).to_numpy(); kd = (P.age < AGE_LIM).to_numpy()
    f_after['people, all'] = (w * pf).sum(); n_after['people, all'] = (w * (1 - pf)).sum()
    f_after['people, adults'] = (w * pf)[ad].sum(); n_after['people, adults'] = (w * (1 - pf))[ad].sum()
    f_after[f'people, under {AGE_LIM}'] = (w * pf)[kd].sum(); n_after[f'people, under {AGE_LIM}'] = (w * (1 - pf))[kd].sum()
    cols = {'new, filers': f_after, 'new, non-filers': n_after, 'SOI': soi_column(year)}
    if year == 2015:
        cols['PUF'] = puf_column()
    t = pd.DataFrame(cols)
    order = list(f_after.keys())
    order = order[:5] + ['dependent filers'] + [k for k in order[5:] if k != 'dependent filers']
    extra = [k for k in t.index if k not in order]
    return t.loc[order + extra]


# ---- The whole stage ----

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('step', choices=['units', 'calibrate'])
    ap.add_argument('years', type=int, nargs='*')
    a = ap.parse_args()
    years = a.years or [int(y) for y in CPS['years']]
    assert all(y in CPS['years'] for y in years), f'years outside settings cps.years: {years}'
    for y in years:
        print(f'=== {a.step} {y}')
        sys.stdout.flush()
        (build_units if a.step == 'units' else calibrate)(y)


if __name__ == '__main__':
    main()

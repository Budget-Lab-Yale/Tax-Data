"""filing.py: the CPS's filing stage. Which CPS tax units file a return, as a chance per unit from the filing rule
(v4, John, 24 October 2026), calibrated in each tax year to population and SOI counts. Moved here from step 1's
scratch folder (explore/2026-10-23/step1_units: score_units.R and calibrate_v4.py) on 24 October 2026; the results are
the same, apart from the comparison columns left behind (below).

The stage has three steps, run in order by run.py (stage filing), every year of settings cps.years:
  python -m prep.filing units [year ...]      the CPS's tax units by the shared rules (tax_units.py, with CBO's
                                         reassignment pass), people in group quarters left out
  Rscript filing_requirement.R [year ...] each unit's statutory filing requirement (Form 1040, Charts A to C)
  python -m prep.filing calibrate [year ...]  the rule, its calibration, separate returns, the outputs and checks

The rule. For a household unit (not a dependent's own return):
  logit(filing chance) = a[age band of the head] + k x (a child under 17 in the unit)
                         + required x (r + s x z + b x (business or farm income not zero))
  required   the unit's statutory filing requirement (filing_requirement.R)
  z          log(max(gi, floor)) - log(weighted median of gi among required units that year); gi is the unit's gross
             income as tax_units.py builds it (head and spouse: wages, self-employment, interest, dividends, rents,
             estates and trusts, pensions, unemployment, plus taxable Social Security by the 1040 worksheet), the
             nearest match to the 1040's total income that Pub 5785 reports; the log is floored at the year's lowest
             gross income threshold (single, under 65), since about 1.2 m required units have less (mostly required
             by the $400 self-employment rule); without it they would be all but certain non-filers
  business   the head's and spouse's business or farm income (selfemp: INCBUS + INCFARM), positive or negative
  Heads under 18 take the 18-25 band's a. The 11 settings (7 a's, k, r, s, b) are solved each year so that 11
  targets hold exactly:
  - non-filing heads and spouses in each of 7 age bands of the head (18-25 ... 75+) = CPS adults in the band (own
    age, CPS weights) - adults on returns in the band (SOI Table 1.6 as the residual anchor builds it,
    research/state_weights/nonfiler_residual/resources/nonfiler_age_shape_<year>.csv, column filing_adults) -
    dependent netting in the band (dependents aged 15+ by the age of the head who claims them, not filing a
    return of their own). Group quarters are left out (John's decision), so they enter as zero.
  - non-filing children under 17 = settings filing.child_share x CPS children under 17
  - non-filing required units = settings filing.required_nonfiling_share x required units
  - their gross income = settings filing.income_share x the gross income of all required units
  - those with business or farm income = settings filing.business_share x non-filing required units
  Dependents' own returns: a dependent aged 15 or over files (chance 1) when required to (the dependent filing
  tests) or with any wages, else not (chance 0). Weights never move.
Separate returns: joint couples taken in the order of a uniform keyed by unit id (salt settings filing.mfs_salt)
until weight x calibrated chance reaches SOI Table 1.6's separate returns / 2 (tax_units.mfs_split, with chance=);
each couple becomes two returns, the dependents on person 1's (dep_rule 'primary').

Outputs, under the run's data folder (common.DATA) / settings cps.step1_dir ("filing"):
  units/        the tax-unit module's persons, units and roster, <year>_cbo_nogq
  requirement/  units_<year>.csv.gz, persons_<year>.csv.gz (filing_requirement.R)
  returns/      (settings cps.step1_out) units_<year>.csv.gz and .parquet, one row per return (separate returns
                included; build_cps.R reads the csv): ids (uid, couple_uid and person for separate returns, hh,
                head_pn, spouse_pn), weight, p_file_cal (the calibrated chance), must_file, filing_status, ages and
                sex, dependents in the PUF's terms, n24, n_dep_eitc, eic, has_kid_target_age, reason (required, EITC
                eligible, or a refundable child credit), the income items, eitc_eligible, year;
                persons_<year>.parquet, one row per person, with return_uid and p_own_return (a dependent's own
                return: 1 files, 0 not, -1 no such return)
  checks/       per year (target_list.py and errors.py read <year>_nonfilers_vs_targets.csv):
                fit (each target and the miss), shifts (the 11 settings and the median used for z), feasibility,
                groups_vs_targets, nonfilers_vs_targets, persons_once, people_by_age, filers_nonfilers (against SOI;
                the PUF in 2015), pub5785_wages, pub5785_required, nonfilers_by_wages, required_by_income,
                children_by_unit, units_by_group, log.txt
Not carried over from step 1: the filing chances "before calibration" (Mok's probits below the filing threshold and
the Pub 5785 hazard above it: no output used them), the comparisons with rules v2 and v3 and with the old non-filer
pool, and the group quarters summaries (used only by those comparisons and by a check).
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
AGE_LIM = 17                                   # children: under 17
# Pub 5785 Table 3, TY2014-16 averages: required non-filing units, those with wages, their wages, those with
# business or farm income, that income, and total income (millions; billions of dollars)
# (research/state_weights/nonfiler_residual/resources/pub5785_table3_notfiler_units.csv). The rule's shares in
# settings "filing" are computed from these; the 2015 run checks that.
PUB5785_T3 = {'units': 11.19, 'wages_n': 6.90, 'wages_bn': 232.1, 'bus_n': 4.99, 'bus_bn': 100.2, 'totinc_bn': 409.4}
WAGE_BINS = [('$0', -np.inf, 0), ('$1-25k', 0, 25e3), ('$25-50k', 25e3, 50e3), ('$50-100k', 50e3, 100e3),
             ('$100-200k', 100e3, 200e3), ('$200k+', 200e3, np.inf)]
# EITC parameters by tax year: income at which the credit ends for 0, 1, 2, 3+ children (unmarried, joint), the
# investment income limit, and the age range for no children (Rev. Procs.; 2021 is the ARPA year)
EITC = {2015: ([14820, 39131, 44454, 47747], [20330, 44651, 49974, 53267], 3400, (25, 64)),
        2017: ([15010, 39617, 45007, 48340], [20600, 45207, 50597, 53930], 3450, (25, 64)),
        2019: ([15570, 41094, 46703, 50162], [21370, 47440, 52493, 55952], 3600, (25, 64)),
        2021: ([21430, 42158, 47915, 51464], [27380, 48108, 53865, 57414], 10000, (19, 200)),
        2023: ([17640, 46560, 52918, 56838], [24210, 53120, 59478, 63398], 11000, (25, 64))}
# The refundable child credit's earned-income floor: $3,000 in 2015-2017, $2,500 in 2018-2020 and 2022-2023, none
# in 2021
CTC_FLOOR = {2015: 3000, 2016: 3000, 2017: 3000, 2021: 0}
AGES = [('under 5', 0, 5), ('5-12', 5, 13), ('13-16', 13, 17), ('17-18', 17, 19), ('19-23', 19, 24), ('24+', 24, 200)]
# Where the solver starts: near the solutions of 2015-2023 (a's, r, k, s, b)
X0 = [2.0, 0.6, 0.2, 0.5, 0.7, 1.0, 0.8, 2.2, 2.3, 1.6, -3.3]


def band_of(age):
    a = np.asarray(age)
    b = np.searchsorted(BREAKS, a, side='right') - 1
    return np.where(a >= 18, b, -1)


def expit(z):
    return 1 / (1 + np.exp(-z))


def nacount(df, name):
    n = df.isna().sum()
    n = n[n > 0]
    if len(n):
        print(f'NA counts in {name}: {n.to_dict()}')
    return int(n.sum())


def eitc_eligible(u, nkids, year):
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
    return f'{year}_cbo_nogq'


def build_units(year):
    """Step 1 of the stage: the CPS's units by the shared rules, group quarters left out."""
    persons, units = T.build(year)
    T.save_cps(persons, units, UNITS, tag(year))


def calibrate(year):
    """Step 3 of the stage: the rule, its calibration, separate returns, the outputs and the checks."""
    os.makedirs(RETURNS, exist_ok=True); os.makedirs(CHECKS, exist_ok=True)
    log = []
    say = lambda s: (print(s), log.append(s))

    # ---- inputs ---------------------------------------------------------------------------------------------
    U = pd.read_parquet(f'{UNITS}/units_{tag(year)}.parquet')
    P = pd.read_parquet(f'{UNITS}/persons_{tag(year)}.parquet')
    M = U[~U.dep_return].reset_index(drop=True)
    SC = pd.read_csv(f'{REQ}/units_{year}.csv.gz')
    SP = pd.read_csv(f'{REQ}/persons_{year}.csv.gz')
    nd = SC[SC.unit_type == 'nondependent'].set_index('unit_id')
    sd = SC[SC.unit_type == 'dependent'].copy()
    assert set(nd.index) == set(M.uid), 'the requirement step\'s household units and tax_units.py\'s differ'
    # the R persons are the same people in the same units
    chk = P.merge(SP, left_on=['hh', 'pn'], right_on=['SERIAL', 'PERNUM'], how='outer', indicator=True)
    assert (chk._merge == 'both').all() and (chk.uid == chk.unit_id).all()
    assert not P[['hh', 'pn']].duplicated().any()
    M['must_file'] = nd.must_file.reindex(M.uid).to_numpy().astype(bool)
    assert np.allclose(nd.weight.reindex(M.uid).to_numpy(), M.weight.to_numpy())
    assert nacount(M[['must_file', 'weight', 'head_age']], 'household units') == 0

    shape = pd.read_csv(f'{REPO}/{S["age_shape"].format(year=year)}').set_index('band').reindex(BANDS)
    assert shape[['pep_adults', 'filing_adults']].notna().all().all()
    pep_kids = pd.read_csv(f'{REPO}/{S["pep_children"]}').set_index('year').loc[year]

    # ---- persons: heads and spouses (band of the unit's head), children -----------------------------------------
    uidx = pd.Series(np.arange(len(M)), index=M.uid)
    P['ui'] = uidx.reindex(P.uid).to_numpy()
    assert P.ui.notna().all()
    P['ui'] = P.ui.astype(int)
    head_band = band_of(M.head_age.to_numpy())
    hs = P[P.tu_rel.isin([1, 2])]
    hs_ui, hs_w = hs.ui.to_numpy(), hs.weight.to_numpy()
    hs_b = head_band[hs_ui]
    grp7 = lambda v: pd.Series(v).groupby(hs_b).sum().reindex(range(7)).fillna(0).to_numpy()

    # ---- dependents' own returns, CPS adults, the adult targets -------------------------------------------------
    w_d = sd.weight.to_numpy()
    p_own = (sd.must_file.astype(bool) | (sd.INCWAGE > 0)).to_numpy().astype(float)
    sd['p_own'] = p_own
    d_band = band_of(sd.age_head.to_numpy())
    dnet = pd.Series(w_d * (1 - p_own) * (d_band >= 0)).groupby(d_band).sum().reindex(range(7)).fillna(0).to_numpy()
    pb_all = band_of(P.age.to_numpy())
    cps_adults = pd.Series(P.weight.to_numpy()).groupby(pb_all).sum().reindex(range(7)).fillna(0).to_numpy()
    A = cps_adults - shape.filing_adults.to_numpy(); G = np.zeros(7)     # group quarters left out: zero
    T_adult = A - G - dnet
    say(f'TY{year}: dependents\' own returns by the rule: {(w_d * p_own).sum() / 1e6:.2f} m file of '
        f'{w_d.sum() / 1e6:.2f} m dependents aged 15+; adult netting {dnet.sum() / 1e6:.2f} m')
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

    # ---- children --------------------------------------------------------------------------------------------
    T_child = S['child_share'] * P.weight[P.age < AGE_LIM].sum()
    say(f'children target: {S["child_share"]:.6f} x CPS children under {AGE_LIM} '
        f'{P.weight[P.age < AGE_LIM].sum() / 1e6:.3f} m = {T_child / 1e6:.3f} m non-filing')
    kid = P.age < AGE_LIM
    k_ui, k_w = P.ui[kid].to_numpy(), P.weight[kid].to_numpy()
    has_kid = np.zeros(len(M), bool); has_kid[np.unique(k_ui)] = True

    # ---- the income and business terms on required units -----------------------------------------------------
    gi = M.gi.to_numpy()
    assert not np.isnan(gi).any()
    GI_FLOOR = float(nd.threshold.min())
    assert GI_FLOOR > 0 and nd.threshold.notna().all()
    wr, gr = M.weight.to_numpy()[r_req], gi[r_req]
    o = np.argsort(gr, kind='stable'); cw = np.cumsum(wr[o])
    gi_med = float(gr[o][np.searchsorted(cw, cw[-1] / 2)])        # weighted median of gi among required units
    assert gi_med > GI_FLOOR > 0
    zinc = np.where(r_req, np.log(np.maximum(gi, GI_FLOOR)) - np.log(gi_med), 0.0)
    assert not M.selfemp.isna().any()
    has_bus = (M.selfemp.to_numpy() != 0)
    bus_req = has_bus[r_req]
    RATE, SHARE, BSHARE = S['required_nonfiling_share'], S['income_share'], S['business_share']
    INC_REQ = float((wr * gr).sum())
    T_inc = SHARE * INC_REQ
    if year == 2015:      # the shares in the settings are Pub 5785's figures over the 2015 CPS's required units
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

    def chances(x):
        a = x[:7][np.maximum(head_band, 0)]                                 # heads under 18: the 18-25 band
        return expit(a + x[8] * has_kid + r_req * (x[7] + x[9] * zinc + x[10] * has_bus))

    wq = M.weight.to_numpy()[r_req]

    def resid(x):
        p = chances(x)
        r = pd.Series(hs_w * (1 - p[hs_ui])).groupby(hs_b).sum().reindex(range(7)).fillna(0).to_numpy() - T_adult
        r = np.append(r, (k_w * (1 - p[k_ui])).sum() - T_child)
        r = np.append(r, (wq * (1 - p[r_req])).sum() - RATE * wq.sum())
        q_req = wq * (1 - p[r_req])
        r = np.append(r, (q_req * bus_req).sum() - BSHARE * q_req.sum())
        r = r / 1e6                                                          # people and units: millions
        return np.append(r, ((wq * (1 - p[r_req]) * gr).sum() - T_inc) / 1e9)   # income: billions

    # feasibility: every band (and the children) between "every unit files" and "none does"
    feas = pd.DataFrame({'band': BANDS, 'target': T_adult, 'min_possible': np.zeros(7), 'max_possible': grp7(hs_w)})
    feas.loc[len(feas)] = ['children', T_child, 0.0, k_w.sum()]
    feas['feasible'] = (feas.target >= feas.min_possible) & (feas.target <= feas.max_possible)
    feas.to_csv(f'{CHECKS}/{year}_feasibility.csv', index=False)
    if not feas.feasible.all():
        say('INFEASIBLE targets:\n' + feas[~feas.feasible].to_string())
        raise SystemExit(f'TY{year}: a target is outside what the rule can reach; stopped')
    # what each group holds
    nr = ~reason
    diag = pd.DataFrame({'band': BANDS, 'target_nonfiling': T_adult / 1e6,
                         'heads_spouses_no_reason': grp7(hs_w * nr[hs_ui]) / 1e6,
                         'heads_spouses_reason': grp7(hs_w * reason[hs_ui]) / 1e6})
    diag['heads_spouses_required'] = grp7(hs_w * r_req[hs_ui]) / 1e6
    diag['heads_spouses_not_required'] = grp7(hs_w * ~r_req[hs_ui]) / 1e6
    diag.loc[len(diag)] = ['children under 17', T_child / 1e6, (k_w * nr[k_ui]).sum() / 1e6, (k_w * reason[k_ui]).sum() / 1e6,
                           (k_w * r_req[k_ui]).sum() / 1e6, (k_w * ~r_req[k_ui]).sum() / 1e6]
    diag.to_csv(f'{CHECKS}/{year}_groups_vs_targets.csv', index=False)
    say('--- targets against what each group holds (millions)\n' + diag.round(3).to_string())

    # ---- the solve -------------------------------------------------------------------------------------------
    x0 = np.array(X0, dtype=float)
    sol = root(resid, x0, method='hybr', tol=1e-12)
    if not sol.success or np.abs(resid(sol.x)).max() > 1e-6:
        sol = root(resid, x0, method='lm', tol=1e-14)
    names = BANDS + ['children under 17', 'non-filing units, required to file',
                     'non-filers with business or farm income, required to file',
                     'gross income of non-filers, required to file (billions)']
    tgt = np.append(np.append(T_adult, T_child), [RATE * M.weight[r_req].sum(),
                                                  BSHARE * RATE * M.weight[r_req].sum()]) / 1e6
    tgt = np.append(tgt, T_inc / 1e9)
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
    M['p_cal'] = chances(x)
    p_cal = M.p_cal.to_numpy()
    shifts = pd.DataFrame({'shift': params, 'logit_shift': x})
    shifts.loc[len(shifts)] = ['weighted median gross income, required (dollars, not a parameter)', gi_med]
    shifts.to_csv(f'{CHECKS}/{year}_shifts.csv', index=False)
    say('the rule\'s settings (logit): ' + ', '.join(f'{a} {b:+.3f}' for a, b in zip(shifts['shift'], x)))

    M['has_kid_target_age'] = has_kid
    # ---- married filing separately ---------------------------------------------------------------------------
    mfs_target = sum(TG.table_1_6(year)[('separate', k)] for k in range(6))
    chosen, share, R = T.mfs_split(M, P, None, mfs_target, salt=S['mfs_salt'], chance=p_cal)
    say(f'MFS: SOI Table 1.6 {mfs_target / 1e6:.3f} m separate returns; {len(chosen)} sample couples '
        f'({share:.4f} of joint couples by weight x chance); expected separate returns '
        f'{(R.weight * R.p_file).sum() / 1e6:.3f} m')
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

    # ---- the units file --------------------------------------------------------------------------------------
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

    # ---- the person file -------------------------------------------------------------------------------------
    P['return_uid'] = P.uid
    pr1, pr2 = T.primary_and_spouse(P, chosen)
    ins = P.uid.isin(chosen)
    one = pd.Series(2 * 10 ** 12 + pr1.index.to_numpy() * 10 + 1, index=pr1.index)
    P.loc[ins, 'return_uid'] = one.reindex(P.uid[ins]).to_numpy()          # dependents and person 1
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

    # ================================ checks ================================================================
    # 1. every person once
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

    # 2. CPS people by age band against Census PEP (resident population, so group quarters included; the CPS frame
    #    leaves them out)
    pb = band_of(P.age.to_numpy())
    rows = [(b, P.weight[pb == i].sum() / 1e6, shape.pep_adults[b] / 1e6) for i, b in enumerate(BANDS)]
    rows.append((f'under {AGE_LIM}', P.weight[P.age < AGE_LIM].sum() / 1e6, pep_kids[f'pep_u{AGE_LIM}'] / 1e6))
    c2 = pd.DataFrame(rows, columns=['age', 'cps_people', 'census_pep'])
    c2['difference'] = c2.cps_people - c2.census_pep
    c2.to_csv(f'{CHECKS}/{year}_people_by_age.csv', index=False)

    # 4. non-filing adults and children against their targets. census_adults holds the CPS's adults (household
    #    population), the population the targets are built on; the children's row holds Census PEP under 17.
    after = grp7(hs_w * (1 - p_cal[hs_ui]))
    c4 = pd.DataFrame({'band': BANDS, 'census_adults': cps_adults, 'adults_on_returns_T16': shape.filing_adults.to_numpy(),
                       'anchor': A, 'gq_nonfilers': G, 'dependent_netting': dnet, 'target_heads_spouses': T_adult,
                       'after': after})
    c4.loc[len(c4)] = [f'children under {AGE_LIM}', pep_kids[f'pep_u{AGE_LIM}'], np.nan, np.nan, np.nan, np.nan,
                       T_child, (k_w * (1 - p_cal[k_ui])).sum()]
    c4['after_minus_target'] = c4.after - c4.target_heads_spouses
    for c in c4.columns[1:]:
        c4[c] = c4[c] / 1e6
    c4.to_csv(f'{CHECKS}/{year}_nonfilers_vs_targets.csv', index=False)
    # the partition: CPS adults = adults on returns + non-filing heads and spouses + group quarters (0) + netting
    part = (c4.iloc[:7].adults_on_returns_T16 + c4.iloc[:7].after + c4.iloc[:7].gq_nonfilers
            + c4.iloc[:7].dependent_netting - c4.iloc[:7].census_adults).abs().max()
    say(f'partition (CPS adults = adults on returns + non-filing heads and spouses + group quarters non-filers + '
        f'dependent netting): largest gap {part * 1e6:.3f} people')

    # 3. expected filers and non-filers, against SOI (and the PUF in 2015)
    ttab = tables(year, out, P, sd)
    ttab.to_csv(f'{CHECKS}/{year}_filers_nonfilers.csv')

    # 5. Pub 5785 check: non-filing people with wages
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
    # required non-filers against Pub 5785 Table 3; non-filers by wage size
    q = out.weight.to_numpy() * (1 - out.p_file_cal.to_numpy()); rq = out.must_file.to_numpy().astype(bool)
    wg, se, g = out.wage.to_numpy(), out.selfemp.to_numpy(), out.gi.to_numpy()
    t = PUB5785_T3
    c6 = pd.DataFrame([
        {'rule': 'v4', 'units_m': q[rq].sum() / 1e6, 'gross_income_bn': (q * g)[rq].sum() / 1e9,
         'with_wages_m': q[rq & (wg > 0)].sum() / 1e6, 'wages_bn': (q * wg)[rq].sum() / 1e9,
         'with_business_farm_m': q[rq & (se != 0)].sum() / 1e6, 'business_farm_bn': (q * se)[rq].sum() / 1e9,
         'all_nonfilers_m': q.sum() / 1e6, 'all_nonfiler_wages_bn': (q * wg).sum() / 1e9},
        {'rule': 'Pub 5785 TY2014-16 average', 'units_m': t['units'], 'gross_income_bn': t['totinc_bn'],
         'with_wages_m': t['wages_n'], 'wages_bn': t['wages_bn'], 'with_business_farm_m': t['bus_n'],
         'business_farm_bn': t['bus_bn'], 'all_nonfilers_m': np.nan, 'all_nonfiler_wages_bn': np.nan}])
    c6.to_csv(f'{CHECKS}/{year}_pub5785_required.csv', index=False)
    say('--- required-to-file non-filing returns (household returns, separate returns counted singly; millions, '
        'billions of the tax year) against Pub 5785 Table 3, whose figures are TY2014-16 averages and include '
        'dependents\' own returns\n' + c6.round(2).to_string())
    rows7 = []
    for name, lo, hi in WAGE_BINS:
        m = (wg <= 0) if name == '$0' else (wg > lo) & (wg <= hi)
        rows7.append((name, q[m].sum() / 1e6, (q * wg)[m].sum() / 1e9))
    rows7.append(('total', q.sum() / 1e6, (q * wg).sum() / 1e9))
    c7 = pd.DataFrame(rows7, columns=['wages', 'nonfilers_m, v4', 'wages_bn, v4']).set_index('wages')
    c7.to_csv(f'{CHECKS}/{year}_nonfilers_by_wages.csv')
    say('--- non-filing returns by the return\'s wages (head and spouse; "$0" includes negative), millions and '
        'billions\n' + c7.round(2).to_string())
    # the non-filing chance of required units by gross income
    gb = pd.cut(gi, [-np.inf, 0, 10e3, 25e3, 50e3, 100e3, 200e3, 500e3, np.inf])
    c8 = pd.DataFrame({'gi': gb[r_req], 'w': M.weight.to_numpy()[r_req], 'q': M.weight.to_numpy()[r_req] *
                       (1 - p_cal[r_req])}).groupby('gi', observed=False)[['w', 'q']].sum()
    c8['nonfiling_rate'] = c8.q / c8.w
    c8 = c8.rename(columns={'w': 'required_units', 'q': 'nonfiling'})
    c8[['required_units', 'nonfiling']] /= 1e6
    c8.to_csv(f'{CHECKS}/{year}_required_by_income.csv')
    say('--- required household units by gross income (before the separate-return split): millions, non-filing '
        'rate\n' + c8.round(4).to_string())
    for name, tb in [('persons once', c1), ('people by age', c2), ('non-filers vs targets', c4)]:
        say(f'--- {name}\n' + tb.round(3).to_string())
    say('--- filers and non-filers\n' + ttab.round(2).to_string())
    say('--- Pub 5785\n' + c5.round(2).to_string())
    open(f'{CHECKS}/{year}_log.txt', 'w').write('\n'.join(log) + '\n')


def rows_for(d, p):
    """d: returns (weight, filing_status, dependents, ...), p: filing chance. Returns two dicts: expected filers
    (weight x p) and non-filers (weight x (1 - p)), millions."""
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
    # Table 1.6 counts dependents' own returns too, nearly all as single: net them out of single and of all, to
    # match the CPS rows (dependents' own returns are folded into the unit that claims them)
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
                                                         '  parents'])   # all dependents (no caps on SOI's count)
    return s


def puf_column():
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
    # the PUF's dependent ages are codes 1-6 for the AGES ranges, 0 withheld (no dependent, or age not shown)
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

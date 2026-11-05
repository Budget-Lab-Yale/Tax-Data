"""Turn the 2015 IRS Public Use File into the Budget Lab's Tax-Simulator file for one year.

    python merge_puf.py <puf_2015.csv> <package folder> <output: .csv, .parquet, or both with no extension>

Needs Python 3 with numpy, pandas and pyarrow, and about 16 GB of memory. The package folder (written by the Budget
Lab's pipeline, package.py) holds the files this script reads; its README.md says what each one is. The steps:

  1. From the PUF's own fields, each record's 2015 items as the Budget Lab defines them (items_2015; the definitions
     are in spec.json, written by the same code that built the file).
  2. Each PUF record appears once per trained network (column copy, 1 to 3). For each record copy, each PUF item is
     its 2015 amount multiplied, in order, by the item's factors in factors.parquet: one factor for positive amounts
     and one for negative amounts at each step (the growth from 2015 to the year the networks drew the records, the
     year's Schedule A multiplier on six itemized items, then each projection step). A 2015 amount of zero stays zero.
     The same factors apply to every record: none depends on the record.
  3. The weight is the PUF weight (S006 / 100) times the record copy's weight factor (records.parquet).
  4. The items the networks and the other imputations added, as plain values (records.parquet), joined on RECID and
     copy.
  5. The columns that are sums or simple functions of the above (capital gains after the $3,000 loss limit, business
     income, other income in AGI, taxable Social Security, each spouse's wages, tips, overtime pay and elective
     deferrals), computed by the same rules and in the same order as the Budget Lab's pipeline (derive).
  6. Two rules of the pipeline that change a PUF item by more than a growth factor, applied here: taxable state
     refunds are removed where the record copy's state_ref_kept is 0 (the tax benefit rule: a refund is taxable only
     to a return that itemized), and AGI's other income and taxable Social Security are computed again without them;
     the AMT net operating loss adjustment is held to the record's net operating loss, the negative part of its other
     income (amt_cap). Three added items that are computed from a PUF amount are computed here from the PUF items, so
     the package ships only their other part (puf_based).
  7. The Tax-Simulator file's columns (tax_simulator_columns), then the records that are not PUF records (non-filers
     and the Forbes list of billionaires, extra_records.parquet), in the file's own order.

Every dollar amount of the output is in nominal dollars of the package's year.
"""
import json
import os
import sys

import numpy as np
import pandas as pd


# ---- the package --------------------------------------------------------------------------------------------------
def read_package(pkg):
    spec = json.load(open(os.path.join(pkg, 'spec.json')))
    fac = pd.read_parquet(os.path.join(pkg, 'factors.parquet'))
    rec = pd.read_parquet(os.path.join(pkg, 'records.parquet'))
    extra = pd.read_parquet(os.path.join(pkg, 'extra_records.parquet'))
    return spec, fac, rec, extra


# ---- step 1: the 2015 items ---------------------------------------------------------------------------------------
def allowed_capital_gain(sched_d, filing_status):
    """The Schedule D net as it enters AGI: a net loss held to $3,000 ($1,500 on a separate return, status 3)."""
    limit = np.where(np.asarray(filing_status) == 3, 1500.0, 3000.0)
    return np.maximum(np.asarray(sched_d, float), -limit)


def signed_sum(df, parts):
    """A total from its parts, {column: sign}, as the pipeline sums it."""
    return df[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float)


def items_2015(raw, spec, floor):
    """Each record's 2015 items from the PUF's fields (the pipeline's build_table.puf_rows). raw: the PUF with every
    field in spec raw_fields; floor: per record, the medical expense floor used for other itemized deductions (0.075
    or 0.1, as the pipeline chose it from the record's age). The four aggregate records (RECID 999996-999999) are read
    as every one an itemizer, and their blank alimony fields as zero."""
    s = spec['items']
    agg = raw['RECID'].to_numpy() >= 999996
    f = {c: raw[c].to_numpy(float) for c in spec['raw_fields']}
    for c in s['suppressed_on_aggregate_records']:
        assert np.isnan(f[c][agg]).all() and not np.isnan(f[c][~agg]).any(), c
        f[c] = np.where(agg, 0.0, f[c])
    na = [c for c in spec['raw_fields'] if np.isnan(f[c]).any()]
    assert not na, f'PUF fields with missing values: {na}'
    mars = f['MARS']
    out = {'wages': f['E00200'], 'txbl_int': f['E00300'], 'div_pref': f['E00650'], 'div_ord': f['E00600'] - f['E00650'],
           'kg_lt': f['P23250'], 'kg_dist': f['E01100']}
    # line 13's shortfall below the allowed Schedule D net is put on the short-term piece
    out['kg_st'] = f['P22250'] + np.minimum(f['E01000'] - allowed_capital_gain(f['P22250'] + f['P23250'], mars), 0)
    for p, (fld, _) in s['parts'].items():
        if fld:
            out[p] = f[fld]
    out['sche_other'] = f['E02000'] - sum(s['parts'][p][1] * out[p] for p in s['sche_parts'])
    out.update({'other_gains': f['E01200'], 'txbl_ira_dist': f['E01400'], 'txbl_pens_dist': f['E01700'],
                'nontxbl_pens_dist': np.maximum(f['E01500'] - f['E01700'], 0.0), 'ui': f['E02300'],
                'alimony': f['E00800'], 'state_ref': f['E00700'], 'gross_ss': f['E02400'], 'exempt_int': f['E00400'],
                'first_mort_int': f['E19200'], 'salt_prop': f['E18500']})
    out.update({p: f[fld] for p, fld in s['adj_parts'].items()})
    out.update({v: f[fld] for v, fld in s['new_items'].items() if fld})
    k1_part = (f['E25940'] + f['E25980'] - f['E25920'] - f['E25960']) != 0
    out['part_se'] = np.where(k1_part, (f['E30400'] + f['E30500']) / 0.9235 - f['E00900'] - f['E02100'], 0.0)
    itemizer = (f['FDED'] == 1) | agg
    medical = np.maximum(0, f['E17500'] - np.maximum(0, f['E00100'] * floor))
    out['other_item_exp'] = np.maximum(np.where(itemizer, f['P04470'] + f['E21040'] - medical - f['E18400'] - f['E18500']
                                                - f['E19200'] - f['E19700'] - f['E20500'] - f['E20800'], 0.0), 0.0)
    # business income: the signed sum of its eighteen parts (on an ordinary record E00900 + E02000 + E02100 exactly)
    parts = {p: sg for p, (_, sg) in s['parts'].items()}
    out['business_income'] = signed_sum(pd.DataFrame({p: out[p] for p in parts}), parts)
    # AGI's other income (the remainder): AGI less every item above and less taxable Social Security (E02500), the
    # items summed in this order (it matters on the aggregate records, whose amounts are not whole dollars)
    signs = s['agi_item_signs']
    terms = {'business_income': f['E00900'] + f['E02000'] + f['E02100'], 'other_gains': out['other_gains'],
             'txbl_ira_dist': out['txbl_ira_dist'], 'txbl_pens_dist': out['txbl_pens_dist'], 'ui': out['ui'],
             'alimony': out['alimony'], 'state_ref': out['state_ref'],
             'adjustments': sum(f[c] for c in s['adj_fields'])}
    assert set(terms) == set(signs), set(terms) ^ set(signs)
    allowed = allowed_capital_gain(out['kg_st'] + f['P23250'], mars)
    out['other_agi'] = f['E00100'] - f['E00200'] - f['E00300'] - f['E00600'] - allowed - f['E01100'] \
        - sum(signs[v] * terms[v] for v in terms) - f['E02500']
    return pd.DataFrame(out, index=raw['RECID'].to_numpy())


def puf_categories(raw):
    """Filing status, marriage and the three dependent counts from the PUF's fields, per record (RECID index)."""
    n_dep = raw[['XOCAH', 'XOCAWH', 'XOPAR', 'XOODEP']].to_numpy().sum(1)
    mars = raw['MARS'].to_numpy()
    return pd.DataFrame({'filing_status': mars, 'married': (mars == 2).astype(np.int64),
                         'n_dep': np.minimum(n_dep, 3), 'n_dep_ctc': np.minimum(raw['N24'].to_numpy(), 3),
                         'n_dep_eitc': np.minimum(raw['EIC'].to_numpy(), 3)}, index=raw['RECID'].to_numpy())


# ---- step 2: the factors ------------------------------------------------------------------------------------------
def grow(base, fac):
    """{column: amounts}: each item's 2015 amounts (base[puf_item]) multiplied by its factors step by step, the factor
    for positive amounts on positive amounts and the factor for negative amounts on negative ones; zero stays zero."""
    out = {}
    for col, g in fac.groupby('column', sort=False):
        g = g.sort_values('step')
        assert g['puf_item'].nunique() == 1, f'{col}: more than one 2015 item'
        v = base[g['puf_item'].iloc[0]].to_numpy(float).copy()
        for p, n in zip(g['positive'].to_numpy(float), g['negative'].to_numpy(float)):
            v = np.where(v > 0, v * p, np.where(v < 0, v * n, v))
        out[col] = v
    return out


# ---- step 5: the computed columns ---------------------------------------------------------------------------------
def taxable_social_security(gross_ss, magi, filing_status, sp):
    """The section 86 worksheet: provisional income p = max(0.5 x benefits + modified AGI, 0); 50% of the part of p
    between the base and the adjusted amount, 85% of the part above it, each part at most the benefits, the sum at
    most 85% of the benefits. Base and adjusted amounts $25,000 and $34,000; $32,000 and $44,000 on a joint return."""
    ss, magi, joint = np.asarray(gross_ss, float), np.asarray(magi, float), np.asarray(filing_status) == 2
    base = np.where(joint, sp['joint'][0], sp['single'][0])
    adjusted = np.where(joint, sp['joint'][1], sp['single'][1])
    p = np.maximum(sp['magi_rate'] * ss + magi, 0.0)
    tier1 = np.minimum(np.clip(p, base, adjusted) - base, ss) * sp['rates'][0]
    tier2 = np.minimum(np.maximum(p - adjusted, 0.0), ss) * sp['rates'][1]
    return np.minimum(tier1 + tier2, sp['rates'][1] * ss)


def magi_without_ss(d, other):
    """AGI without taxable Social Security (wages, interest, dividends, the Schedule D net, other income), plus
    tax-exempt interest and the student loan interest deduction: the worksheet's modified AGI."""
    without = d[['wages', 'txbl_int', 'dividends', 'capital_gains']].to_numpy(float).sum(1) + other
    return without + d['exempt_int'].to_numpy(float) + d['sl_int_ded'].to_numpy(float)


def derive_inputs(spec):
    """The columns derive reads (besides those it makes)."""
    D = spec['derive']
    cols = ['kg_st', 'kg_lt', 'kg_dist', 'filing_status', 'other_agi_remainder', 'wages', 'txbl_int', 'exempt_int',
            'sl_int_ded', 'gross_ss', 'elective_deferrals', 'married', 'age1', 'age2']
    for parts in D['computed_totals'].values():
        cols += list(parts)
    cols += D['k1_parts'] + list(D['agi_fold']) + D['pension_parts']
    for slot, (item, base) in D['ratios'].items():
        cols += [slot, base]
    for item, (total, part) in D['remainders'].items():
        cols += [total, part]
    for slot, (item, cap, *less) in D['person_caps'].items():
        cols += [slot, cap] + less
    for parts in D['sums'].values():
        cols += parts
    for flag, amount in D['flags_with'].values():
        cols += [flag, amount]
    return list(dict.fromkeys(cols))


def derive(d, spec):
    """In place: the columns computed from the items, in the pipeline's order (dollars.draw_dollars, and
    tilt.recombine in a projection year)."""
    D = spec['derive']
    fs = d['filing_status'].to_numpy()
    d['capital_gains'] = allowed_capital_gain(d['kg_st'] + d['kg_lt'], fs)
    d['capital_gains_line13'] = d['capital_gains'] + d['kg_dist']
    for tot, parts in D['computed_totals'].items():
        d[tot] = signed_sum(d, parts)
    d['k1_owner'] = (d[D['k1_parts']].to_numpy(float) != 0).any(1).astype(np.int64)
    other = d['other_agi_remainder'].to_numpy(float) + d['kg_dist'].to_numpy(float) + signed_sum(d, D['agi_fold'])
    d['gross_pens_dist'] = d[D['pension_parts']].to_numpy(float).sum(1)
    d['ss_taxable'] = taxable_social_security(d['gross_ss'], magi_without_ss(d, other), fs, D['social_security'])
    d['other_agi'] = other + d['ss_taxable'].to_numpy(float)
    for slot, (item, base) in D['ratios'].items():
        if item in D['ratio_items_made']:
            d[item] = d[slot] * d[base]
    for item, (total, part) in D['remainders'].items():
        d[item] = d[total] - d[part]
    for slot, (item, cap, *less) in D['person_caps'].items():
        c = d[cap].to_numpy(float)
        if less:
            c = c - d[less[0]].to_numpy(float)
        d[item] = np.minimum(d[slot].to_numpy(float), np.maximum(c, 0.0))
    for item, parts in D['sums'].items():
        d[item] = d[parts].to_numpy(float).sum(1)
    for item, (flag, amount) in D['flags_with'].items():
        d[item] = np.where(d[amount].to_numpy(float) != 0, d[flag].to_numpy(), 0).astype(np.int64)
    lim, roth = D['deferral_limit'], D['roth_share']
    trad = np.zeros(len(d))
    married = d['married'].to_numpy()
    drawn = {1: d['elective1'].to_numpy(float), 2: d['elective2'].to_numpy(float)}
    for k in (1, 2):
        limit = np.where(d[f'age{k}'].to_numpy(float) >= D['catch_up_age'], lim[1], lim[0])
        own = np.minimum(np.minimum(drawn[k], limit), np.maximum(d[f'wages{k}'].to_numpy(float), 0.0))
        d[f'elective{k}'] = own
        d[f'trad_contr_er{k}'] = own * (1 - np.where(married == 1, roth['joint'], roth['nonjoint']))
        trad = trad + d[f'trad_contr_er{k}'].to_numpy(float)
    d['trad_deferrals'] = trad
    d['medicare_wages'] = d['wages'].to_numpy(float) + trad
    return d


# ---- step 6: the two rules ----------------------------------------------------------------------------------------
def refund_rule(d, kept, spec):
    """In place (the pipeline's taxes.apply_refund_rule): the taxable state refund removed where kept is 0, other
    income lowered by the refund removed, then taxable Social Security computed again on the lower AGI, in the
    pipeline's order, on every record."""
    ref = d['state_ref'].to_numpy(float)
    ref2 = np.where(np.asarray(kept) == 1, ref, 0.0)
    d['state_ref'] = ref2
    d['other_agi'] = d['other_agi'].to_numpy(float) + (ref2 - ref)
    other = d['other_agi'].to_numpy(float) - d['ss_taxable'].to_numpy(float)
    sp = spec['derive']['social_security']
    taxable = taxable_social_security(d['gross_ss'], magi_without_ss(d, other), d['filing_status'].to_numpy(), sp)
    d['ss_taxable'] = taxable
    d['other_agi'] = other + taxable
    return d


def amt_cap(d):
    """In place (the pipeline's non_micro_imputations.amt_nol_cap): the AMT net operating loss adjustment (PUF P60100,
    grown) held to at most the record's net operating loss, the negative part of its other income (the PUF has no
    field of its own for the loss), so it is zero on a record with no loss; a negative adjustment is left as it is."""
    loss = np.maximum(0.0, -d['other_agi_remainder'].to_numpy(float))
    d['amt_nols'] = np.minimum(d['amt_nols_drawn'].to_numpy(float), loss)
    return d


def lt_basis(kg, r):
    """The basis of long-term gains: a gain times r / (1 - r), a loss less r / (r - 1) times it, r the basis-to-sales
    ratio of the record's holding period (the pipeline's non_micro_imputations.gains_holding_period); 0 with none."""
    kg, r = np.asarray(kg, float), np.asarray(r, float)
    b = np.zeros(len(kg))
    g, l = kg > 0, kg < 0
    b[g] = kg[g] * r[g] / (1 - r[g])
    b[l] = -kg[l] * r[l] / (r[l] - 1)
    return b


def puf_based(d, rec, spec):
    """In place: the added items computed from a PUF amount (the pipeline's non_micro_imputations), so that the package
    ships only their other part: the basis of long-term gains from the gain and the shipped ratio r; the farm's wage
    bill for the qualified business income deduction, the farm income if positive; the S corporation's wage bill, the
    shipped outside workers' pay plus the record's wages where the shipped flag is 1."""
    if 'kg_lt_basis' in spec['puf_based']:
        d['kg_lt_basis'] = lt_basis(d['kg_lt'], rec['kg_lt_basis_ratio'].to_numpy(float))
    if 'wagebill_farm' in spec['puf_based']:
        d['wagebill_farm'] = np.maximum(np.asarray(d['farm'], float), 0.0)
    if 'wagebill_scorp' in spec['puf_based']:
        o = rec['wagebill_scorp_outside'].to_numpy(float)
        d['wagebill_scorp'] = np.where(rec['wagebill_scorp_owner_wages'].to_numpy() == 1, o + np.asarray(d['wages'], float), o)
    return d


# ---- step 7: the Tax-Simulator file's columns ---------------------------------------------------------------------
def formula(name, d):
    """The Tax-Simulator columns made from the record's items (the pipeline's calculator.convert and
    taxsim_file.build)."""
    x = lambda c: d[c].to_numpy(float)
    share = x('spouse_se_share')
    if name in ('sole_prop2', 'farm2', 'part_se2'):
        return x(name[:-1]) * share
    if name in ('sole_prop1', 'farm1', 'part_se1'):
        return x(name[:-1]) - x(name[:-1]) * share
    if name == 'other_inc':
        return x('other_agi_remainder') + x('farm_rent') + x('sche_other')
    if name == 'kg_lt':
        return x('kg_lt') + x('kg_dist')
    if name == 'gross_pens_dist':
        return x('txbl_pens_dist') + x('nontxbl_pens_dist')
    if name == 'divorce_year':   # 0 where alimony is received or paid, else missing (production's rule)
        return np.where((x('alimony') != 0) | (x('alimony_exp') != 0), 0.0, np.nan)
    if name in ('kg_lt_years_held', 'kg_lt_basis'):
        # missing where the long-term gain (with capital gain distributions) is 0; 0 where it is not 0 but the record
        # has no holding period (capital gain distributions alone)
        kg = x('kg_lt') + x('kg_dist')
        none = (kg != 0) & (x('kg_lt_years_held') < 0)
        return np.where(kg == 0, np.nan, np.where(none, 0.0, x(name)))
    if name == 'id':
        return d['record_id'].to_numpy(np.int64)
    if name == 'weight':
        return x('weight')
    raise ValueError(name)


def tax_simulator_columns(d, rec, spec):
    """The Tax-Simulator file's columns for the PUF record copies: each column is a column of the merged frame, a
    formula of it, a value shipped in records.parquet (prefix ts.), or a constant (spec taxsim rules)."""
    out = {}
    for c, (how, what) in spec['taxsim']['rules'].items():
        if how == 'file':
            out[c] = d[what].to_numpy(np.int64 if c == 'id' else float)
        elif how == 'formula':
            out[c] = formula(c, d)
        elif how == 'shipped':
            out[c] = rec['ts.' + c].to_numpy()
        elif how == 'constant':
            out[c] = np.full(len(d), np.nan if what is None else what, dtype=object if isinstance(what, str) else float)
        else:
            raise ValueError(f'{c}: {how}')
    return pd.DataFrame({c: out[c] for c in spec['taxsim']['columns']})


# ---- the merge ----------------------------------------------------------------------------------------------------
SPEC = {}


def merge(puf_path, pkg, file_frame=False):
    """The year's Tax-Simulator file from the PUF at puf_path and the package folder pkg. file_frame: also return the
    merged frame in the pipeline's own column names (the round-trip test reads it)."""
    spec, fac, rec, extra = read_package(pkg)
    SPEC.clear()
    SPEC.update(spec)
    raw = pd.read_csv(puf_path, usecols=lambda c: c in set(spec['raw_fields']))
    assert raw['RECID'].is_unique, 'RECID is not unique in this PUF'
    floor = rec.groupby('RECID', sort=False)['medical_floor'].first()
    raw = raw[raw['RECID'].isin(floor.index)].reset_index(drop=True)
    assert len(raw) == len(floor), 'a RECID of the package is not in this PUF'
    x15 = items_2015(raw, spec, floor.loc[raw['RECID']].to_numpy())
    rid = rec['RECID'].to_numpy()
    base = x15.loc[rid].reset_index(drop=True)
    d = pd.DataFrame({'RECID': rid, 'copy': rec['copy'].to_numpy()})
    grown = grow(base, fac)
    for c in spec['factor_columns']:
        d[c] = grown[c]
    w15 = raw.set_index('RECID')['S006'].loc[rid].to_numpy(float) / 100
    d['weight'] = w15 * rec['weight_factor'].to_numpy(float)
    cats = puf_categories(raw).loc[rid].reset_index(drop=True)
    agg = rid >= 999996                      # the aggregate records' categories were drawn: in records.parquet
    for c in cats.columns:
        d[c] = np.where(agg, rec[c].to_numpy(), cats[c].to_numpy()).astype(np.int64)
    for c in spec['imputed']:
        d[c] = rec[c].to_numpy()
    for c, v in spec['constants'].items():
        d[c] = v
    d['record_id'] = spec['real_id_base'] + (d['copy'] - 1) * spec['copy_block'] + d['RECID']
    derive(d, spec)
    refund_rule(d, rec['state_ref_kept'].to_numpy(), spec)
    amt_cap(d)
    puf_based(d, rec, spec)
    t = tax_simulator_columns(d, rec, spec)
    t = t[t['weight'] > 0]          # live records only, as the pipeline writes the file
    t.insert(0, 'row', rec['row'].to_numpy()[t.index.to_numpy()])
    out = pd.concat([t, extra[['row'] + spec['taxsim']['columns']]], ignore_index=True)
    out = out.sort_values('row', kind='stable').drop(columns='row').reset_index(drop=True)
    assert len(out) == spec['taxsim']['records'], f"{len(out):,} records; the year's file has {spec['taxsim']['records']:,}"
    return (out, d) if file_frame else out


def write_csv(g, path, spec):
    """Production's text form: NA for missing, TRUE/FALSE flags, whole numbers without a decimal point."""
    t = g.copy()
    for c in spec['taxsim']['flags']:
        v = t[c].to_numpy(float)
        t[c] = np.where(np.isnan(v), 'NA', np.where(v == 1, 'TRUE', 'FALSE')).astype(object)
    for c in spec['taxsim']['integers']:
        assert t[c].notna().all() and (t[c] == t[c].round()).all(), f'{c}: not a whole number'
        t[c] = t[c].astype(np.int64)
    for c in spec['taxsim']['integers_with_missing']:
        t[c] = t[c].astype('Int64')
    t.to_csv(path, index=False, na_rep='NA')


if __name__ == '__main__':
    if len(sys.argv) != 4:
        sys.exit(__doc__)
    puf, pkg, dest = sys.argv[1:]
    out = merge(puf, pkg)
    stem, ext = os.path.splitext(dest)
    if ext in ('', '.parquet'):
        out.to_parquet(stem + '.parquet', index=False)
    if ext in ('', '.csv'):
        write_csv(out, stem + '.csv', SPEC)
    print(f'{len(out):,} records, {out["weight"].sum() / 1e6:,.2f} million tax units, written to {dest}')

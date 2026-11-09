"""The years of the CEX, the CPS and the SIPP that a target year's file is drawn from, and their shares.

The shares are written by hand in the prior of targets_config/<year>/prior.yaml, one entry per survey; this module
only reads them. A survey year here is the tax year the survey's income refers to.
"""
import pandas as pd

from shared import common

ENTRY = {'cex': 'consumption', 'cps': 'cps items', 'sipp': 'sipp tips'}
SOURCES = list(ENTRY)


def prior_shares(year, entry, source):
    """[(year, share)] from one entry of the year's prior, whose keys are '<source> <year>', sorted by year."""
    from reweight import target_list as TLS
    prior = TLS.resolve(year).get('prior', {}).get(entry)
    where = f'targets_config/{year}/prior.yaml, prior "{entry}"'
    assert prior, f'{where}: missing (the {source} year mix)'
    out = []
    for key, share in prior.items():
        src, y = str(key).split()
        assert src == source and float(share) > 0, f'{where}: {key}: {share}'
        out.append((int(y), float(share)))
    total = sum(s for _, s in out)
    assert abs(total - 1) < 1e-6, f'{where}: shares sum to {total}'
    return sorted((y, s / total) for y, s in out)


def mix(source, year=None):
    """[(tax year, share)] for the survey in the target year's file, sorted by year, shares summing to one."""
    year = common.TARGET_YEAR if year is None else year
    return prior_shares(year, ENTRY[source], source)


def years(source, year=None):
    return [y for y, _ in mix(source, year)]


# ---- the surveys' units in the year mix (targets and reports) ----------------------------------------------------
def growth(source, y):
    """{variable: factor from survey year y to the target year} for the survey's dollar amounts, as the dollar maps
    use them."""
    g = pd.read_csv(f'{common.MAPS}/survey_year_growth_factors.csv')
    g = g[(g['survey'] == source) & (g['year'] == y)]
    assert len(g), f'no growth factors for {source} {y} in {common.MAPS}/survey_year_growth_factors.csv'
    return dict(zip(g['variable'], g['factor to the target year'].astype(float)))


# Dollar columns of a survey's unit file that have no map, grown by the wage factor
EXTRA_DOLLARS = {'cps': ['wages_translated', 'wages_primary_reported', 'wages_spouse_reported', 'se_primary_reported',
                         'se_spouse_reported'],
                 'sipp': ['wages_translated', 'wages_primary_reported', 'wages_spouse_reported', 'tips_primary_reported',
                          'tips_spouse_reported'],
                 'cex': []}


def grown_units(source, y):
    """The survey's tax units of year y with every dollar column in target-year dollars: the map amounts by the maps'
    factors, and the other dollar columns by SOI's wages per return, which is the factor of every CPS and SIPP map
    amount (read from the CPS's childcare line)."""
    u = pd.read_parquet(f'{common.DATA}/{source}_units_{y}_all.parquet')
    f = growth(source, y)
    for c, x in f.items():
        if c in u:
            u[c] = u[c].to_numpy(float) * x
    if source != 'cex':
        wf = f['care_exp'] if source == 'cps' else next(iter(f.values()))
        for c in EXTRA_DOLLARS[source]:
            assert c in u, f'{source} {y}: no column {c}'
            u[c] = u[c].to_numpy(float) * wf
    return u


def mixed_units(source, year=None):
    """The survey's units over its year mix as one frame in target-year dollars, with columns 'share' and
    'survey_year'. Each year's weights are scaled to the same total, then multiplied by the year's share, so the
    shares do not depend on the size of a year's population."""
    m = mix(source, year)
    frames = [(y, s, grown_units(source, y)) for y, s in m]
    ref = sum(s * u['weight'].sum() for _, s, u in frames)
    out = []
    for y, s, u in frames:
        u = u.copy()
        u['weight'] = u['weight'].to_numpy(float) * ref / u['weight'].sum() * s
        u['survey_year'], u['share'] = y, s
        out.append(u)
    return pd.concat(out, ignore_index=True)

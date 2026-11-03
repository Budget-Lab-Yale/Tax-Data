"""survey_mix.py: the years of the CEX, the CPS and the SIPP that a target year's file is drawn from (TARGETS.md
section 7, "the two nearest of their years, weighted by distance").

The shares are written by hand in the target year's configuration, targets_config/<year>/prior.yaml, in the
prior, one entry per survey (the SCF's are the "balance sheet" entry, read by generate.scf_waves):
    consumption: {cex 2019: 0.7311, cex 2017: 0.2689}      the CEX
    cps items:   {cps 2019: 0.7311, cps 2017: 0.2689}      the CPS
    sipp tips:   {sipp 2019: 0.7311, sipp 2017: 0.2689}    the SIPP
This module only reads them; the code does not compute the shares. A survey year here is the tax year, which is
the year the training table and the model use (the SCF's wave of survey year y is its income year y - 1).
"""
import common

ENTRY = {'cex': 'consumption', 'cps': 'cps items', 'sipp': 'sipp tips'}
SOURCES = list(ENTRY)


def prior_shares(year, entry, source):
    """[(year, share)] from one entry of the year's prior (targets_config/<year>/prior.yaml), whose keys are
    '<source> <year>': sorted by year, each share divided by their sum, which must be one."""
    import target_list as TLS
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
    """[(tax year, share)] for the survey in the target year's file, sorted by year, shares summing to one. Every year
    must be one the training table holds for that survey (checked by the callers that read the table)."""
    year = common.TARGET_YEAR if year is None else year
    return prior_shares(year, ENTRY[source], source)


def years(source, year=None):
    return [y for y, _ in mix(source, year)]


# ---- the surveys' units in the year mix (targets and reports) ----------------------------------------------------
import pandas as pd


def growth(source, y):
    """{variable: factor from the survey year y to the target year} for the survey's dollar amounts, the factors the
    dollar maps use (build_table.build_year_maps, written to data/maps/survey_year_growth_factors.csv)."""
    g = pd.read_csv(f'{common.MAPS}/survey_year_growth_factors.csv')
    g = g[(g['survey'] == source) & (g['year'] == y)]
    assert len(g), f'no growth factors for {source} {y} in {common.MAPS}/survey_year_growth_factors.csv'
    return dict(zip(g['variable'], g['factor to the target year'].astype(float)))


def wage_growth(source, y):
    """The factor SOI's wages per return gives, survey year y to the target year (the CPS and SIPP maps' rule)."""
    f = growth(source, y)
    return f['care_exp'] if source == 'cps' else next(iter(f.values()))


# columns of a survey's unit file that are dollars but not map amounts, grown by the wage factor in reports
EXTRA_DOLLARS = {'cps': ['wages_translated', 'wages_primary_reported', 'wages_spouse_reported', 'se_primary_reported',
                         'se_spouse_reported'],
                 'sipp': ['wages_translated', 'wages_primary_reported', 'wages_spouse_reported', 'tips_primary_reported',
                          'tips_spouse_reported'],
                 'cex': []}


def grown_units(source, y):
    """The survey's tax units of year y (data/<survey>_units_<y>_all.parquet) with every dollar column in the target
    year's dollars: the map amounts by the maps' factors, and the unit files' other dollar columns (wages, the
    reported amounts) by SOI's wages per return."""
    u = pd.read_parquet(f'{common.DATA}/{source}_units_{y}_all.parquet')
    f = growth(source, y)
    for c, x in f.items():
        if c in u:
            u[c] = u[c].to_numpy(float) * x
    if source != 'cex':
        wf = wage_growth(source, y)
        for c in EXTRA_DOLLARS[source]:
            assert c in u, f'{source} {y}: no column {c}'
            u[c] = u[c].to_numpy(float) * wf
    return u


def mixed_units(source, year=None):
    """The survey's units over its year mix as one frame, dollars in the target year's, with the column 'share' (the
    year's share) and 'survey_year'. weight is the unit's survey weight times the year's share, with each year's units
    scaled to the same total (the share-weighted mean of the years' totals), as scf_targets scales the SCF's waves, so
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

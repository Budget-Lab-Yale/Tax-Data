"""Builds resources/top/forbes_billionaires_<year>.csv: the US billionaires on Forbes's World's Billionaires list
of the year, one row per entry, from Forbes's own JSON file of the list (the fetch dates, counts and checksums are in
sources.csv). It is run by hand, once per list. A US billionaire is an entry with US citizenship and a net worth of
$1 billion or more. A married couple listed as two entries (SPOUSE_PAIRS, found by reading the entries by hand) is
marked as one joint return. The columns ending in _later come from production's 2025 Forbes profiles and are not
dated to the list's year.

    python -I research/rebuild/pipeline/resources_build/build_forbes.py <year> <forbes json> <production resources/forbes folder> <output csv>
"""
import csv
import datetime as dt
import json
import sys

# Married couples listed as two entries, as (first uri, second uri); a uri is Forbes's id of an entry. In a year where
# both are US entries, each entry names the other in the column joint_with.
SPOUSE_PAIRS = [('stanley-kroenke', 'ann-walton-kroenke'),   # married 1974
                ('eren-ozmen', 'fatih-ozmen'),               # "Ozmen and her husband Fatih"
                ('sehat-sutardja', 'weili-dai'),             # "cofounded ... Marvell Technology with his wife, Weili Dai"
                ('bharat-desai', 'neerja-sethi'),            # "Her husband Bharat Desai is also a billionaire"
                ('matt-hulsizer', 'jenny-just'),             # "cofounded ... Peak6 ... with his now-wife Jenny Just"
                ('richard-uihlein', 'elizabeth-uihlein'),    # "Elizabeth (Liz) and her husband, Richard (Dick)"
                ('stewart-resnick', 'lynda-resnick'),        # one entry to 2022, two from 2023 ("share a fortune")
                ('andrew-cherng-1', 'peggy-cherng'),         # one entry to 2022, two from 2023
                ('mark-jones', 'robyn-jones'),               # one entry 2021-2022, two from 2024
                ('charles-liang', 'sara-liu'),               # "Husband and wife Charles Liang and Sara Liu"
                ('arthur-levine', 'lauren-leichtman'),       # "cofounded ... with his wife Lauren Leichtman"
                ('jay-z', 'beyonce-knowles-carter'),         # "Her husband Jay-Z is also a billionaire"
                ('li-ge', 'zhao-ning')]                      # wife and cofounder; she died in 2023 (a joint return that year)
SPOUSES = {a: b for x, y in SPOUSE_PAIRS for a, b in [(x, y), (y, x)]}
# Entries that name two people: married couples (one joint return) and siblings (one family fortune). A uri counts
# only in a year whose entry names two people (Judy Love's and Jack Dangermond's entries name one person in 2018-2019).
COUPLES = {'stewart-and-lynda-resnick', 'stewart-lynda-resnick', 'do-won-jin-sook-chang', 'andrew-peggy-cherng',
           'judy-love', 'jack-dangermond', 'mark-robyn-jones'}
SIBLINGS = {'hank-doug-meijer'}
# Each list's month, and the US count and total ($ billion) in the data, which the script checks.
MONTH = {2015: 3, 2016: 3, 2017: 3, 2018: 3, 2019: 3, 2020: 4, 2021: 4, 2022: 4, 2023: 4, 2024: 4, 2025: 4, 2026: 3}
COUNTS = {2015: (535, 2564.3), 2016: (541, 2400.1), 2017: (565, 2755.9), 2018: (586, 3098.7), 2019: (607, 3111.0),
          2020: (615, 2948.7), 2021: (724, 4398.4), 2022: (735, 4701.1), 2023: (734, 4489.0), 2024: (813, 5700.8),
          2025: (902, 6750.4), 2026: (991, 8431.4)}
# Forbes's published US count and total ($ trillion; None where not checked), from Forbes's announcement of each
# list. They are printed beside the data's for comparison.
PUBLISHED = {2015: (536, None), 2016: (540, None), 2017: (565, None), 2018: (585, None), 2019: (607, 3.1),
             2020: (614, 2.9), 2021: (724, 4.4), 2022: (735, 4.7), 2023: (735, 4.5), 2024: (813, 5.7),
             2025: (902, 6.8), 2026: (989, 8.4)}


def main(year, src, prod, out):
    """Writes the year's US billionaires to out, from Forbes's JSON src and production's Forbes folder prod."""
    # 1. Keep the US entries worth $1 billion or more, and check their count and total.
    year = int(year)
    d = json.load(open(src))['personList']['personsLists']
    assert all(r['year'] == year and r['month'] == MONTH[year] for r in d), f'not the {MONTH[year]}/{year} list'
    us = [r for r in d if r.get('countryOfCitizenship') == 'United States']
    below = [r['uri'] for r in us if r['finalWorth'] < 1000]
    assert below == (['wilbur-ross-jr'] if year == 2017 else []), f'US entries below $1 billion: {below}'
    us = [r for r in us if r['finalWorth'] >= 1000]
    total = sum(r['finalWorth'] for r in us)
    assert len(us) == COUNTS[year][0] and abs(total / 1e3 - COUNTS[year][1]) < 0.05, (len(us), total)
    # 2. Production's profiles, and each entry's latest net worth in production's 2022-2025 lists.
    prof = {r['uri']: r for r in csv.DictReader(open(f'{prod}/forbes_profiles.csv'))}
    later = {}
    for r in csv.DictReader(open(f'{prod}/forbes_billionaires_2022_2025.csv')):
        if r['forbes_uri'] and (r['forbes_uri'] not in later or int(r['year']) > later[r['forbes_uri']][0]):
            later[r['forbes_uri']] = (int(r['year']), float(r['net_worth']))
    # 3. Classify the entries that name two people, and find the spouses listed as two entries.
    named_two = {r['uri'] for r in us if (' & ' in r['personName'] or ' and ' in r['personName'])
                 and '& family' not in r['personName'].lower()}
    assert named_two <= COUPLES | SIBLINGS, f'entries naming two people not classified: {sorted(named_two - COUPLES - SIBLINGS)}'
    uris = {r['uri'] for r in us}
    joined = {u: v for u, v in SPOUSES.items() if u in uris and v in uris}
    # 4. One row per entry. The public share is the entry's publicly traded equity over its later net worth, at most 1.
    rows = []
    for r in us:
        p = prof.get(r['uri'])
        share, share_year = '', ''
        if p is not None and r['uri'] in later:
            inter, every = float(p['public_equity_interactive'] or 0), float(p['public_equity_all'] or 0)
            pub = inter if inter > 0 else every
            if pub > 0:
                share, share_year = min(1.0, pub / later[r['uri']][1]), later[r['uri']][0]
        bd = r.get('birthDate')
        name = r['personName']
        rows.append({
            'forbes_rank': r['rank'], 'uri': r['uri'], 'name': name, 'net_worth': round(r['finalWorth'] * 1e6),
            'age': r.get('age', ''),
            'birth_date': dt.datetime(1970, 1, 1, tzinfo=dt.timezone.utc).__add__(dt.timedelta(milliseconds=bd)).date().isoformat()
            if bd is not None else '',
            'gender': r.get('gender', ''), 'self_made': r['selfMade'], 'category': r.get('category', ''),
            'industries': ';'.join(r.get('industries') or []), 'source': r.get('source', ''),
            'residence_country': r.get('country', ''), 'residence_state': r.get('state', ''),
            'family_entry': '& family' in name.lower() or r['uri'] in named_two & SIBLINGS,
            'couple_entry': r['uri'] in named_two & COUPLES,
            'joint_with': joined.get(r['uri'], ''),
            'profile_matched': p is not None,
            'marital_status_later': p['marital_status'] if p is not None else '',
            'children_later': p['children'] if p is not None else '',
            'public_share_later': share, 'public_share_later_year': share_year})
    rows.sort(key=lambda x: (-x['net_worth'], x['uri']))
    with open(out, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    pc, pt = PUBLISHED[year]
    print(f'{year}: {len(rows)} US billionaires, ${total / 1e3:,.1f} billion (Forbes published {pc}'
          f'{f", ${pt} trillion" if pt else ""}); couples in one entry {sorted(named_two & COUPLES)}; siblings '
          f'{sorted(named_two & SIBLINGS)}; family entries {sum(r["family_entry"] for r in rows)}; spouses joined '
          f'{sorted({tuple(sorted(x)) for x in joined.items()})}; profiles matched {sum(r["profile_matched"] for r in rows)}; '
          f'public share known {sum(r["public_share_later"] != "" for r in rows)}')


if __name__ == '__main__':
    main(*sys.argv[1:])

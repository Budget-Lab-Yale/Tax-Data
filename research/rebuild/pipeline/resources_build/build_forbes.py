"""Builds resources/top/forbes_billionaires_<year>.csv: the US billionaires on one of Forbes's World's Billionaires
lists (the March or April list of the year), one row per list entry, every year 2015-2026.

Usage (a plain script, run with python -I, the downloaded file passed as an argument, never imported):

    python -I research/rebuild/pipeline/resources_build/build_forbes.py <year> <forbes json> <production resources/forbes folder> <output csv>

<forbes json> is Forbes's own data for the list, from
https://www.forbes.com/forbesapi/person/billionaires/<year>/position/true.json?limit=<n> (fetch dates, entry counts
and SHA-256 in sources.csv). Every entry must carry the year; the month is 3 (2015-2019, 2026) or 4 (2020-2025), the
month Forbes published the list. The 2025 and 2026 lists have more than 3,000 entries, so they were fetched with
limit=5000 (with limit=3000 the list is cut at 3,000 entries).

The US billionaires are the entries whose country of citizenship is the United States and whose net worth is $1
billion or more (Forbes's data keep one 2017 entry, Wilbur Ross, at $0.6 billion, after Forbes found in November 2017
that he was not a billionaire; he is left out). COUNTS gives each year's count and total in the data, checked here,
and PUBLISHED Forbes's own published US count (and total where Forbes gave one) for comparison: the data differ from
the published figures by at most two entries, from Forbes's later corrections.

Columns from the list itself: forbes_rank (the world rank; ties share a rank), uri, name, net_worth (dollars), age and
birth_date (Forbes's, at the list date), gender, self_made, category, industries, source (the fortune's source),
residence_country, residence_state, family_entry (the name says "& family", or two siblings named in one entry, Hank
& Doug Meijer: one fortune for a family, one record), couple_entry (a married couple named in one entry; one joint
return). joint_with: the uri of a spouse listed separately (one joint return).

Spouses: every
same-surname group of US entries, every biography naming another US entry next to a word for a spouse, and every set
of US entries with the same fortune and source (from 2023 Forbes lists a couple's shared fortune as two entries of
half each) were read by hand. SPOUSE_PAIRS are the married couples found; a pair is joined in every year both are on
the list. Not joined: ex-spouses (the Grosses, the Wynns, Bezos and Scott, Gates and French Gates, Koguan and Thai Lee,
Glenn and Penny Sanford, George and Gloria Joseph, Robert and Sheila Johnson) and couples whose divorce was filed before
the list date (Kanye West and Kim Kardashian, filed February 2021; Dan and Jennifer Gilbert, filed September 2025).
COUPLES and SIBLINGS: the entries naming two people.

Columns from production's later Forbes profiles (resources/forbes/forbes_profiles.csv, pulled once at Forbes's latest
date in 2025; NOT dated to the list's year, used because the lists have neither): profile_matched,
marital_status_later, children_later, and public_share_later = the profile's public holdings (Forbes's tracked share
holdings, public_equity_interactive, else public_equity_all) over the person's latest net worth in
forbes_billionaires_2022_2025.csv (the 2025 row, else the latest year present), held to at most 1; empty where the
profile has no public holdings or no later net worth (unknown, not zero).
"""
import csv
import datetime as dt
import json
import sys

# (first uri, second uri): married couples listed as two entries; joined in a year where both are US entries
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
# entries naming two people: married couples (one joint return) and siblings (one family fortune); a uri counts only
# in a year whose entry names two people (Judy Love's and Jack Dangermond's entries name one person in 2018-2019)
COUPLES = {'stewart-and-lynda-resnick', 'stewart-lynda-resnick', 'do-won-jin-sook-chang', 'andrew-peggy-cherng',
           'judy-love', 'jack-dangermond', 'mark-robyn-jones'}
SIBLINGS = {'hank-doug-meijer'}
# the list month, and the US count and total ($ billion) in the data, checked
MONTH = {2015: 3, 2016: 3, 2017: 3, 2018: 3, 2019: 3, 2020: 4, 2021: 4, 2022: 4, 2023: 4, 2024: 4, 2025: 4, 2026: 3}
COUNTS = {2015: (535, 2564.3), 2016: (541, 2400.1), 2017: (565, 2755.9), 2018: (586, 3098.7), 2019: (607, 3111.0),
          2020: (615, 2948.7), 2021: (724, 4398.4), 2022: (735, 4701.1), 2023: (734, 4489.0), 2024: (813, 5700.8),
          2025: (902, 6750.4), 2026: (991, 8431.4)}
# Forbes's published US count and total ($ trillion; None where not checked), from Forbes's announcement of each list
PUBLISHED = {2015: (536, None), 2016: (540, None), 2017: (565, None), 2018: (585, None), 2019: (607, 3.1),
             2020: (614, 2.9), 2021: (724, 4.4), 2022: (735, 4.7), 2023: (735, 4.5), 2024: (813, 5.7),
             2025: (902, 6.8), 2026: (989, 8.4)}


def main(year, src, prod, out):
    year = int(year)
    d = json.load(open(src))['personList']['personsLists']
    assert all(r['year'] == year and r['month'] == MONTH[year] for r in d), f'not the {MONTH[year]}/{year} list'
    us = [r for r in d if r.get('countryOfCitizenship') == 'United States']
    below = [r['uri'] for r in us if r['finalWorth'] < 1000]
    assert below == (['wilbur-ross-jr'] if year == 2017 else []), f'US entries below $1 billion: {below}'
    us = [r for r in us if r['finalWorth'] >= 1000]
    total = sum(r['finalWorth'] for r in us)
    assert len(us) == COUNTS[year][0] and abs(total / 1e3 - COUNTS[year][1]) < 0.05, (len(us), total)
    prof = {r['uri']: r for r in csv.DictReader(open(f'{prod}/forbes_profiles.csv'))}
    later = {}
    for r in csv.DictReader(open(f'{prod}/forbes_billionaires_2022_2025.csv')):
        if r['forbes_uri'] and (r['forbes_uri'] not in later or int(r['year']) > later[r['forbes_uri']][0]):
            later[r['forbes_uri']] = (int(r['year']), float(r['net_worth']))
    named_two = {r['uri'] for r in us if (' & ' in r['personName'] or ' and ' in r['personName'])
                 and '& family' not in r['personName'].lower()}
    assert named_two <= COUPLES | SIBLINGS, f'entries naming two people not classified: {sorted(named_two - COUPLES - SIBLINGS)}'
    uris = {r['uri'] for r in us}
    joined = {u: v for u, v in SPOUSES.items() if u in uris and v in uris}
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

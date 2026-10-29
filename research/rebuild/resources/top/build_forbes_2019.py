"""Builds forbes_billionaires_2019.csv: the US billionaires on Forbes's World's Billionaires list of March 2019 (wealth
valued 8 February 2019), one row per list entry.

Usage (run with python -I, the downloaded file passed as an argument, never imported):

    python -I build_forbes_2019.py <forbes json> <production resources/forbes folder> <output csv>

<forbes json> is Forbes's own data for the list, fetched on 29 October 2026 from
https://www.forbes.com/forbesapi/person/billionaires/2019/position/true.json?limit=3000 (2,153 entries, every one with
year 2019 and month 3; SHA-256 c6e7ebfa3487006225a6b75efae2af28e115a9376e3a978405e62b4864d5bc27). The US billionaires
are the entries whose country of citizenship is the United States: 607 with $3,111.0 billion, Forbes's own published
count and total for the list ("607 US billionaires ... $3.1 trillion"). Residence would give 615.

Columns from the 2019 list itself: forbes_rank (the world rank; ties share a rank), uri, name, net_worth (dollars),
age and birth_date (Forbes's, at the list date), gender, self_made, category, industries, source (the fortune's
source), residence_country, residence_state, family_entry (the name says "& family", or two siblings named in one
entry, Hank & Doug Meijer: one fortune for a family, one record), couple_entry (a married couple named in one entry:
Stewart and Lynda Resnick, Do Won & Jin Sook Chang, Andrew & Peggy Cherng; one joint return).
joint_with: the uri of a spouse listed separately (found by reading every same-surname group's Forbes biographies and
every biography naming another US billionaire as a wife or husband; two couples: Stanley Kroenke and Ann Walton
Kroenke, Eren and Fatih Ozmen). Ex-spouses (the Grosses, the Wynns) are separate units.

Columns from production's later Forbes profiles (resources/forbes/forbes_profiles.csv, pulled once at Forbes's latest
date in 2025; NOT dated 2019, used because the 2019 list has neither): profile_matched, marital_status_later,
children_later, and public_share_later = the profile's public holdings (Forbes's tracked share holdings,
public_equity_interactive, else public_equity_all) over the person's latest net worth in
forbes_billionaires_2022_2025.csv (the 2025 row, else the latest year present), held to at most 1; empty where the
profile has no public holdings or no later net worth (unknown, not zero).
"""
import csv
import datetime as dt
import json
import sys

SPOUSES = {'stanley-kroenke': 'ann-walton-kroenke', 'ann-walton-kroenke': 'stanley-kroenke',
           'eren-ozmen': 'fatih-ozmen', 'fatih-ozmen': 'eren-ozmen'}
COUPLES = {'stewart-and-lynda-resnick', 'do-won-jin-sook-chang', 'andrew-peggy-cherng'}   # married couples in one entry
SIBLINGS = {'hank-doug-meijer'}                                                             # brothers in one entry


def main(src, prod, out):
    d = json.load(open(src))['personList']['personsLists']
    assert all(r['year'] == 2019 and r['month'] == 3 for r in d), 'not the March 2019 list'
    us = [r for r in d if r.get('countryOfCitizenship') == 'United States']
    total = sum(r['finalWorth'] for r in us)
    assert len(us) == 607 and abs(total - 3111000.0) < 1e-6, (len(us), total)
    prof = {r['uri']: r for r in csv.DictReader(open(f'{prod}/forbes_profiles.csv'))}
    later = {}
    for r in csv.DictReader(open(f'{prod}/forbes_billionaires_2022_2025.csv')):
        if r['forbes_uri'] and (r['forbes_uri'] not in later or int(r['year']) > later[r['forbes_uri']][0]):
            later[r['forbes_uri']] = (int(r['year']), float(r['net_worth']))
    named_two = {r['uri'] for r in us if (' & ' in r['personName'] or ' and ' in r['personName'])
                 and '& family' not in r['personName']}
    assert named_two == COUPLES | SIBLINGS, f'entries naming two people: {sorted(named_two)}'
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
        assert r['uri'] not in SPOUSES or SPOUSES[r['uri']] in {x['uri'] for x in us}
        rows.append({
            'forbes_rank': r['rank'], 'uri': r['uri'], 'name': name, 'net_worth': round(r['finalWorth'] * 1e6),
            'age': r.get('age', ''),
            'birth_date': dt.datetime(1970, 1, 1, tzinfo=dt.timezone.utc).__add__(dt.timedelta(milliseconds=bd)).date().isoformat()
            if bd is not None else '',
            'gender': r.get('gender', ''), 'self_made': r['selfMade'], 'category': r.get('category', ''),
            'industries': ';'.join(r.get('industries') or []), 'source': r.get('source', ''),
            'residence_country': r.get('country', ''), 'residence_state': r.get('state', ''),
            'family_entry': '& family' in name or r['uri'] in SIBLINGS, 'couple_entry': r['uri'] in COUPLES,
            'joint_with': SPOUSES.get(r['uri'], ''),
            'profile_matched': p is not None,
            'marital_status_later': p['marital_status'] if p is not None else '',
            'children_later': p['children'] if p is not None else '',
            'public_share_later': share, 'public_share_later_year': share_year})
    rows.sort(key=lambda x: (-x['net_worth'], x['uri']))
    with open(out, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f'{len(rows)} US billionaires, ${total / 1e3:,.1f} billion; profiles matched {sum(r["profile_matched"] for r in rows)}; '
          f'public share known {sum(r["public_share_later"] != "" for r in rows)}')


if __name__ == '__main__':
    main(*sys.argv[1:])

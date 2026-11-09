"""sipp_read.py: the SIPP public-use person-month files, only the columns the pipeline uses (roster_sipp.py).

The files (settings paths.raw.sipp, a folder; sipp.releases maps each reference year to its file) are pipe-delimited,
one row per person and month of the reference year (MONTHCODE 1-12), several GB each, so only the columns below are
read (pyarrow, include_columns) and the result is cached as a parquet file in the run's data folder
(sipp_raw_<year>.parquet); delete the cache to read the file again. The 2014 panel names its columns in mixed case;
every name is read case-blind and stored in upper case. A zipped release (2023: pu2024_csv.zip) is read from the one
CSV inside it.

OPTIONAL columns are read where the release has them: the own elective deferrals at the main
employer (settings deferrals.sipp_item, TSCNTAMT_401, in the 2022 and 2024 releases only). A cached file that lacks an
optional column the release has is read again.

SHARED columns (roster_sipp.py, the units' dividends, gross Social Security and pensions), required
in every release (all five have them): income from stocks and mutual funds (TINC_STMF, a yearly amount repeated in
every month), retirement income items 1-8 (TRET1AMT-TRET8AMT, monthly; TSSSAMT, monthly Social Security, is in INCOME
already) and the questions that decide who is asked each amount, which explain its missing values: received Social
Security for oneself this month (ESSSMNYN), received retirement income item k this month (ERET1MNYN-ERET8MNYN), and
owned stocks or mutual funds in the year, alone, jointly with the spouse or jointly with others (EOWN_ST, EOWN_MF,
EJSOWNST, EJSOWNMF, EJOOWNST, EJOOWNMF). A cached file that lacks one of them is read again.
"""
import os
import zipfile
import gzip

import pandas as pd
import pyarrow as pa
import pyarrow.csv as pcsv

from shared import common

PERSON = ['SSUID', 'PNUM', 'MONTHCODE', 'ERESIDENCEID', 'THHLDSTATUS', 'SPANEL', 'SWAVE', 'ERELRPE', 'ESEX', 'TAGE',
          'TAGE_EHC', 'EMS', 'EMS_EHC', 'EPNSPOUSE', 'EPNSPOUS_EHC', 'EPNCOHAB', 'EPNCOHAB_EHC', 'EPNPAR1', 'EPNPAR2',
          'EPAR1TYP', 'EPAR2TYP', 'EDEPCLM', 'EFSTATUS', 'WPFINWGT', 'RENROLL', 'EEDGRADE', 'RDIS', 'RFAMNUM',
          'RFAMREF']
INCOME = ['TPEARN', 'TSSSAMT', 'TSSI_AMT', 'TUC1AMT', 'TUC2AMT', 'TPPRPINC', 'TPOTHINC', 'TPTOTINC']
JOBS = 7            # job lines 1-7: earnings (MSUM), industry (IND) and class of worker (CLWRK)
TIP_JOBS = 4        # tips (TXAMT) are asked of job lines 1-4 only (5-7 are suppressed), as in production's annualize.R
JOB_COLS = [f'TJB{j}_MSUM' for j in range(1, JOBS + 1)] + [f'TJB{j}_IND' for j in range(1, JOBS + 1)] + \
    [f'EJB{j}_CLWRK' for j in range(1, JOBS + 1)] + [f'TJB{j}_TXAMT' for j in range(1, TIP_JOBS + 1)]
RET_ITEMS = 8       # retirement income items 1-8 (company or union, federal, state, local, military, railroad, reserve, other)
OWNERSHIP = ['EOWN_ST', 'EOWN_MF', 'EJSOWNST', 'EJSOWNMF', 'EJOOWNST', 'EJOOWNMF']
SHARED = ['TINC_STMF'] + [f'TRET{k}AMT' for k in range(1, RET_ITEMS + 1)] + \
    ['ESSSMNYN'] + [f'ERET{k}MNYN' for k in range(1, RET_ITEMS + 1)] + OWNERSHIP
COLUMNS = PERSON + INCOME + JOB_COLS + SHARED
OPTIONAL = [common.SETTINGS['deferrals']['sipp_item']]   # read where the release has them
STRINGS = ['SSUID', 'ERESIDENCEID'] + [f'TJB{j}_IND' for j in range(1, JOBS + 1)]


def release_path(year):
    S = common.SETTINGS['sipp']
    return os.path.join(common.RAW['sipp'], S['releases'][str(year)])


def _open(path):
    """A binary stream of the release's CSV (plain, gzipped or the one CSV in a zip)."""
    if path.endswith('.zip'):
        z = zipfile.ZipFile(path)
        names = [n for n in z.namelist() if n.lower().endswith('.csv')]
        assert len(names) == 1, (path, names)
        return z.open(names[0])
    if path.endswith('.gz'):
        return gzip.open(path, 'rb')
    return open(path, 'rb')


def read_release(year, cache=True):
    """The person-month rows of the release for reference year `year`, the columns COLUMNS (upper case), numbers as
    floats (missing = NaN), SSUID, the residence id (letters in the 2014 panel) and industry codes as strings."""
    out = f'{common.DATA}/sipp_raw_{year}.parquet'
    path = release_path(year)
    with _open(path) as f:
        header = f.readline().decode().rstrip('\r\n').split('|')
    by_upper = {h.upper(): h for h in header}
    want = COLUMNS + [c for c in OPTIONAL if c in by_upper]
    if cache and os.path.exists(out):
        d = pd.read_parquet(out)
        if all(c in d.columns for c in want):
            return d
        common.say(f'SIPP {year}: the cached {os.path.basename(out)} lacks {[c for c in want if c not in d.columns]}; '
                   'reading the release again')
    missing = [c for c in COLUMNS if c not in by_upper]
    assert not missing, f'SIPP {year} ({path}): columns the pipeline needs are not in this release: {missing}'
    names = [by_upper[c] for c in want]
    types = {by_upper[c]: (pa.string() if c in STRINGS else pa.float64()) for c in want}
    with _open(path) as f:
        tab = pcsv.read_csv(f, parse_options=pcsv.ParseOptions(delimiter='|'),
                            read_options=pcsv.ReadOptions(block_size=1 << 26),
                            convert_options=pcsv.ConvertOptions(include_columns=names, column_types=types,
                                                                strings_can_be_null=True))
    d = tab.to_pandas()
    d.columns = [c.upper() for c in d.columns]
    common.say(f'SIPP {year} ({os.path.basename(path)}): {len(d):,} person-month rows, '
               f'{d.groupby(["SSUID", "PNUM"]).ngroups:,} people')
    if cache:
        os.makedirs(common.DATA, exist_ok=True)
        d.to_parquet(out, index=False)
    return d

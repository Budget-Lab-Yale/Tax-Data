# CEX Pipeline Improvement TODO

Tracking decisions and implementation status for improvements identified in the
GPT vs. current pipeline comparative review (2026-03-25).

---

## 1. MO_SCOPE Weighting

**Status**: Approved — implement as described

**Problem**: All 5 quarters of Interview data are weighted equally via raw
`FINLWT21`, but boundary-quarter interviews have reference months that fall
outside the target calendar year. A January 2023 interview references Oct–Dec
2022 (0 months in 2023); a February 2023 interview references Nov 2022–Jan 2023
(1 month in 2023). Without MO_SCOPE, these get full weight, biasing aggregate
consumption estimates by ~2–5% and systematically overweighting winter spending.

**Fix**: Compute MO_SCOPE from `QINTRVMO` and `QINTRVYR`, then derive
`WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3)`. Replace raw `FINLWT21` with
`WT_ANNUAL` in:
1. Weighted percentile construction (line ~269–271)
2. Bootstrap sampling (line ~335)

MO_SCOPE logic for target year t:
```r
MO_SCOPE = case_when(
  QINTRVYR == t   & QINTRVMO <= 1  ~ 0L,
  QINTRVYR == t   & QINTRVMO == 2  ~ 1L,
  QINTRVYR == t   & QINTRVMO == 3  ~ 2L,
  QINTRVYR == t   & QINTRVMO >= 4  ~ 3L,
  QINTRVYR == t+1 & QINTRVMO == 1  ~ 3L,
  QINTRVYR == t+1 & QINTRVMO == 2  ~ 2L,
  QINTRVYR == t+1 & QINTRVMO == 3  ~ 1L,
  TRUE                              ~ 0L
)
```

Interviews with MO_SCOPE = 0 effectively drop out (zero weight).

**Decision**: Yes, fix as described. (2026-03-25)

---

## 2. Equivalence-Scale CU-to-TU Allocation

**Status**: Skipping for now

**Problem**: `CU_pct = tu_size / FAM_SIZE` splits all consumption categories by
headcount. This misallocates shared goods (housing, utilities) in multi-TU CUs.

**Why skipping**: The allocation concept is consumption, not spending. Shared
goods are consumed jointly — both people in a household benefit equally from
housing services regardless of who pays. Any CU→TU allocation of a shared good
(headcount, equivalence scales, income share) is inherently modeling a mixed
population of standalone vs. cohabiting tax units, and the QRF can't distinguish
between them. Equivalence scales shift numbers but don't solve the fundamental
problem. Multi-TU CUs are ~15–20% of the sample; the 80%+ of single-TU CUs
(where CU_pct = 1) provide the dominant training signal.

**Revisit if**: We add features that distinguish living arrangements (e.g.,
`lives_with_parents`, `multi_tu_cu` flag), or move away from QRF to a model
that can condition on household structure. (2026-03-25)

---

## 3. Health Care Classification (External Proportions)

**Status**: Skipping — subsumed by #5 (UCC-level MTBI)

**Problem**: HEALTHCQ maps 100% to `health_care`, but PCE splits it across
health care (provider services), other_nondurables (drugs), other_durables
(therapeutic equipment), and financial_insurance (net health insurance).
HEALTHCQ has no FMLI sub-variables — the second Claude's claim that HLTHINCQ,
MEDSRVCQ, PREDRGCQ, MEDSUPCQ exist was incorrect.

**Why skipping**: An external-proportions fix is approximate and doesn't capture
household-level variation. The real fix requires UCC-level MTBI data (#5). If we
pursue #5, the health split comes for free. (2026-03-25)

---

## 4. Vehicle Insurance Split

**Status**: Skipping — subsumed by #5 (UCC-level MTBI)

**Problem**: VEHINSCQ maps 100% to `financial_insurance`, but BLS splits motor
vehicle insurance across motor_vehicles (collision claims for parts),
transport_services (repair labor claims), health_care (medical claims), and
financial_insurance (net margin only). Fixing precisely requires UCC-level data
or external proportions. If we pursue #5, this comes for free. (2026-03-25)

---

## 5. UCC-Level MTBI Integration

**Status**: Approved — requires standalone implementation plan before starting

**Problem**: FMLI sub-variables are a lossy compression of underlying UCC detail.
Health, vehicle insurance, and other durables cross PCE category boundaries at
the FMLI level. Fixes #3 (health split) and #4 (vehicle insurance split).

**Scope**:
- Read MTBI quarterly files (same 5-quarter pattern as FMLI/MEMI)
- Build UCC→detailed PCE concept→20 broad category bridge CSV (~1,000 UCCs)
- Aggregate MTBI COST by NEWID + PCE category (monthly→quarterly)
- Replace FMLI sub-variable consumption formulas with MTBI-based aggregates
- Exclude gifts (GIFT flag)
- Still read FMLI for weights, demographics, tech variables

**Conceptual decisions** (few, mostly settled):
1. UCC→PCE mapping: ~95% mechanical from BLS hierarchical groupings; ~5% edge
   cases (insurance splits, FISIM, NPISH) already discussed
2. Monthly→quarterly: sum COST where REF_YR/REF_MO in CQ window
3. Gift exclusion: yes, standard

**Engineering**: Mostly mechanical coding and bridge table construction. The
bridge CSV is tedious but not ambiguous. An agent can build the initial version
from BLS published tables; user reviews edge cases.

**Decision**: Approved. Create a standalone plan before implementation — this is
substantially more involved than the other items. (2026-03-25)

---

## 6. Diary Integration

**Status**: Skipping

**Problem**: ~6% of PCE spending is diary-only or diary-preferred (detailed food,
clothing, personal care). Pipeline uses Interview only.

**Why skipping**: TPC doesn't use Diary data. Diary-preferred items are already
captured (noisier) in FMLI Interview aggregates. NIPA benchmarking corrects
levels. Full diary integration requires new file pipeline (FMLD, EXPD), donor
matching, weekly→quarterly conversion — high effort, low marginal gain for QRF
training on broad categories. (2026-03-25)

---

## 7. EXPN Detail Files

**Status**: Skipping

**Problem**: Annual detail files (CLA, IHB, VEQ, etc.) provide person-specific
expenditure records that enable precise CU→TU allocation.

**Why skipping**: Person-level allocation (#2) is skipping — shared goods are
consumed jointly and the QRF signal is dominated by single-TU CUs. Without a
use case for person-level allocation, EXPN files have no purpose in the
pipeline. (2026-03-25)

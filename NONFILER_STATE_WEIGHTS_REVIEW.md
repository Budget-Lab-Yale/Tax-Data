# Nonfiler and State-Weight Improvements: Review and Recommended Workplan

**Review date:** 2026-08-30  
**Repositories reviewed:** `Tax-Data` and `Tax-Simulator`  
**Tax-Data review point:** `asec-nonfiler-pool` at `981a69525f1a940888c5fe314d17d37ef64b5e59`  
**Tax-Simulator review point:** `state-tax` at `064b54f672021a75e41959bbb85695b51e006a1b`

## Executive decision

The proposed process is a substantial improvement over the prior system. Replacing the static DINA nonfiler append with an ASEC-based annual population makes the construction more transparent, gives the model observed household composition, and permits annual changes in ages, dependents, earnings, and income-source incidence. The state-weight research also demonstrates that the existing calibration machinery can fit a large set of targeted margins closely.

The implementation is not ready to merge or support production state estimates. Four cross-repository failures block the handoff:

1. `Tax-Data` does not register the `ASEC-Nonfilers` dependency in the active baseline runscript.
2. The `Tax-Simulator` output schema does not satisfy the `Tax-Data` consumer contract.
3. `Tax-Data` reads only the 2017 pool, so it does not implement the decision to rebuild the population annually through the observed-data ceiling.
4. The 2023 Social Security-area alignment is calculated as a diagnostic but is not applied to the published pool or downstream targets.

Federal validation has also not been run. Its configurations and preflight script exist only as proposed documentation. State weights remain a research candidate: the preferred configuration fits targeted margins well, but its held-out error is large, its saved scratch artifacts are gone, and the production runtime still assigns uniform state shares.

The recommended path is therefore sequential. First repair and test the nonfiler data contract. Next complete and validate the federal population. Only then rebuild state targets, refit the estimator, test state liabilities, and replace the uniform production weights.

## Scope and review standard

This review covers four linked questions:

- **Code:** Does each repository implement the documented design, and does the cross-repository interface run?
- **Documentation:** Can a new contributor determine what was decided, what was implemented, and what remains open?
- **Plan:** Are the proposed next steps in the right order, with testable completion criteria?
- **Economics:** Do the population universes, filing-status definitions, calibration targets, and validation exercises support the intended tax-policy estimands?

The review used the current branches, commit history, research notes, decision logs, handoff documents, source code, targeted R tests, static parsing, and live configuration checks. Existing uncommitted documentation changes in `Tax-Simulator` were preserved.

## Intended end-to-end process

The documents and code imply the following target architecture:

1. `Tax-Simulator` constructs ASEC tax units and classifies required filers and modeled voluntary nonfilers.
2. It adds the group-quarters population, calibrates the pool to national nonfiler controls, and emits a PUF-compatible pool for each observed year.
3. At the 2023 handoff, each primary-age band is aligned to the CBO Social Security-area population universe. Later years are aged from that aligned base.
4. `Tax-Data` combines filers with the corresponding annual nonfiler pool. It projects only beyond the observed-data ceiling and applies filer-only extensive factors when the numerator is a returns universe.
5. `Tax-Simulator` derives state residual targets after the accepted federal pool is fixed.
6. The state-weight estimator splits each national record across states, using a demographic prior and calibrated state margins.
7. Targeted margins, held-out margins, effective sample sizes, weight concentration, and downstream state tax liabilities determine whether the weights are acceptable.
8. A versioned state-weight artifact replaces the current uniform state assignment in production.

This architecture is coherent. The present code implements only parts of steps 1–4 and research versions of steps 5–7.

## Status of the existing work

| Workstream | Work completed | Remaining issue | Readiness |
|---|---|---|---|
| S13–S19 nonfiler research | ASEC households, group quarters, filing model, calibration, annual projections, acceptance diagnostics, and 2023 universe analysis | Several decisions do not reach the emitted or consumed artifact | Research complete; handoff incomplete |
| `Tax-Data` nonfiler branch | DINA append replaced, weight-target resource added, filer-only extensive-factor denominator implemented | Missing dependency, incompatible schema, 2017-only consumption, S19 scale absent | Not merge-ready |
| Federal validation | Detailed runbook and proposed tripwires | Test runscripts and preflight program are absent; no results exist | Not started operationally |
| State-weight Phase 1 | Preferred configuration fits 95.3% of targeted margins within 2%; targeted MARD is about 0.43% | Held-out MARD is about 22.3%; weak income sources persist; fit artifacts were deleted | Promising research result |
| State-weight production | Loader and estimator scaffolding exist | Runtime still uses uniform shares; accepted configuration is not encoded | Not implemented |
| Documentation | Plan, decision log, findings, design, status, and handoff documents exist | Sources of truth conflict and some statements are stale | Needs consolidation |

## Findings that block the nonfiler handoff

### 1. The interface is registered but not configured

`Tax-Data/config/interfaces/interface_versions.yaml` declares `ASEC-Nonfilers`, and `Tax-Data/src/impute_nonfilers.R` dereferences it. `Tax-Data/config/runscripts/baseline.yaml` does not provide a vintage and scenario for that interface. The configuration layer therefore returns a zero-length path, and the subsequent `read_csv()` cannot load the pool.

**Required change**

- Add an exact `ASEC-Nonfilers` vintage and scenario to every runscript that reaches `impute_nonfilers.R`.
- Add a configuration assertion that each required interface resolves to exactly one directory.
- Check that the expected manifest and requested annual pool files exist before reading any data.
- Keep the interface version pinned. Do not make the baseline silently consume the latest published artifact.

**Acceptance gate**

A clean baseline configuration must resolve one pinned nonfiler interface, find its manifest, and enumerate every required annual file before the simulation starts.

### 2. The producer and consumer disagree about the schema

`Tax-Simulator/research/state_weights/nonfiler_pool/05_emit_pool.R` writes `qual_div`, `source`, and `tax_year`. `Tax-Data/src/impute_nonfilers.R` stops when it sees any column absent from the PUF. The `Tax-Data` schema uses `div_pref` rather than `qual_div`. The producer's claim that it emits a PUF-schema file is therefore not true under the consumer's own check.

The current contract also relies on broad zero-filling. That approach is safe only for variables intentionally unavailable for nonfilers. It is unsafe when a new substantive field disappears because of a rename or upstream regression.

**Required change**

- Create a versioned schema manifest in the producer. At minimum, record the column name, storage type, unit, sign convention, source, and whether a missing variable may be zero-filled.
- Rename `qual_div` to the exact downstream name after confirming its tax meaning. If the intended field is preferential-rate dividends, the name should match `div_pref` throughout both repositories.
- Move provenance fields such as `source`, `tax_year`, construction vintage, and calibration version to an allowed metadata block or sidecar manifest. Alternatively, make the consumer whitelist and then drop them explicitly.
- Divide downstream variables into three sets: required and populated; permitted to be missing and zero-filled; forbidden extras.
- Add a contract test that emits a small pool and runs the actual `Tax-Data` schema validation.

**Acceptance gate**

The published sample file must load through `impute_nonfilers.R` without manual edits. Every populated economic variable must retain its intended name, type, unit, and sign. The test must fail if a required field is renamed, omitted, duplicated, or silently zero-filled.

### 3. Annual population reconstruction is not consumed

Decision S18(c) calls for rebuilding the nonfiler pool annually through the observed-data ceiling and aging only beyond that ceiling. `Tax-Data/src/impute_nonfilers.R` reads `nonfiler_pool_2017.csv.gz` once. `Tax-Data/src/project_puf.R` then changes the weights on those 2017 records for later years.

This implementation preserves none of the annual changes in household composition, ages, spouse characteristics, dependents, earnings, or income-source incidence. It reduces the new system to a better 2017 starting pool plus weight growth, which is materially narrower than the adopted design.

**Required change**

- Publish one accepted pool for every intended observed tax year, not just the two years used during development.
- Add a `load_nonfiler_pool(tax_year)` interface in `Tax-Data`.
- For years through the observed ceiling, replace the nonfiler cross-section with the year-specific pool after projecting the filer population.
- For years after the ceiling, project the last observed nonfiler pool using documented factors.
- Decide how stochastic draws attach to changing annual cross-sections. The current positional random-number binding is fragile. Prefer deterministic draws keyed by the annual record identifier and tax year.
- Make identifiers unique within an annual file and unambiguous across years. Do not imply longitudinal identity when annual ASEC cross-sections represent different sampled households.

**Acceptance gate**

For every observed year, the simulation must report the same weighted age-band totals, household characteristics, and income aggregates as the published annual pool. A test must show that changing the annual input file changes composition, not only total weights.

### 4. S19 alignment is not part of the artifact

`Tax-Simulator/research/state_weights/nonfiler_pool/15_ssarea_alignment.R` calculates 2023 primary-age-band scale factors of roughly 1.061–1.134. The script writes diagnostics but does not apply the factors to an emitted pool. The current `Tax-Data` target resource was produced before this final alignment decision.

This leaves the federal handoff between two universes: the calibrated ASEC/administrative residual and the CBO Social Security-area population used for longer-run projections.

**Required change**

- State the precise source and population universe for each side of the 2023 identity: filers, nonfilers, dependents, group quarters, and the Social Security-area total.
- Apply each age-band scale exactly once, preferably in the upstream publication step.
- Write pre-scale and post-scale counts, scale factors, target counts, and residuals to a machine-readable audit table.
- Mark the resulting file as the 2023 handoff pool. Later projections must begin from the aligned weights.
- Add a downstream assertion that filer plus nonfiler adults close to the handoff universe by band and in total.

**Acceptance gate**

The 2023 audit table must close every age band and the total to a predeclared numerical tolerance. The producer and consumer reports must agree exactly after rounding, and no downstream code may apply the scale again.

## Additional code and economic findings

### Preserve observed spouse sex

The emitter publishes observed `male2` for joint ASEC units. `Tax-Data/src/imputations/demographics.R` then overwrites it with a random rule that assumes 1% of marriages are same-sex. This reverses part of S14 and introduces simulation noise into credit eligibility, mortality, and consumption calculations.

Preserve nonmissing upstream values and impute only records that lack spouse sex. Add a fixed-seed test proving that an observed ASEC value survives demographic processing unchanged.

### Treat the filing requirement as an estimated proxy

`Tax-Simulator/src/data/asec_tax_units.R` sets `must_file` when `abs(se_income) >= 400`. A business loss of at least $400 in absolute value therefore triggers filing. IRS guidance instead refers to net self-employment earnings of $400 or more. The implementation also uses signed net business income in gross income and omits available positive capital gains from the main threshold measure. The Form 1040 definition includes gains but not losses and uses business gross income rather than net profit for the ordinary gross-income test.

ASEC does not contain all variables needed to reproduce the legal filing requirement. The model should not present this field as exact.

**Required change**

- Replace the absolute-value self-employment trigger with the economically correct positive-net-earnings rule.
- Include positive capital gains in the threshold proxy when the source variable exists.
- Prevent business losses from reducing gross income used for the ordinary threshold.
- Rename or document `must_file` as a filing-requirement proxy wherever exact legal classification is not possible.
- Produce a sensitivity table showing how these changes affect nonfiler counts, wages, self-employment income, capital gains, and the 65-and-older population.
- Add tests for positive self-employment income, small positive income, large negative income, capital-gain-only income, dependent thresholds, and age-specific thresholds.

Primary sources: [IRS Self-Employed Individuals Tax Center](https://www.irs.gov/businesses/small-businesses-self-employed/self-employed-individuals-tax-center) and [2022 Form 1040 Instructions](https://www.irs.gov/pub/irs-prior/i1040gi--2022.pdf).

### Verify all filing-model constraints

The filing model applies a mean-income tilt and then re-solves the probability level. Its convergence test includes the categorical shares and total count, but not the achieved mean income. The final assertion checks only count and probability bounds.

Add the achieved mean and residual to every iteration report, the stopping condition, the output margin table, and the final assertions. If the probability cap makes the mean unreachable, record that as a failed or explicitly waived target rather than reporting all eight constraints as satisfied.

### Reconsider the state-weight KL penalty

The gradient estimator minimizes target error plus an unweighted row-level KL penalty. The target gradient incorporates national record weights, while the KL term assigns equal cost to moving every sampled row. A high-weight record can therefore move more cheaply per represented person than a low-weight record.

If the intended estimator is a calibration-weight entropy estimator for `W_is = w_i P_is`, the regularizer should normally be proportional to `sum_i w_i KL(P_i || P0_i)`. Normalize the base weights before applying `beta` so the tuning scale is stable. If the unweighted penalty is intentional, describe it as a row-level assignment prior and remove the claim of direct Deville–Särndal equivalence.

The preferred configuration must be rerun under both formulations. Compare target fit, held-out fit, effective sample size, maximum state share, weight tails, and tax liabilities. Do not select `beta` from target fit alone.

### Fix the validation measures before using them

`Tax-Simulator/src/data/post_processing/summary_stats.R` constructs `n_nonfilers` but excludes it from the selected demographic outputs. Its `n_adults` formula assigns one adult to a joint filer and zero adults to a single filer. Correct the formula to represent one primary adult plus a spouse for joint units, then decide whether the metric should cover all tax units or only filers.

The federal validation harness must test the summary-statistic definitions on small hand-calculated units before comparing large aggregates.

### Resolve credit-induced filing explicitly

The CTC code promotes only some nonfilers to filer status, and no corresponding EITC filing transition exists. A nonfiler with positive earned income can receive refundable credit without entering filer-gated totals. The ASEC pool contains more nonfiler wages than the former DINA append, so these edge cases may become economically important.

Measure the baseline frequency and dollar value of each case. Then make a policy decision for CTC, EITC, and stimulus-related credits. Keep this decision separate from the data-source replacement so the federal A/B can attribute changes correctly.

### Complete or revise S14

S14 says DINA leaves `Tax-Data`, but filer-side DINA sex distributions remain in `Tax-Data/src/imputations/demographics.R`, and the interface remains configured. Either replace that consumer with the documented W-2/ASEC approach or revise S14 to say that DINA leaves only the nonfiler construction. The decision log should distinguish a design decision from completed implementation.

### Audit the long-run extensive factor

Restricting the extensive-factor denominator to filers is correct while its numerator measures returns. The CBO 1040 resource ends in 2036, after which the factor falls back to a broader macro series. Document the 2036 estimand change and test whether a filer-only denominator remains consistent with the far-horizon numerator.

## Economic interpretation and validation requirements

### Population-universe accounting

The process combines IRS returns, CBO projections, ASEC households, ACS group quarters, PEP or related population controls, and a Social Security-area universe. These sources do not automatically describe the same people.

Create one universe-crosswalk table with the following columns:

| Input or target | Unit | Geography | Includes dependents? | Includes group quarters? | Residency concept | Filing concept | Years used | Transformation |
|---|---|---|---|---|---|---|---|---|
| IRS return counts | Return/tax unit | Filing jurisdiction | Document | Document | Filing address | Observed return | Historical | None or projection |
| CBO 1040 projection | Return/tax unit | United States | Document | Document | Document | Projected return | Projection years | Growth factors |
| ASEC pool | Constructed tax unit | Residence | Yes | No | Survey residence | Modeled | Observed years | Unit construction and calibration |
| ACS group quarters | Person/unit | Residence | As constructed | Yes | Survey residence | Modeled | Observed years | GQ construction |
| Social Security area | Person/adult | SS area | Document | Document | SS-area concept | None | 2023 onward | Handoff scaling |
| State residual targets | Person, return, or dollars | State | Varies | Varies | Must be explicit | Mixed | Historical/projection | Federal residual and state shares |

No calibration should proceed until each row has a clear definition and the accounting identities reconcile those definitions.

### Filing selection

Nonfiler status is partly modeled rather than observed. The filing proxy determines which households enter the pool, so it affects both population totals and the joint distribution of income, age, family structure, and credit eligibility. Report the effect of each filing-rule correction on those distributions. A stable total count is not enough if the composition changes materially.

### Age calibration estimand

The current calibration assigns a joint unit to the primary person's age band. This is an acceptable tax-unit convention, but it does not identify the age distribution of all adults. Label the target as a **primary-age-band tax-unit control**. Report spouse age and the age distribution of all represented adults as held-out diagnostics, especially before simulating state policies tied to age or retirement income.

### State-weight estimand

The state procedure creates a benchmarked synthetic allocation. It does not recover each national record's true state. Interpret state estimates as calibrated aggregates whose accuracy depends on the target set, prior, regularization, and remaining within-cell heterogeneity.

The old ACS adult shares by age and income tier should not remain the default production target merely because the code already supports them. Implement the adopted residual anchors first. Use weak or conceptually mismatched margins as priors or held-out tests, not hard targets.

### Target fit versus external validity

The preferred research configuration attains a targeted MARD near 0.43%, but overall held-out MARD remains around 22.3%. This gap shows that the estimator can satisfy its constraints; it does not yet show that it assigns untargeted income or tax bases correctly.

The validation report should include:

- Equal-cell absolute relative error.
- Population- or dollar-weighted absolute relative error.
- State-level errors for wages, pensions, Social Security, business income, capital gains, and other policy-relevant bases.
- QWI age-by-sex margins corrected to residence using LODES or a documented alternative.
- ACS marital-status-by-age margins.
- Effective sample size, maximum record shares, weight quantiles, and the number of binding or structurally impossible cells.
- State tax liabilities and major credits for a diverse set of states, not only Illinois, Colorado, and New York.
- Replicate- or jackknife-based uncertainty for state aggregates.

### Loss function and policy importance

The current relative-error objective gives a small target cell similar influence to a large target cell. Retain the unweighted diagnostics because small groups matter, but add population-, income-, and liability-weighted loss measures. Select the production estimator using a predeclared combination of statistical fit, weight stability, and policy relevance.

## Recommended implementation plan

### Phase 0: Freeze definitions and ownership

**Repository:** both  
**Purpose:** prevent further code from embedding conflicting population concepts.

- [ ] Name `plan.md` as the authoritative work tracker.
- [ ] Add the universe crosswalk described above.
- [ ] Mark every S13–S19 decision as `proposed`, `implemented upstream`, `implemented downstream`, `validated`, or `superseded`.
- [ ] Decide whether S14 removes DINA entirely or only from nonfiler construction.
- [ ] Decide the exact observed-data ceiling and the years that must be published.
- [ ] Decide whether S19 scaling occurs upstream or downstream; upstream publication is preferred.
- [ ] Decide how annual record IDs and random draws work with repeated cross-sections.
- [ ] Record the filing-proxy definition and the intended treatment of credit-induced filing.

**Deliverable:** an updated decision log and universe crosswalk with no unresolved ownership questions.  
**Gate:** code work may proceed when each transformation has one named repository and one authoritative artifact.

### Phase 1: Establish the data contract

**Primary repository:** `Tax-Simulator`; consumer tests in `Tax-Data`

- [ ] Add a versioned nonfiler schema manifest.
- [ ] Reconcile `qual_div`/`div_pref` and every other producer/consumer name.
- [ ] Separate data columns from provenance metadata.
- [ ] Emit all required annual sample files with deterministic build metadata.
- [ ] Add a small fixture that contains a joint unit, dependents, positive and negative business income, retirement income, Social Security, dividends, and group quarters.
- [ ] Run the fixture through the actual `Tax-Data` reader.
- [ ] Add `ASEC-Nonfilers` to the baseline and validation runscripts with a pinned vintage.

**Deliverables:** schema manifest, annual-file manifest, producer fixture, consumer contract test.  
**Gate:** the published fixture loads without dropped, renamed, duplicated, or unintentionally zero-filled variables.

### Phase 2: Complete the federal population implementation

**Repositories:** both

- [ ] Correct and relabel the filing-requirement proxy.
- [ ] Add the missing achieved-mean constraint check.
- [ ] Preserve observed spouse sex.
- [ ] Build and publish annual pools through the observed ceiling.
- [ ] Apply S19 age-band alignment to the 2023 handoff artifact.
- [ ] Load the corresponding annual pool in `Tax-Data`.
- [ ] Project only beyond the observed ceiling.
- [ ] Add annual accounting tables for rows, weighted units, adults, dependents, income totals, and age bands.

**Deliverables:** accepted annual pools, 2023 alignment audit, annual `Tax-Data` integration report.  
**Gate:** producer and consumer totals agree for each observed year, and every 2023 band closes to the declared handoff universe.

### Phase 3: Run federal validation before state refitting

**Repositories:** test configuration in `Tax-Data`; reporting in `Tax-Simulator/research/state_weights`

Create the missing executable artifacts referenced by the runbook:

- `config/runscripts/tests/nonfiler_ab.csv` or the repository's canonical runscript format.
- `config/runscripts/tests/nonfiler_cbo.csv`.
- `research/state_weights/nonfiler_residual/05_preflight_vintage.R`.
- A machine-readable results table and a short signed findings report.

Run the comparison in stages:

1. Reproduce the common starting baseline.
2. Apply only the filer-weight and filer-only extensive-factor corrections.
3. Hold those corrections fixed and replace DINA nonfilers with ASEC nonfilers.
4. Apply annual-pool and S19 changes.
5. Evaluate CTC, EITC, and other filing-transition decisions separately.

At minimum, compare:

- Return and nonfiler counts by year and age band.
- Adults, dependents, and filing-status composition.
- Wages, self-employment, pensions, Social Security, interest, dividends, rents, capital gains, and other income.
- AGI, taxable income, individual income tax, payroll tax, and after-tax income.
- CTC, refundable CTC, EITC, and 2021 stimulus-related outcomes.
- Accounting identities and the exact contribution of each intended filer-side correction.

**Provisional acceptance rules**

- Accounting identities and configured calibration constraints close within programmed tolerances.
- No unexplained change appears in filer-only aggregates when only the nonfiler source changes.
- Every material federal change has a source: population count, composition, income mapping, filing behavior, projection, or policy transition.
- Credit totals and recipient counts receive an explicit sign-off for 2021 and one normal-policy year.
- Large deviations from IRS or CBO benchmarks are either corrected or accepted in the decision log with an economic explanation.

**Gate:** sign the federal pool before rebuilding any state residual target. State residuals calculated from an unaccepted federal population are disposable work.

### Phase 4: Rebuild state targets

**Primary repository:** `Tax-Simulator`

- [ ] Add the missing SSA workbooks for 2014–2016 or document an alternative source.
- [ ] Implement the corrected federal residual basis and residence concepts.
- [ ] Use the adopted state anchors as targets.
- [ ] Demote weak income-tier margins to the prior when the design calls for that treatment.
- [ ] Hold 2022 state shares after 2022 only where no later source exists, and label those values as projections.
- [ ] Identify and report structural zeros and unreachable cells before fitting.
- [ ] Version the full target table with source citations and transformation metadata.

**Deliverables:** state-target artifact, source manifest, pre-fit structural-cell report.  
**Gate:** every target has a defined universe, year, state concept, numerator, denominator, source, and transformation.

### Phase 5: Correct, reproduce, and select the state estimator

**Primary repository:** `Tax-Simulator`

- [ ] Encode every experimental configuration in a versioned configuration file, including seed, prior, target set, `beta`, learning rate, schedule, iteration count, and stopping rules.
- [ ] Add national-weighted KL as a candidate specification.
- [ ] Rerun the former config 7 under the final targets and both KL definitions.
- [ ] Save fit histories, target residuals, held-out residuals, ESS, weight-tail diagnostics, convergence status, and runtime.
- [ ] Test sensitivity to seeds and tuning parameters.
- [ ] Preserve accepted artifacts outside scratch storage.

Pre-register final acceptance thresholds before comparing the new candidates. A reasonable starting point is:

- Do not materially regress from the former 95.3% within-2% targeted fit or 0.43% targeted MARD unless held-out validity improves enough to justify the tradeoff.
- Require a clear reduction from the approximately 22.3% overall held-out MARD.
- Set separate maximum errors for economically important series rather than averaging away weak pensions, Social Security, business income, or capital gains.
- Require stable ESS and weight tails by state; no accepted result should depend on a small number of national records receiving extreme state shares.
- Prefer the candidate with better held-out and policy performance when target-fit differences are economically negligible.

**Deliverables:** reproducible sweep, comparison report, accepted versioned weight artifact.  
**Gate:** the selected configuration passes predeclared targeted, held-out, stability, and convergence rules.

### Phase 6: Validate policy outputs and integrate production

**Repositories:** both

- [ ] Run state tax liabilities and major credits for states representing different tax structures.
- [ ] Compare model totals with available administrative aggregates and explain remaining differences.
- [ ] Add uncertainty intervals to state outputs.
- [ ] Wire the accepted artifact into `src/data/state_weights.R` and remove the uniform placeholder in `src/sim/run.R`.
- [ ] Add artifact version, checksum, target version, and estimator configuration to every run output.
- [ ] Add CI smoke tests for row sums, national preservation, state closures, deterministic loading, and missing-artifact failure.
- [ ] Run a clean end-to-end baseline from published interfaces rather than developer scratch files.

**Deliverables:** state policy validation report, production weight artifact, runtime integration, CI tests.  
**Gate:** production starts only after a clean run reproduces the signed federal and state validation reports from pinned inputs.

## Proposed pull-request sequence

Keep the changes reviewable and avoid merging a consumer before its producer artifact exists.

1. **Simulator PR: filing model and schema contract.** Correct filing rules, constraint checks, field names, metadata, fixtures, and manifests.
2. **Data PR: interface and contract reader.** Register the pinned dependency, validate the manifest, preserve observed demographics, and add consumer tests.
3. **Simulator PR: annual pools and S19 artifact.** Publish all observed years and the aligned 2023 handoff with audit tables.
4. **Data PR: annual consumption and post-ceiling projection.** Load annual pools, define IDs/random draws, and assert closures.
5. **Cross-repo validation PRs.** Add the federal harness, fix summary metrics, run the staged A/B, and record sign-off.
6. **Simulator PR: state targets and estimator.** Rebuild margins, correct or justify KL, rerun the sweep, and preserve artifacts.
7. **Production PR: state-weight integration.** Add policy validation, uncertainty, versioned loading, CI, and remove the uniform placeholder.

Each PR should update the plan and decision log. A design document should not mark a feature complete until both repositories implement it and the relevant validation gate passes.

## Documentation changes

The current documentation contains valuable detail but has too many competing status summaries. Consolidate it as follows:

- `research/state_weights/nonfiler_residual/plan.md`: authoritative task list, dependencies, owners, and gates.
- `research/state_weights/nonfiler_residual/decisions_log.md`: immutable decisions and later superseding decisions, with separate upstream/downstream/validation statuses.
- `research/state_weights/nonfiler_residual/04_findings.md`: empirical findings only; update its finding count to include F11.
- `research/state_weights/nonfiler_residual/handoff.md`: exact published artifact contract and consumer instructions, not a statement that proposed behavior already exists.
- `research/STATUS.md`: short pointer to the authoritative plan and latest signed milestone.
- `research/state_weights/nonfiler_federal_validation.md`: executable runbook whose commands and paths exist.
- `Tax-Data/NONFILER_BRANCH_NOTES.md`: branch-specific changes and known limitations, linked back to the authoritative cross-repository plan.

Also fix the conventions checker's Windows UTF-8 handling. Its current YAML parsing can fail on an em dash under the startup locale and report many false missing-metadata findings. Read files explicitly as UTF-8 and test the checker on Windows and Linux.

## Required test matrix

| Level | Test | Failure it prevents |
|---|---|---|
| Unit | Filing thresholds, losses, gains, dependents, age rules | Misclassification of the nonfiler population |
| Unit | Mean-income and categorical calibration residuals | Reporting an unconverged constraint as satisfied |
| Unit | Observed demographic preservation | Replacing ASEC spouse sex with random imputations |
| Contract | Producer schema against consumer schema | Cross-repo publication that cannot load |
| Contract | Manifest, vintage, checksum, annual file list | Silent use of the wrong or incomplete artifact |
| Integration | Annual pool composition in `Tax-Data` | Applying only 2017 composition to every year |
| Integration | 2023 universe closure | Losing or double-applying S19 scale factors |
| Regression | Commitwise federal A/B | Attributing filer corrections to the pool swap |
| Statistical | Targeted and held-out state margins | Mistaking in-sample calibration for external validity |
| Stability | ESS, maximum shares, weight tails, seed sensitivity | State estimates driven by a few records |
| Policy | State tax and credit totals | Good demographic fit but biased tax bases |
| Reproducibility | Clean build from pinned interfaces | Dependence on deleted scratch artifacts |

## Definition of done

The nonfiler and state-weight work is complete only when all of the following are true:

- [ ] Every required interface resolves from a clean, pinned runscript.
- [ ] The producer-to-consumer schema test passes.
- [ ] `Tax-Data` consumes annual pools through the observed ceiling.
- [ ] The 2023 handoff closes by age band and in total.
- [ ] Observed demographics survive downstream processing.
- [ ] Filing-proxy limitations and sensitivity results are documented.
- [ ] All filing-model constraints are asserted and reported.
- [ ] The staged federal A/B has a signed findings report.
- [ ] State targets are rebuilt from the accepted federal population.
- [ ] The state fit is reproducible from versioned configurations and saved artifacts.
- [ ] Targeted, held-out, stability, and policy-output gates pass.
- [ ] Uncertainty accompanies state aggregates.
- [ ] The production runtime uses the accepted state-weight artifact rather than uniform shares.
- [ ] A clean end-to-end run reproduces all signed validation tables.
- [ ] The plan, decision log, handoff, branch notes, and status page agree on what is complete.

## Review evidence and limitations

The following checks passed during this review:

- `test_asec_tax_units()`.
- `test_state_weights()`.
- The state-weight finite-difference gradient self-test; the largest discrepancy was approximately `1.54e-11`.
- Parsing of the changed `Tax-Data` R files.

The checks are narrower than the proposed system. The state-weight self-test uses a small synthetic example and `beta = 0`, so it does not test the KL issue or the production configuration. No end-to-end `Tax-Data` run was possible because the new interface does not resolve from the active baseline and the published shared artifact is not available in this environment. The full state fit could not be reproduced because the documented scratch artifacts were deleted. These are readiness findings, not reasons to weaken the required validation.

## Final recommendation

Do not merge the current `Tax-Data` branch as the completed nonfiler implementation, and do not describe the current state weights as production-ready. Preserve the research and documentation work, but insert the contract, annual-pool, S19-alignment, and federal-validation gates before the proposed state refit.

The next coding action should be Phase 1: publish and test one exact producer/consumer schema. That step exposes errors cheaply and creates the foundation for every later validation exercise. Once the federal population is signed, the state-target rebuild and estimator refit become meaningful rather than provisional.

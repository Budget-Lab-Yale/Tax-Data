# Turn the 2015 IRS Public Use File into the Budget Lab's Tax-Simulator file for one year: the same steps as
# merge_puf.py, in R.
#
#   Rscript merge_puf.R <puf_2015.csv> <package folder> <output: .csv, .parquet, or both with no extension>
#
# Needs R with the packages data.table, jsonlite and arrow, and about 16 GB of memory. The package folder (written by
# the Budget Lab's pipeline, package.py) holds the files this script reads; its README.md says what each one is. The
# steps, in the order merge_puf.py takes them (each function below names its Python twin):
#   1. each record's 2015 items from the PUF's own fields (items_2015);
#   2. for each record copy (column copy, 1 to 3: one per trained network), each PUF item is its 2015 amount times the
#      item's factors in factors.parquet, step by step, the factor for positive amounts on positive amounts and the
#      factor for negative amounts on negative ones; zero stays zero; a factor of zero from the year the law repealed
#      a deduction; the same factors for every record (grow);
#   3. the weight: the PUF weight (S006 / 100) times the record copy's weight factor;
#   4. the items we added, as values (records.parquet);
#   5. the computed columns, in the pipeline's order (derive);
#   6. the two rules: the taxable state refund times state_ref_multiplier (0 removed, 1 kept whole, in between the share
#      the tax benefit rule keeps; only a multiplication, no tax calculation), then other income and taxable Social
#      Security computed again (refund_rule); the AMT net operating loss adjustment held to the record's net operating
#      loss (amt_cap); three added items computed from a PUF amount computed from the PUF items (puf_based);
#   7. the Tax-Simulator file's columns, then the records that are not PUF records, in the file's own order.
# The deductible half of self-employment tax is not in the package and is no output column: the tax calculator
# computes it from the self-employment income and wages (the working items carry the PUF's own E03260 grown).
# Sums are added from left to right; Python's numpy may add some in another order, so the two scripts can differ in
# the last binary digit on a few records.

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(arrow)
})

# ---- step 1: the 2015 items --------------------------------------------------------------------------------------
allowed_capital_gain <- function(sched_d, filing_status) {
  limit <- ifelse(filing_status == 3, 1500, 3000)
  pmax(as.numeric(sched_d), -limit)
}

signed_sum <- function(d, parts) {
  acc <- 0
  for (p in names(parts)) acc <- acc + d[[p]] * parts[[p]]
  acc
}

items_2015 <- function(raw, spec, floor) {
  s <- spec$items
  agg <- raw$RECID >= 999996
  f <- lapply(setNames(unlist(spec$raw_fields), unlist(spec$raw_fields)), function(c) as.numeric(raw[[c]]))
  for (c in unlist(s$suppressed_on_aggregate_records)) {
    stopifnot(all(is.na(f[[c]][agg])), !anyNA(f[[c]][!agg]))
    f[[c]][agg] <- 0
  }
  na <- names(f)[vapply(f, anyNA, logical(1))]
  if (length(na)) stop("PUF fields with missing values: ", paste(na, collapse = ", "))
  mars <- f$MARS
  out <- list(wages = f$E00200, txbl_int = f$E00300, div_pref = f$E00650, div_ord = f$E00600 - f$E00650,
              kg_lt = f$P23250, kg_dist = f$E01100)
  out$kg_st <- f$P22250 + pmin(f$E01000 - allowed_capital_gain(f$P22250 + f$P23250, mars), 0)
  for (p in names(s$parts)) {
    fld <- s$parts[[p]][[1]]
    if (!is.null(fld)) out[[p]] <- f[[fld]]
  }
  acc <- 0
  for (p in unlist(s$sche_parts)) acc <- acc + s$parts[[p]][[2]] * out[[p]]
  out$sche_other <- f$E02000 - acc
  out$other_gains <- f$E01200; out$txbl_ira_dist <- f$E01400; out$txbl_pens_dist <- f$E01700
  out$nontxbl_pens_dist <- pmax(f$E01500 - f$E01700, 0); out$ui <- f$E02300; out$alimony <- f$E00800
  out$state_ref <- f$E00700; out$gross_ss <- f$E02400; out$exempt_int <- f$E00400
  out$first_mort_int <- f$E19200; out$salt_prop <- f$E18500
  for (p in names(s$adj_parts)) out[[p]] <- f[[s$adj_parts[[p]]]]
  for (v in names(s$new_items)) if (!is.null(s$new_items[[v]])) out[[v]] <- f[[s$new_items[[v]]]]
  k1_part <- (f$E25940 + f$E25980 - f$E25920 - f$E25960) != 0
  # the couple's Schedule SE earnings once (E30400 holds the spouse's E30500), as build_table.puf_rows (part_se fix)
  out$part_se <- ifelse(k1_part, f$E30400 / 0.9235 - f$E00900 - f$E02100, 0)
  itemizer <- (f$FDED == 1) | agg
  medical <- pmax(0, f$E17500 - pmax(0, f$E00100 * floor))
  out$other_item_exp <- pmax(ifelse(itemizer, f$P04470 + f$E21040 - medical - f$E18400 - f$E18500 - f$E19200 -
                                      f$E19700 - f$E20500 - f$E20800, 0), 0)
  parts <- lapply(s$parts, function(x) x[[2]])
  out$business_income <- signed_sum(out, parts)
  signs <- s$agi_item_signs
  adj <- 0
  for (c in unlist(s$adj_fields)) adj <- adj + f[[c]]
  terms <- list(business_income = f$E00900 + f$E02000 + f$E02100, other_gains = out$other_gains,
                txbl_ira_dist = out$txbl_ira_dist, txbl_pens_dist = out$txbl_pens_dist, ui = out$ui,
                alimony = out$alimony, state_ref = out$state_ref, adjustments = adj)
  stopifnot(setequal(names(terms), names(signs)))
  allowed <- allowed_capital_gain(out$kg_st + f$P23250, mars)
  acc <- 0
  for (v in names(terms)) acc <- acc + signs[[v]] * terms[[v]]
  out$other_agi <- f$E00100 - f$E00200 - f$E00300 - f$E00600 - allowed - f$E01100 - acc - f$E02500
  out <- as.data.table(out)
  out[, RECID := raw$RECID]
  out
}

puf_categories <- function(raw) {
  n_dep <- raw$XOCAH + raw$XOCAWH + raw$XOPAR + raw$XOODEP
  data.table(RECID = raw$RECID, filing_status = raw$MARS, married = as.integer(raw$MARS == 2),
             n_dep = pmin(n_dep, 3), n_dep_ctc = pmin(raw$N24, 3), n_dep_eitc = pmin(raw$EIC, 3))
}

# ---- step 2: the factors -----------------------------------------------------------------------------------------
grow <- function(base, fac) {
  out <- list()
  for (col in unique(fac$column)) {
    g <- fac[fac$column == col, ]
    g <- g[order(g$step), ]
    stopifnot(length(unique(g$puf_item)) == 1)
    v <- as.numeric(base[[g$puf_item[1]]])
    for (i in seq_len(nrow(g))) {
      pos <- v > 0; neg <- v < 0
      v[pos] <- v[pos] * g$positive[i]
      v[neg] <- v[neg] * g$negative[i]
    }
    out[[col]] <- v
  }
  out
}

# ---- step 5: the computed columns --------------------------------------------------------------------------------
taxable_social_security <- function(ss, magi, filing_status, sp) {
  joint <- filing_status == 2
  base <- ifelse(joint, sp$joint[[1]], sp$single[[1]])
  adjusted <- ifelse(joint, sp$joint[[2]], sp$single[[2]])
  p <- pmax(sp$magi_rate * ss + magi, 0)
  tier1 <- pmin(pmin(pmax(p, base), adjusted) - base, ss) * sp$rates[[1]]
  tier2 <- pmin(pmax(p - adjusted, 0), ss) * sp$rates[[2]]
  pmin(tier1 + tier2, sp$rates[[2]] * ss)
}

magi_without_ss <- function(d, other) {
  without <- d$wages + d$txbl_int + d$dividends + d$capital_gains + other
  without + d$exempt_int + d$sl_int_ded
}

derive <- function(d, spec) {
  D <- spec$derive
  fs <- d$filing_status
  d$capital_gains <- allowed_capital_gain(d$kg_st + d$kg_lt, fs)
  d$capital_gains_line13 <- d$capital_gains + d$kg_dist
  for (tot in names(D$computed_totals)) d[[tot]] <- signed_sum(d, D$computed_totals[[tot]])
  n <- length(d$RECID)
  k1 <- rep(FALSE, n)
  for (p in unlist(D$k1_parts)) k1 <- k1 | (d[[p]] != 0)
  d$k1_owner <- as.integer(k1)
  other <- d$other_agi_remainder + d$kg_dist + signed_sum(d, D$agi_fold)
  pp <- unlist(D$pension_parts)
  d$gross_pens_dist <- d[[pp[1]]] + d[[pp[2]]]
  d$ss_taxable <- taxable_social_security(d$gross_ss, magi_without_ss(d, other), fs, D$social_security)
  d$other_agi <- other + d$ss_taxable
  made <- unlist(D$ratio_items_made)
  for (slot in names(D$ratios)) {
    item <- D$ratios[[slot]][[1]]; base <- D$ratios[[slot]][[2]]
    if (item %in% made) d[[item]] <- d[[slot]] * d[[base]]
  }
  for (item in names(D$remainders)) d[[item]] <- d[[D$remainders[[item]][[1]]]] - d[[D$remainders[[item]][[2]]]]
  for (slot in names(D$person_caps)) {
    r <- unlist(D$person_caps[[slot]])
    cap <- d[[r[2]]]
    if (length(r) > 2) cap <- cap - d[[r[3]]]
    d[[r[1]]] <- pmin(d[[slot]], pmax(cap, 0))
  }
  for (item in names(D$sums)) {
    parts <- unlist(D$sums[[item]])
    acc <- d[[parts[1]]]
    for (p in parts[-1]) acc <- acc + d[[p]]
    d[[item]] <- acc
  }
  for (item in names(D$flags_with)) {
    r <- unlist(D$flags_with[[item]])
    d[[item]] <- as.integer(ifelse(d[[r[2]]] != 0, d[[r[1]]], 0))
  }
  lim <- unlist(D$deferral_limit); roth <- D$roth_share
  trad <- rep(0, n)
  drawn <- list(d$elective1, d$elective2)
  share <- ifelse(d$married == 1, roth$joint, roth$nonjoint)
  for (k in 1:2) {
    limit <- ifelse(d[[paste0("age", k)]] >= D$catch_up_age, lim[2], lim[1])
    own <- pmin(pmin(drawn[[k]], limit), pmax(d[[paste0("wages", k)]], 0))
    d[[paste0("elective", k)]] <- own
    d[[paste0("trad_contr_er", k)]] <- own * (1 - share)
    trad <- trad + d[[paste0("trad_contr_er", k)]]
  }
  d$trad_deferrals <- trad
  d$medicare_wages <- d$wages + trad
  d
}

# ---- step 6: the two rules ---------------------------------------------------------------------------------------
refund_rule <- function(d, multiplier, spec) {   # merge_puf.refund_rule
  stopifnot(!anyNA(multiplier), all(multiplier >= 0), all(multiplier <= 1))
  ref <- d$state_ref
  ref2 <- ref * multiplier
  d$state_ref <- ref2
  d$other_agi <- d$other_agi + (ref2 - ref)
  other <- d$other_agi - d$ss_taxable
  taxable <- taxable_social_security(d$gross_ss, magi_without_ss(d, other), d$filing_status,
                                     spec$derive$social_security)
  d$ss_taxable <- taxable
  d$other_agi <- other + taxable
  d
}

amt_cap <- function(d) {
  loss <- pmax(0, -d$other_agi_remainder)
  d$amt_nols <- pmin(d$amt_nols_drawn, loss)
  d
}

lt_basis <- function(kg, r) {
  b <- rep(0, length(kg))
  g <- kg > 0; l <- kg < 0
  b[g] <- kg[g] * r[g] / (1 - r[g])
  b[l] <- -kg[l] * r[l] / (r[l] - 1)
  b
}

puf_based <- function(d, rec, spec) {   # merge_puf.puf_based
  pb <- unlist(spec$puf_based)
  if ("kg_lt_basis" %in% pb) d$kg_lt_basis <- lt_basis(d$kg_lt, rec$kg_lt_basis_ratio)
  if ("wagebill_farm" %in% pb) d$wagebill_farm <- pmax(d$farm, 0)
  if ("wagebill_scorp" %in% pb) {
    o <- rec$wagebill_scorp_outside
    d$wagebill_scorp <- ifelse(rec$wagebill_scorp_owner_wages == 1, o + d$wages, o)
  }
  d
}

# ---- step 7: the Tax-Simulator file's columns --------------------------------------------------------------------
formula_column <- function(name, d) {
  share <- d$spouse_se_share
  if (name %in% c("sole_prop2", "farm2", "part_se2")) return(d[[sub("2$", "", name)]] * share)
  if (name %in% c("sole_prop1", "farm1", "part_se1")) {
    x <- d[[sub("1$", "", name)]]
    return(x - x * share)
  }
  if (name == "other_inc") return(d$other_agi_remainder + d$farm_rent + d$sche_other)
  if (name == "kg_lt") return(d$kg_lt + d$kg_dist)
  if (name == "gross_pens_dist") return(d$txbl_pens_dist + d$nontxbl_pens_dist)
  if (name == "divorce_year") return(ifelse((d$alimony != 0) | (d$alimony_exp != 0), 0, NA_real_))
  if (name %in% c("kg_lt_years_held", "kg_lt_basis")) {
    kg <- d$kg_lt + d$kg_dist
    none <- (kg != 0) & (d$kg_lt_years_held < 0)
    return(ifelse(kg == 0, NA_real_, ifelse(none, 0, d[[name]])))
  }
  if (name == "id") return(as.numeric(d$record_id))
  if (name == "weight") return(d$weight)
  stop("unknown formula ", name)
}

tax_simulator_columns <- function(d, rec, spec) {
  n <- length(d$RECID)
  out <- list()
  for (c in names(spec$taxsim$rules)) {
    r <- spec$taxsim$rules[[c]]
    how <- r[[1]]; what <- r[[2]]
    out[[c]] <- switch(how,
      file = as.numeric(d[[what]]),
      formula = formula_column(c, d),
      shipped = rec[[paste0("ts.", c)]],
      constant = if (is.null(what)) rep(NA_real_, n) else if (is.character(what)) rep(what, n)
                 else rep(as.numeric(what), n),
      stop(c, ": unknown rule ", how))
  }
  as.data.table(out[unlist(spec$taxsim$columns)])
}

# ---- the merge ---------------------------------------------------------------------------------------------------
merge_puf <- function(puf_path, pkg) {
  spec <- fromJSON(file.path(pkg, "spec.json"), simplifyVector = FALSE)
  fac <- as.data.frame(read_parquet(file.path(pkg, "factors.parquet")))
  rec <- as.data.table(read_parquet(file.path(pkg, "records.parquet")))
  extra <- as.data.table(read_parquet(file.path(pkg, "extra_records.parquet")))
  raw <- fread(puf_path, select = unlist(spec$raw_fields))
  stopifnot(!anyDuplicated(raw$RECID))
  floor <- rec[, .(medical_floor = medical_floor[1]), by = RECID]
  raw <- raw[RECID %in% floor$RECID]
  stopifnot(nrow(raw) == nrow(floor))
  x15 <- items_2015(raw, spec, floor$medical_floor[match(raw$RECID, floor$RECID)])
  at <- match(rec$RECID, x15$RECID)
  base <- x15[at]
  # the merged record copies as a list of columns (assignment into a list does not copy the other columns)
  d <- list(RECID = rec$RECID, copy = rec$copy)
  n <- length(d$RECID)
  grown <- grow(base, fac)
  for (c in unlist(spec$factor_columns)) d[[c]] <- grown[[c]]
  w15 <- as.numeric(raw$S006)[match(rec$RECID, raw$RECID)] / 100
  d$weight <- w15 * rec$weight_factor
  cats <- puf_categories(raw)[match(rec$RECID, raw$RECID)]
  agg <- rec$RECID >= 999996
  for (c in setdiff(names(cats), "RECID")) d[[c]] <- as.integer(ifelse(agg, rec[[c]], cats[[c]]))
  for (c in unlist(spec$imputed)) d[[c]] <- rec[[c]]
  for (c in names(spec$constants)) d[[c]] <- rep(spec$constants[[c]], n)
  d$record_id <- spec$real_id_base + (d$copy - 1) * spec$copy_block + d$RECID
  d <- derive(d, spec)
  d <- refund_rule(d, as.numeric(rec$state_ref_multiplier), spec)
  d <- amt_cap(d)
  d <- puf_based(d, rec, spec)
  t <- tax_simulator_columns(d, rec, spec)
  t[, row := rec$row]
  t <- t[weight > 0]
  cols <- unlist(spec$taxsim$columns)
  out <- rbind(t, extra[, c("row", cols), with = FALSE], use.names = TRUE)
  setorder(out, row)
  out[, row := NULL]
  stopifnot(nrow(out) == spec$taxsim$records)
  list(out = out, spec = spec)
}

shortest <- function(x) {
  # the fewest digits (15 or 17) that read back as the same number
  s <- sprintf("%.15g", x)
  bad <- !is.na(x) & as.numeric(s) != x
  s[bad] <- sprintf("%.17g", x[bad])
  s[is.na(x)] <- "NA"
  s
}

write_text <- function(g, path, spec) {
  t <- copy(g)
  tx <- spec$taxsim
  for (c in unlist(tx$flags)) t[[c]] <- ifelse(is.na(t[[c]]), "NA", ifelse(t[[c]] == 1, "TRUE", "FALSE"))
  for (c in c(unlist(tx$integers), unlist(tx$integers_with_missing))) {
    v <- t[[c]]
    stopifnot(all(is.na(v) | v == round(v)))
    t[[c]] <- ifelse(is.na(v), "NA", sprintf("%.0f", v))
  }
  for (c in names(t)) if (is.numeric(t[[c]])) t[[c]] <- shortest(t[[c]])
  fwrite(t, path, na = "NA", quote = "auto")
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 3) {
  res <- merge_puf(args[1], args[2])
  dest <- args[3]
  ext <- tools::file_ext(dest)
  stem <- sub("\\.[^.]*$", "", dest)
  if (ext %in% c("", "parquet")) write_parquet(res$out, paste0(stem, ".parquet"))
  if (ext %in% c("", "csv")) write_text(res$out, paste0(stem, ".csv"), res$spec)
  cat(sprintf("%s records, %.2f million tax units, written to %s\n", format(nrow(res$out), big.mark = ","),
              sum(res$out$weight) / 1e6, dest))
} else if (!interactive()) {
  stop("usage: Rscript merge_puf.R <puf_2015.csv> <package folder> <output .csv, .parquet, or no extension>")
}

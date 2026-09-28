#------------------------------------------------------------------------------
# agi_proxy.R
#
# Adjusted gross income for a Tax-Data record, from its own columns, for
# classing records into SOI's AGI size classes when a year is reweighted
# (the per-year filer refit, research/state_weights/notes/filer_refit_design.md).
#
# Why a proxy: the 2017 reweight classes records by the PUF's E00100, which
# create_2017_puf.R never ages (it rescales the components, not E00100), and
# no later year carries AGI at all. AGI proper is Tax-Simulator's job
# (src/calc/functions/income/agi.R there); this reproduces its structure
# term by term under current-law statute, with three pieces computed here
# because the file does not carry them:
#   * taxable Social Security, by the Pub 915 worksheet (thresholds fixed in
#     statute since 1993, not indexed);
#   * net capital gain with losses limited to $3,000 ($1,500 MFS);
#   * the deduction for the employer-equivalent half of SE tax.
# Limits not modelled (all small below the top): the student loan interest
# phase-out, the educator expense cap, the business loss limitation.
#------------------------------------------------------------------------------

# Taxable Social Security (Pub 915): base and adjusted base amounts by filing
# status. Married filing separately is treated as living with the spouse
# (base 0), the common case on the PUF.
SS_BASE     = c(`1` = 25000, `2` = 32000, `3` = 0, `4` = 25000)
SS_ADJ_BASE = c(`1` = 34000, `2` = 44000, `3` = 0, `4` = 34000)

# Capital loss deductible against ordinary income
KG_LOSS_LIMIT     = 3000
KG_LOSS_LIMIT_MFS = 1500

# Self-employment tax: net earnings are 92.35% of SE income; none is due below
# $400. OASDI 12.4% up to the wage base (less the earner's wages), HI 2.9%.
SE_EARNINGS_SHARE = 0.9235
SE_MIN_EARNINGS   = 400
SE_OASDI_RATE     = 0.124
SE_HI_RATE        = 0.029
OASDI_WAGE_BASE   = c(`2017` = 127200, `2018` = 128400, `2019` = 132900,
                      `2020` = 137700, `2021` = 142800, `2022` = 147000,
                      `2023` = 160200)

# Above-the-line limits by year
SL_INT_LIMIT         = 2500
TUITION_LIMIT        = 4000                                                        # ...
TUITION_LAST_YEAR    = 2020                                                        # repealed from TY2021
DPAD_LAST_YEAR       = 2017                                                        # repealed by TCJA
ALIMONY_REPEAL_YEAR  = 2019                                                        # divorces from 2019 on
CHAR_ABOVE_LIMIT     = list(`2020` = c(300, 300, 300, 300), `2021` = c(300, 600, 300, 300))   # by filing status

# One earner's deductible half of SE tax
seca_half = function(se_income, wages, wage_base) {
  net = pmax(0, se_income) * SE_EARNINGS_SHARE
  net = if_else(net < SE_MIN_EARNINGS, 0, net)
  oasdi = SE_OASDI_RATE * pmin(net, pmax(0, wage_base - wages))
  0.5 * (oasdi + SE_HI_RATE * net)
}

# The Tax-Data output columns agi_proxy() reads, for callers that fread() a
# subset of a vintage file
AGI_PROXY_INPUTS = c('filing_status', 'divorce_year',
                     'wages', 'wages1', 'wages2', 'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
                     'state_ref', 'alimony', 'sole_prop', 'sole_prop1', 'sole_prop2',
                     'farm', 'farm1', 'farm2', 'part_se1', 'part_se2', 'kg_st', 'kg_lt', 'other_gains',
                     'txbl_ira_dist', 'txbl_pens_dist', 'rent', 'rent_loss', 'estate', 'estate_loss',
                     'part_active', 'part_active_loss', 'part_passive', 'part_passive_loss', 'part_179',
                     'scorp_active', 'scorp_active_loss', 'scorp_passive', 'scorp_passive_loss', 'scorp_179',
                     'ui', 'gross_ss', 'other_inc',
                     'ed_exp', 'hsa_contr', 'trad_contr_ira', 'sl_int_ded', 'keogh_contr', 'se_health',
                     'early_penalty', 'alimony_exp', 'tuition_ded', 'dpad', 'other_above_ded', 'char_cash')

# AGI and its three computed pieces for records of one tax year. `d` needs
# AGI_PROXY_INPUTS; returns d with part_scorp_net, sch_e_proxy, txbl_ss_proxy,
# txbl_kg_proxy, seca_ded_proxy, above_ded_proxy and agi_proxy added.
agi_proxy = function(d, year) {
  y = as.character(year)
  if (!(y %in% names(OASDI_WAGE_BASE))) stop('agi_proxy(): no OASDI wage base for ', year)
  stopifnot(all(d$filing_status %in% 1:4))

  d %>%
    mutate(
      # Schedule E. Tax-Simulator (src/calc/utils.R there) keeps only a net
      # partnership + S corporation GAIN; the PUF's losses are the allowed
      # (post-limitation) amounts, so AGI takes the net either way here.
      part_scorp_net = part_active + part_passive - part_active_loss - part_passive_loss - part_179 +
                       scorp_active + scorp_passive - scorp_active_loss - scorp_passive_loss - scorp_179,
      sch_e_proxy    = part_scorp_net + (rent - rent_loss) + (estate - estate_loss),
      txbl_kg_proxy  = pmax(kg_st + kg_lt,
                            -if_else(filing_status == 3, KG_LOSS_LIMIT_MFS, KG_LOSS_LIMIT)),
      alimony_qualifies = !is.na(divorce_year) & divorce_year < ALIMONY_REPEAL_YEAR,
      inc_ex_ss = wages + txbl_int + div_ord + div_pref + state_ref + txbl_ira_dist +
                  txbl_pens_dist + txbl_kg_proxy + other_gains + alimony * alimony_qualifies +
                  sole_prop + sch_e_proxy + farm + ui + other_inc,

      seca_ded_proxy = seca_half(sole_prop1 + farm1 + part_se1, wages1, OASDI_WAGE_BASE[[y]]) +
                       seca_half(sole_prop2 + farm2 + part_se2, wages2, OASDI_WAGE_BASE[[y]]),
      char_above = if (y %in% names(CHAR_ABOVE_LIMIT))
                     pmin(char_cash, CHAR_ABOVE_LIMIT[[y]][filing_status]) else 0,
      above_ded_proxy = ed_exp + hsa_contr + trad_contr_ira + keogh_contr + se_health +
                        early_penalty + alimony_exp * alimony_qualifies + other_above_ded +
                        pmin(sl_int_ded, SL_INT_LIMIT) +
                        (if (year <= TUITION_LAST_YEAR) pmin(tuition_ded, TUITION_LIMIT) else 0) +
                        (if (year <= DPAD_LAST_YEAR) dpad else 0) +
                        char_above + seca_ded_proxy,

      # Pub 915: provisional income is modified AGI (tax-exempt interest
      # included) plus half the benefits
      provisional   = inc_ex_ss - above_ded_proxy + exempt_int + 0.5 * gross_ss,
      ss_base       = SS_BASE[as.character(filing_status)],
      ss_adj        = SS_ADJ_BASE[as.character(filing_status)],
      txbl_ss_proxy = case_when(
        gross_ss <= 0            ~ 0,
        provisional <= ss_base   ~ 0,
        provisional <= ss_adj    ~ pmin(0.5 * (provisional - ss_base), 0.5 * gross_ss),
        TRUE                     ~ pmin(0.85 * (provisional - ss_adj) +
                                          pmin(0.5 * (ss_adj - ss_base), 0.5 * gross_ss),
                                        0.85 * gross_ss)),

      agi_proxy = inc_ex_ss + txbl_ss_proxy - above_ded_proxy
    ) %>%
    select(-alimony_qualifies, -char_above, -provisional, -ss_base, -ss_adj)   # part_scorp_net is kept: the diagnostics read it
}

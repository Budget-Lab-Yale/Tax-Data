# Step 33 (33_STATUS.md): the settings every step-33 job shares. Sourced by 33_*.sbatch.
export MINIMAL_TABLE=E33 MINIMAL_ALL_RECORDS=1 MINIMAL_KG_PIECES=1
export MINIMAL_MAP_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/map/V33
export MINIMAL_SETTINGS=research/rebuild/minimal/33_settings_scf.json
# the new amounts get dollars; the nine AGI parts are folded back into other_agi
export MINIMAL_EXTRA_AMOUNTS=business_income,retirement_gross,tax_exempt_interest,other_gains,ira_taxable,pensions_taxable,ss_taxable,unemployment,alimony,state_refunds,adjustments
export MINIMAL_AGI_FOLD=business_income:1,other_gains:1,ira_taxable:1,pensions_taxable:1,ss_taxable:1,unemployment:1,alimony:1,state_refunds:1,adjustments:-1

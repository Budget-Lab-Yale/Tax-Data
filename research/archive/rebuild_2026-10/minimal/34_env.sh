# Step 34 (34_STATUS.md): the settings every step-34 job shares. Sourced by 34_*.sbatch.
# Step 33's settings (33_env.sh) with table E34, map/V34 and the step-34 sheet;
# the eighteen business income parts get dollars (appended after step 33's
# amounts), and business_income's dollars become their exact signed sum
# before the AGI fold (04_dollars MINIMAL_SUM_TOTAL).
export MINIMAL_TABLE=E34 MINIMAL_ALL_RECORDS=1 MINIMAL_KG_PIECES=1
export MINIMAL_MAP_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/map/V34
export MINIMAL_SETTINGS=research/rebuild/minimal/34_settings_scf.json
export STEP34_PARTS=sch_c,part_pass_inc,part_nonpass_inc,part_pass_loss,part_nonpass_loss,part_179,scorp_pass_inc,scorp_nonpass_inc,scorp_pass_loss,scorp_nonpass_loss,scorp_179,rent_inc,rent_loss,estate_inc,estate_loss,farm_rent,sche_other,farm
export MINIMAL_EXTRA_AMOUNTS=business_income,retirement_gross,tax_exempt_interest,other_gains,ira_taxable,pensions_taxable,ss_taxable,unemployment,alimony,state_refunds,adjustments,sch_c,part_pass_inc,part_nonpass_inc,part_pass_loss,part_nonpass_loss,part_179,scorp_pass_inc,scorp_nonpass_inc,scorp_pass_loss,scorp_nonpass_loss,scorp_179,rent_inc,rent_loss,estate_inc,estate_loss,farm_rent,sche_other,farm
export MINIMAL_AGI_FOLD=business_income:1,other_gains:1,ira_taxable:1,pensions_taxable:1,ss_taxable:1,unemployment:1,alimony:1,state_refunds:1,adjustments:-1
export MINIMAL_SUM_TOTAL=business_income
export MINIMAL_SUM_PARTS=sch_c:1,part_pass_inc:1,part_nonpass_inc:1,part_pass_loss:-1,part_nonpass_loss:-1,part_179:-1,scorp_pass_inc:1,scorp_nonpass_inc:1,scorp_pass_loss:-1,scorp_nonpass_loss:-1,scorp_179:-1,rent_inc:1,rent_loss:-1,estate_inc:1,estate_loss:-1,farm_rent:1,sche_other:1,farm:1

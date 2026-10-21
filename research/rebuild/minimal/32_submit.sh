#!/bin/bash
# Step 32 (32_STATUS.md): submit the whole chain. Run from the repository root:
#   bash research/rebuild/minimal/32_submit.sh
# 1. maps (map/V32/); 2. per seed, step 31's stack again with net worth under
# the 2019 wave's token (2018), and under the 2016 wave's token through the
# new path (a check), on the GPU type step 31's stack used; 3. per seed, the
# three starting files (32_mix.py); 4. per seed, ten tilts: files m16, m80,
# m19 with each wave's own map, m16 and m80 with the 2019 wave's map, each on
# both wealth count bases; 5. the reproduction check (after the m16 tilts);
# 6. the report (after every tilt, whether or not each succeeded).
# Job ids go to $R/logs/step32/jobs.txt. Every step skips itself when its
# output exists, so resubmitting resumes.
# GPU_ANY=1 bash ... drops the GPU type request (the token-2018 stack's PUF
# records then differ from step 31's; the mixture is still valid).
set -euo pipefail
S=research/rebuild/minimal
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
mkdir -p $R/logs/step32
L=$R/logs/step32/jobs.txt
echo "--- chain submitted $(date)" >> $L
maps=$(sbatch --parsable -J maps32 $S/32_cpu.sbatch maps)
echo "maps $maps" >> $L
gpu() {   # the GPU step 31's stack of seed $1 ran on
  if [ "${GPU_ANY:-0}" = 1 ]; then echo "-p gpu_b200,gpu,gpu_h100,gpu_h200 --gres=gpu:1"
  elif [ "$1" = 3 ]; then echo "-p gpu_b200 --gres=gpu:b200:1"
  else echo "-p gpu --gres=gpu:a5000:1"; fi
}
ALL=""; M16=""
for k in 1 2 3; do
  s18=$(sbatch --parsable -J st32k${k}y18 $(gpu $k) $S/32_stack.sbatch $k 2018)
  s15=$(sbatch --parsable -J st32k${k}y15 $(gpu $k) $S/32_stack.sbatch $k 2015)
  mix=$(sbatch --parsable -J mix32k$k --dependency=afterok:$s18,afterany:$s15 $S/32_cpu.sbatch mix $k)
  echo "seed $k: stack token 2018 $s18, token 2015 (check) $s15, mix $mix" >> $L
  for spec in m16: m80: m19: m16:t19 m80:t19; do
    mx=${spec%%:*}; rule=${spec##*:}
    for base in scf_units file_units; do
      b=$([ $base = scf_units ] && echo s || echo f)
      t=$(sbatch --parsable -J t32k${k}${mx}${rule}${b} --dependency=afterok:$mix:$maps \
            $S/32_tilt.sbatch $k $mx $base $rule)
      echo "seed $k tilt $mx ${rule:-own-wave} $base $t" >> $L
      ALL="$ALL:$t"
      [ "$spec" = "m16:" ] && M16="$M16:$t"
    done
  done
done
chk=$(sbatch --parsable -J check32 --dependency=afterany$M16 $S/32_cpu.sbatch check)
rep=$(sbatch --parsable -J report32 --dependency=afterany$ALL $S/32_cpu.sbatch report)
echo "check $chk (afterany the m16 tilts); report $rep (afterany every tilt)" >> $L
tail -40 $L

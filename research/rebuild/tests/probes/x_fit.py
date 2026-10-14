"""Probe X: run test 3's tilt (../posttrain/c_fit.py, unchanged) on the
translated dataset full_v3x: its targets ($Q/a_v3x, from x_targets.py) and
the model trained on it. a_common.configure_full is redirected to this
probe's folders; everything else is c_fit.py as C ran it.

    sbatch x_gpu.sbatch x_fit.py --arm P_core --dataset full_v3x --outdir $Q/c/P_core [--k 16 --k-fit 16]
"""
import sys
from pathlib import Path

POST = Path("/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain")
sys.path.insert(0, str(POST))
Q = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/x")
import a_common as A  # noqa: E402


def configure_v3x(version="full_v3x", ckpt=None):
    a_dir = {"full_v3x": "a_v3x", "full_v3x2": "a_v3x2"}[version]
    A.configure(Q / "data" / version,
                ckpt or Q / "model" / version / f"checkpoint_{version}_w1_seed01.pt",
                Q / a_dir, POST / "g_tokenizer.py")


A.configure_full = configure_v3x
import c_fit  # noqa: E402

c_fit.main()

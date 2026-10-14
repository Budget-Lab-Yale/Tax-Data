"""Probe EV: put the existing scorecards (probe V's v_score.py output) for
the transformer and the references into the EV folder, relabelled so
v_collect.py groups them: probe C's zero-coded model (three model seeds,
sampling seed 1), the PUF validation records, probe BL's trees on the PUF
alone (minimum leaf 20). Aggregates only."""
import json
E = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/ev/v"
C = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/v"
V = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/v/out"
for k in (1, 2, 3):
    r = json.load(open(f"{C}/Czero_seed0{k}.json"))
    r["label"] = f"Transformer (probe C, zero coding) seed {k}"
    json.dump(r, open(f"{E}/transformer_seed0{k}.json", "w"), indent=1)
r = json.load(open(f"{V}/validation.json"))
print("validation label", r["label"])
json.dump(r, open(f"{E}/validation.json", "w"), indent=1)
for s in (1, 2):
    r = json.load(open(f"{V}/bl_sizefirst_leaf20_s{s}.json"))
    print("BL label", r["label"])
    r["label"] = f"BL trees, PUF alone (probe BL, minimum leaf 20) seed {s}"
    json.dump(r, open(f"{E}/bl_leaf20_s{s}.json", "w"), indent=1)

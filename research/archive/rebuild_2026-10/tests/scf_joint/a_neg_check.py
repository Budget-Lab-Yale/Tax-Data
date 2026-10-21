import pandas as pd, numpy as np
for w in (2019, 2022):
    u = pd.read_csv(f"/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/scf_units/scf_units_{w}.csv.gz")
    for c in ["ss_pens", "db"]:
        neg = u[c] < 0
        print(w, c, "negative rows", int(neg.sum()), "distinct negative values", sorted(u.loc[neg, c].round(2).unique())[:8], "unit codes", u.loc[neg, "taxunit_code"].value_counts().to_dict())
    print(w, "hh_X5722 negative values", u.loc[u.hh_X5722 < 0, "hh_X5722"].value_counts().to_dict())

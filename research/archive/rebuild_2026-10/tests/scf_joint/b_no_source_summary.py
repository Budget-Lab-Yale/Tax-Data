"""Print the matched three-seed source ablation comparison (standard library only)."""
import json
from pathlib import Path
from statistics import mean


ROOT = Path('/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint')
GROUPS = [
    ('Source label, own source', ROOT / 'eval/expA_clean', 'model_joint_w1_plain'),
    ('Source label, presented as SCF', ROOT / 'eval/expA_clean', 'model_joint_w1_asSCF_plain'),
    ('No source label', ROOT / 'no_source/eval/expA_clean', 'model_joint_w1_nosource_plain'),
    ('SCF half 2 only', ROOT / 'eval/expA_clean', 'model_half2_w1_plain'),
]
METRICS = [
    ('Holding by income group: gap (pp)', 'hold_inc_mae_pp', 2),
    ('Amounts by income group: rank gap', 'amt_inc_rank_mae_pctile', 2),
    ('Income-wealth rank correlation: gap', 'income_corr_mae', 3),
    ('Mean net worth by income group: error (%)', 'nw_inc_mean_mape', 1),
    ('Top 1% share of net worth (%)', 'nw_top1_cand', 1),
    ('Classifier AUC (ideal 0.5)', 'classifier_auc', 3),
]


def main():
    groups = []
    for name, root, stem in GROUPS:
        entries = [json.loads((root / f'{stem}_seed{i:02d}' / 'summary.json').read_text())
                   for i in range(1, 4)]
        assert all(e['n_records'] == 15380 and e['n_records_missing'] == 0
                   and e['n_draws'] == 5 for e in entries)
        groups.append((name, entries))
    print('Three training seeds; mean [minimum, maximum]. Same 15,380 graded records; five draws each.')
    print('True top 1% wealth share:', groups[0][1][0]['headline']['nw_top1_ref'])
    print('\n| Metric | ' + ' | '.join(n for n, _ in groups) + ' |')
    print('|---|' + '---|' * len(groups))
    for label, key, digits in METRICS:
        cells = []
        for _, entries in groups:
            vals = [e['headline'][key] for e in entries]
            cells.append(f'{mean(vals):.{digits}f} [{min(vals):.{digits}f}, {max(vals):.{digits}f}]')
        print('| ' + label + ' | ' + ' | '.join(cells) + ' |')
    print('\nPaired changes (no source minus own source), seeds 1, 2, 3:')
    for label, key, digits in METRICS:
        delta = [b['headline'][key] - a['headline'][key]
                 for a, b in zip(groups[0][1], groups[2][1])]
        print(label, ', '.join(f'{v:+.{digits}f}' for v in delta))


if __name__ == '__main__':
    main()

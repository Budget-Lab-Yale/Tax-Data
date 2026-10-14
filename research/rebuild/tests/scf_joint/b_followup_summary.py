"""Summarize the bounded follow-up experiment, including incomplete status."""
import json
from pathlib import Path
from statistics import mean

S = Path('/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint')
ROOT = S / 'followup_20261009'
METRICS = [
    ('Ownership error (pp)', 'hold_inc_mae_pp', 2),
    ('Amount-rank error', 'amt_inc_rank_mae_pctile', 2),
    ('Income correlation error', 'income_corr_mae', 3),
    ('Mean wealth error (%)', 'nw_inc_mean_mape', 1),
    ('Top 1% wealth share (%)', 'nw_top1_cand', 1),
    ('Classifier AUC', 'classifier_auc', 3),
]


def files(root, stem, suffix=''):
    return [json.loads(f.read_text()) for i in range(1, 4)
            if (f := root / f'{stem}_seed{i:02d}{suffix}' / 'summary.json').exists()]


def cell(entries, key, digits, section='headline'):
    vals = [e[section][key] for e in entries if key in e[section]]
    if not vals:
        return 'pending'
    return f'{mean(vals):.{digits}f} [{min(vals):.{digits}f}, {max(vals):.{digits}f}]'


def main():
    for ds in ['expA_clean', 'expB']:
        postfix = '_scf' if ds == 'expB' else ''
        groups = [
            ('Source label', files(S / 'eval' / ds, f'model_joint_w1{postfix}_plain')),
            ('Source label, SCF at completion', files(S / 'eval' / ds,
                 'model_joint_w1_asSCF_plain' if ds == 'expA_clean' else 'model_joint_w1_scf_plain')),
            ('No source label', files(S / 'no_source/eval' / ds, 'model_joint_w1_nosource_plain')
                 if ds == 'expA_clean' else files(ROOT / 'eval' / ds, 'model_joint_w1_nosource_random_scf_plain')),
            ('Corrected orderless', files(ROOT / 'eval' / ds, f'model_joint_w1_nosource_orderless{postfix}_plain')),
            ('Fixed order', files(ROOT / 'eval' / ds, f'model_joint_w1_nosource_ordered{postfix}_plain')),
            ('SCF-only transformer', files(S / 'eval' / ds,
                 'model_half2_w1_plain' if ds == 'expA_clean' else 'model_scfonly_w1_scf_plain')),
            ('Weighted forest', files(S / 'eval' / ds, 'comp_drfsw')),
        ]
        print(f'\n## {ds}: held-out SCF truth, mean [min, max] over training seeds\n')
        print('| Method (completed seeds) | ' + ' | '.join(x[0] for x in METRICS) + ' |')
        print('|---|' + '---|' * len(METRICS))
        for name, entries in groups:
            print(f'| {name} ({len(entries)}/3) | ' + ' | '.join(cell(entries,k,d) for _,k,d in METRICS) + ' |')
        if groups[0][1]:
            print('True top 1% wealth share:', groups[0][1][0]['headline']['nw_top1_ref'])

    print('\n## PUF comparison with interpolated SCF (no true PUF wealth)\n')
    print('| Method (seeds) | Ownership gap (pp) | Amount gap (%) | Top 1% share (%) |')
    print('|---|---|---|---|')
    groups = [('Source label', S / 'eval/expB', 'model_joint_w1_puf_plain'),
              ('Presented as SCF', S / 'eval/expB', 'model_joint_w1_puf_asSCF_plain')]
    groups += [(style, ROOT / 'eval/expB', f'model_joint_w1_nosource_{style}_puf_plain')
               for style in ['random', 'orderless', 'ordered']]
    for name, root, stem in groups:
        es = files(root, stem, '_puf')
        tops = [100*e['headline']['nw_top1%_share']['puf2015'] for e in es]
        top = f'{mean(tops):.1f} [{min(tops):.1f}, {max(tops):.1f}]' if tops else 'pending'
        print(f'| {name} ({len(es)}/3) | {cell(es,"hold_inc_mae_pp",2)} | {cell(es,"amt_inc_dollar_mae_pct",1)} | {top} |')

    print('\n## Reversed wealth completion order, expB SCF\n')
    for style in ['random', 'orderless']:
        es = files(ROOT / 'eval/expB', f'model_joint_w1_nosource_{style}_reverse_scf_plain')
        print(style, f'{len(es)}/3', {label:cell(es,key,d) for label,key,d in METRICS})

    print('\n## Oracle-target calibration, expB SCF\n')
    print('Four true group means supplied as targets, not predicted. Other metrics untargeted.')
    for stem in ['joint_w1_scf_plain'] + [f'joint_w1_nosource_{style}_scf_plain'
                                        for style in ['random','orderless','ordered']]:
        es = files(ROOT / 'eval/expB', 'tilt_' + stem)
        if not es:
            print(stem, 'pending')
            continue
        print(stem, f'{len(es)}/3',
              'all feasible:', all(g['feasible'] for e in es for g in e['diagnostics']['groups']),
              'effective draws:', mean(e['diagnostics']['population_weighted_mean_effective_draws'] for e in es),
              'KL:', mean(e['diagnostics']['population_weighted_kl'] for e in es))
        for label,key,d in METRICS[:-1]:
            print(' ', label, cell(es,key,d,'before'), '->', cell(es,key,d,'after'))


if __name__ == '__main__':
    main()

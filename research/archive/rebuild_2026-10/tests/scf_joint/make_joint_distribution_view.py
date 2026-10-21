"""Build a compact, self-contained view of actual vs raw DRF vs transformer.

Uses the already evaluated expB SCF holdout; does not rerun any model. The
levels log is the existing population-weighted three-seed comparison.
"""

import csv
import json
from pathlib import Path


HERE = Path(__file__).resolve().parent
ROOT = Path('/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint')
LEVELS = ROOT / 'logs/levels_28877312.out'
EVAL = ROOT / 'eval/expB'
GROUPS = ['Bottom 50%', '50-90%', '90-99%', 'Top 1%']
METHODS = [('SCF (truth)', 'Observed SCF'), ('Production forest', 'Raw DRF'),
           ('Transformer', 'Transformer')]
SECTIONS = {
    'Share of all net worth held by each income group (%)': 'wealth_share',
    'Mean net worth by income group ($ thousands)': 'mean_wealth',
    'Median net worth by income group ($ thousands)': 'median_wealth',
    'Share holding stocks directly (%)': 'stocks',
    'Share owning a home (%)': 'home',
    'Share owning a business (pass-through) (%)': 'business',
}


def read_level_table(source, title):
    block = source.split(title + '\n', 1)[1].split('\n\n', 1)[0]
    lines = [line for line in block.splitlines() if line.startswith('|')]
    header = [x.strip() for x in lines[0].strip('|').split('|')]
    data = {}
    for line in lines[2:]:
        cells = [x.strip() for x in line.strip('|').split('|')]
        if cells[0] in GROUPS:
            data[cells[0]] = {m: float(cells[header.index(m)]) for m, _ in METHODS}
    if list(data) != GROUPS:
        raise ValueError(f'{title}: unexpected income groups {list(data)}')
    return {label: [data[g][key] for g in GROUPS] for key, label in METHODS}


def read_conditional(holds, given):
    rows = {}
    for source, label in [('comp_drf', 'Raw DRF'),
                          ('model_joint_w1_scf_plain', 'Transformer')]:
        cand, ref = [], []
        for seed in (1, 2, 3):
            path = EVAL / f'{source}_seed{seed:02d}' / 'detail_cond.csv'
            with path.open() as f:
                matches = [row for row in csv.DictReader(f)
                           if row['holds'] == holds and row['given'] == given]
            if len(matches) != 1:
                raise ValueError(f'{path}: expected one {holds}|{given}')
            cand.append(float(matches[0]['cand']) * 100)
            ref.append(float(matches[0]['ref']) * 100)
        rows[label] = sum(cand) / 3
        rows['Observed SCF'] = sum(ref) / 3
    return {k: round(v, 1) for k, v in rows.items()}


def markdown_table(data, key, unit='%', decimals=1):
    out = ['| Income group | Observed SCF | Raw DRF | Transformer |',
           '|---|---:|---:|---:|']
    for i, group in enumerate(GROUPS):
        vals = [data[key][name][i] for name in
                ('Observed SCF', 'Raw DRF', 'Transformer')]
        out.append('| ' + group + ' | ' + ' | '.join(
            f'{v:,.{decimals}f}{unit}' for v in vals) + ' |')
    return '\n'.join(out)


def main():
    source = LEVELS.read_text()
    data = {short: read_level_table(source, title)
            for title, short in SECTIONS.items()}
    cond = {
        'Mortgage among homeowners': read_conditional('primary_mortgage', 'primary_home'),
        'Business assets among units with business income': read_conditional('pass_throughs', 'business'),
        'Stocks among units with interest or dividends': read_conditional('equities', 'int_div'),
    }
    payload = dict(groups=GROUPS, metrics=data, conditional=cond)
    (HERE / 'JOINT_DISTRIBUTION_VIEW.json').write_text(json.dumps(payload, indent=2) + '\n')

    colors = ['#b9d0d5', '#6b9fac', '#316e80', '#163749']
    svg = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 980 360" '
           'role="img" aria-label="Share of total net worth by income group: observed SCF, raw DRF, transformer">',
           '<rect width="980" height="360" fill="#f7f8f6"/>',
           '<text x="28" y="39" font-family="Arial,sans-serif" font-size="22" '
           'font-weight="bold" fill="#18232b">Where the wealth lands</text>',
           '<text x="28" y="62" font-family="Arial,sans-serif" font-size="13" '
           'fill="#4c5d67">Share of total net worth held by each income group · held-out SCF units</text>']
    for row, method in enumerate(('Observed SCF', 'Raw DRF', 'Transformer')):
        y = 103 + 70 * row
        svg.append(f'<text x="28" y="{y+26}" font-family="Arial,sans-serif" '
                   f'font-size="15" fill="#18232b">{method}</text>')
        x = 180.0
        for i, group in enumerate(GROUPS):
            v = data['wealth_share'][method][i]
            width = v * 7.45
            svg.append(f'<rect x="{x:.2f}" y="{y}" width="{width:.2f}" height="42" '
                       f'fill="{colors[i]}"><title>{method}: {group} {v:.1f}%</title></rect>')
            if width >= 54:
                fg = '#fff' if i >= 2 else '#18232b'
                svg.append(f'<text x="{x+width/2:.2f}" y="{y+27}" '
                           f'text-anchor="middle" font-family="Arial,sans-serif" '
                           f'font-size="14" font-weight="bold" '
                           f'fill="{fg}">{v:.1f}%</text>')
            x += width
    lx = [180, 350, 495, 645]
    for i, group in enumerate(GROUPS):
        svg.append(f'<rect x="{lx[i]}" y="322" width="15" height="15" fill="{colors[i]}"/>')
        svg.append(f'<text x="{lx[i]+22}" y="335" font-family="Arial,sans-serif" '
                   f'font-size="13" fill="#18232b">{group}</text>')
    svg.append('</svg>')
    (HERE / 'JOINT_DISTRIBUTION_VIEW.svg').write_text(''.join(svg) + '\n')

    md = [
        '# Where the wealth lands: observed SCF, raw DRF, transformer',
        '',
        'Same 12,021 held-out SCF tax-unit records (2,207 households) from 2013 and 2016. '
        'All levels are population-weighted; methods are means of three fits with five draws each. '
        'Income groups are ranks of total income within source and year. '
        'Rows without observed SCF wealth are excluded from all three columns.',
        '',
        'Raw DRF means the refitted production donor-draw stage **before** its later '
        'calibration. The transformer is the baseline joint PUF+SCF model, completed '
        'for held-out SCF units as SCF. The experimental survey-weighted DRF is excluded.',
        '',
        '## Share of total net worth by income group', '',
        markdown_table(data, 'wealth_share'), '',
        'The top 1% of **income** holds 30.6% of observed wealth. The transformer '
        'puts 25.1% there; raw DRF puts 38.2% there. This measures the wealth–income '
        'link, distinct from the share held by the top 1% of **wealth**.',
        '', '## Mean net worth within each income group', '',
        markdown_table(data, 'mean_wealth', 'k', 0), '',
        '## Share directly holding stocks', '', markdown_table(data, 'stocks'), '',
        '## Share owning a primary home', '', markdown_table(data, 'home'), '',
        '## Share owning a pass-through business', '', markdown_table(data, 'business'), '',
        '## Cross-variable ownership', '',
        '| Relationship | Observed SCF | Raw DRF | Transformer |',
        '|---|---:|---:|---:|',
    ]
    for name, vals in cond.items():
        md.append('| ' + name + ' | ' + ' | '.join(
            f'{vals[k]:.1f}%' for k in ('Observed SCF', 'Raw DRF', 'Transformer')) + ' |')
    md += ['', 'The accompanying [interactive charts](JOINT_DISTRIBUTION_VIEW.html) and '
           '[stacked wealth chart](JOINT_DISTRIBUTION_VIEW.svg) '
           'show these same evaluated values. These SCF holdout comparisons test '
           'relationships that are observed; PUF wealth is not observed and cannot '
           'be validated record by record.', '',
           '## Architectural note: PUF as the starting population', '',
           'The PUF-first version can keep the actual PUF rows, tax fields, and '
           'weights as the starting population. One model trained across SCF, '
           'SIPP, and other surveys can learn the missing survey fields given '
           'their common fields; it then fills those fields on PUF rows. '
           'The resulting completed records still define one joint distribution '
           'over the tax and survey fields. Reweighting can move that joint '
           'distribution to aggregate targets. It leaves PUF-only tax–wealth '
           'dependence beyond the shared fields as a modeling assumption until '
           'bridging evidence is added.', '',
           'Source: `$S/logs/levels_28877312.out` and expB `detail_cond.csv` '
           'for seeds 1–3. Generate with `python3 make_joint_distribution_view.py`.', '']
    (HERE / 'JOINT_DISTRIBUTION_VIEW.md').write_text('\n'.join(md))

    json_data = json.dumps(payload).replace('<', '\\u003c')
    document = '''<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>SCF wealth: observed vs raw DRF vs transformer</title>
<style>
:root{font-family:system-ui,-apple-system,Segoe UI,sans-serif;color:#18232b;background:#f7f8f6}
body{max-width:1080px;margin:0 auto;padding:24px 18px 64px}h1{font-size:1.8rem;margin-bottom:.35rem}
p{line-height:1.5;max-width:870px}.small{font-size:.9rem;color:#4c5d67}
.panel{background:white;border:1px solid #dce1dd;border-radius:12px;padding:20px;margin:18px 0;box-shadow:0 2px 12px #1b28200a}
.tabs{display:flex;flex-wrap:wrap;gap:8px;margin:20px 0}
button{border:1px solid #ccd6d1;background:#fff;border-radius:999px;padding:9px 13px;cursor:pointer;color:#152f3b;font-weight:600}
button[aria-selected=true]{background:#163749;color:white;border-color:#163749}
.legend{display:flex;flex-wrap:wrap;gap:18px;margin:8px 0 14px}.key{display:inline-flex;align-items:center;gap:7px}
.swatch{width:13px;height:13px;border-radius:3px;display:inline-block}
svg{width:100%;height:auto;overflow:visible}svg text{font-family:inherit;fill:#32434c;font-size:13px}
.axis{stroke:#aab8b4;stroke-width:1}.grid{stroke:#e8eeeb;stroke-width:1}
table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{padding:9px;border-bottom:1px solid #e4e9e5;text-align:right}
th:first-child,td:first-child{text-align:left}th{background:#f2f5f2}
@media(max-width:620px){body{padding:16px 10px}svg text{font-size:11px}.panel{padding:12px}}
</style></head><body>
<h1>Where the wealth lands</h1>
<p>Observed SCF, raw DRF, and the joint transformer on the same held-out SCF units. Select a relationship to see how each method changes it.</p>
<p class="small">12,021 tax-unit records in 2,207 households, SCF 2013 and 2016; population-weighted. DRF is its donor-draw stage before calibration. Model values average three fits and five draws per record.</p>
<div class="tabs" id="tabs"></div><div class="panel"><h2 id="chartTitle"></h2>
<div class="legend" id="legend"></div><div id="chart"></div><p class="small" id="chartNote"></p></div>
<div class="panel"><h2>Exact levels</h2><div id="table"></div></div>
<div class="panel"><h2>Relationships across variables</h2><p>Each number is the share with the first item among units satisfying the condition.</p><div id="conditional"></div></div>
<p class="small">The top 1% here means the top 1% of <b>income</b>. A separate top 1% of wealth measure asks a different question. PUF wealth is unobserved, so this is a held-out SCF comparison.</p>
<script>
const data = __DATA__;
const methods=['Observed SCF','Raw DRF','Transformer'];
const colors={'Observed SCF':'#253b4a','Raw DRF':'#c47539','Transformer':'#2e8790'};
const groupColors=['#b9d0d5','#6b9fac','#316e80','#163749'];
const labels={wealth_share:'Share of all net worth by income group',mean_wealth:'Mean net worth within income group',stocks:'Share directly holding stocks',home:'Share owning a primary home',business:'Share owning a pass-through business'};
const notes={wealth_share:'Each row sums to 100%. Raw DRF shifts too much wealth into the highest-income group; the transformer shifts too little.',mean_wealth:'Dollar axis uses a log scale so the bottom half and top 1% can appear in one chart.',stocks:'An income–asset relationship: ownership of directly held stocks rises with income.',home:'Homeownership is close across methods, even where wealth amounts differ.',business:'Ownership of pass-through business assets by income group.'};
const esc=s=>String(s).replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
let active='wealth_share';
function table(metric){const unit=metric==='mean_wealth'?'$k':'%';let s='<table><thead><tr><th>Income group</th>'+methods.map(m=>'<th>'+esc(m)+'</th>').join('')+'</tr></thead><tbody>';data.groups.forEach((g,i)=>{s+='<tr><td>'+esc(g)+'</td>'+methods.map(m=>'<td>'+data.metrics[metric][m][i].toLocaleString(undefined,{maximumFractionDigits:1})+unit+'</td>').join('')+'</tr>'});return s+'</tbody></table>'}
function stacked(){let s='<svg viewBox="0 0 950 275" role="img" aria-label="Share of wealth by income group, stacked bars for observed SCF, raw DRF and transformer">';methods.forEach((m,r)=>{const y=38+r*77;s+='<text x="10" y="'+(y+22)+'">'+esc(m)+'</text>';let x=170;data.groups.forEach((g,i)=>{const v=data.metrics.wealth_share[m][i],w=v*7.3;s+='<rect x="'+x+'" y="'+y+'" width="'+w+'" height="38" fill="'+groupColors[i]+'"><title>'+esc(m+': '+g+' '+v.toFixed(1)+'%')+'</title></rect>';if(w>54)s+='<text x="'+(x+w/2)+'" y="'+(y+24)+'" text-anchor="middle" fill="'+(i>1?'white':'#18232b')+'">'+v.toFixed(1)+'%</text>';x+=w})});s+='</svg>';s+='<div class="legend">'+data.groups.map((g,i)=>'<span class="key"><span class="swatch" style="background:'+groupColors[i]+'"></span>'+esc(g)+'</span>').join('')+'</div>';return s}
function lineChart(metric){const series=data.metrics[metric],mean=metric==='mean_wealth';const W=950,H=350,L=75,R=22,T=20,B=67,plotW=W-L-R,plotH=H-T-B;const vmax=mean?30000:100;let s='<svg viewBox="0 0 '+W+' '+H+'" role="img" aria-label="'+esc(labels[metric])+' for observed SCF, raw DRF, and transformer">';const y=v=>T+plotH*(1-(mean?Math.log10(Math.max(v,10)/10)/Math.log10(vmax/10):v/vmax));const ticks=mean?[10,100,1000,10000]:[0,25,50,75,100];ticks.forEach(v=>{const yy=y(v);s+='<line class="grid" x1="'+L+'" x2="'+(W-R)+'" y1="'+yy+'" y2="'+yy+'"/><text x="'+(L-10)+'" y="'+(yy+4)+'" text-anchor="end">'+(mean?'$'+(v>=1000?(v/1000)+'m':v+'k'):v+'%')+'</text>'});data.groups.forEach((g,i)=>{const x=L+plotW*(i+.5)/4;s+='<text x="'+x+'" y="'+(H-27)+'" text-anchor="middle">'+esc(g)+'</text>'});methods.forEach(m=>{const pts=series[m].map((v,i)=>[L+plotW*(i+.5)/4,y(v)]);s+='<polyline fill="none" stroke="'+colors[m]+'" stroke-width="3" points="'+pts.map(p=>p.join(',')).join(' ')+'"/>';pts.forEach((p,i)=>{s+='<circle cx="'+p[0]+'" cy="'+p[1]+'" r="5" fill="'+colors[m]+'"><title>'+esc(m+': '+data.groups[i]+' '+series[m][i]+(mean?' thousand dollars':'%'))+'</title></circle>'})});return s+'</svg>'}
function render(){document.getElementById('tabs').innerHTML=Object.keys(labels).map(k=>'<button type="button" data-key="'+k+'" aria-selected="'+(k===active)+'">'+esc(labels[k])+'</button>').join('');document.querySelectorAll('button[data-key]').forEach(b=>b.onclick=()=>{active=b.dataset.key;render()});document.getElementById('chartTitle').textContent=labels[active];document.getElementById('legend').innerHTML=active==='wealth_share'?'':methods.map(m=>'<span class="key"><span class="swatch" style="background:'+colors[m]+'"></span>'+esc(m)+'</span>').join('');document.getElementById('chart').innerHTML=active==='wealth_share'?stacked():lineChart(active);document.getElementById('chartNote').textContent=notes[active];document.getElementById('table').innerHTML=table(active)}
document.getElementById('conditional').innerHTML='<table><thead><tr><th>Relationship</th>'+methods.map(m=>'<th>'+esc(m)+'</th>').join('')+'</tr></thead><tbody>'+Object.entries(data.conditional).map(([name,vals])=>'<tr><td>'+esc(name)+'</td>'+methods.map(m=>'<td>'+vals[m].toFixed(1)+'%</td>').join('')+'</tr>').join('')+'</tbody></table>';
render();
</script></body></html>'''.replace('__DATA__', json_data)
    (HERE / 'JOINT_DISTRIBUTION_VIEW.html').write_text(document)
    print('Wrote JOINT_DISTRIBUTION_VIEW.md, .html, .svg, and .json')


if __name__ == '__main__':
    main()

"""The rebuild pipeline's runner. Run it from the repository root, or set REBUILD_ROOT.

The pipeline is a chain of stages. Each stage reads the files earlier stages wrote and writes its own:
  1. data preparation: the survey tax units, the SOI targets and the training table;
  2. pretraining: the networks, trained on the table;
  3. generation: each past year's records drawn from the networks, with the top of the file added;
  4. scaling: each record's dollar amounts put in target-year dollars;
  5. reweighting: weights solved to each year's national targets, then the state weights;
  6. output: each year's Tax-Simulator file, the CRS package and the reports.
The `training` command runs stages 1 and 2 into a training folder. The `years` command runs stages 3 to 6 for the past
years, then projects 2024 from 2023 and the later years from 2024. The `stage` command runs named stages (see STAGES)
on one folder. The training folder must hold two downloads made by hand: data/scf_rw/<wave>/ (the SCF replicate
weights) and targets/soi_cv/raw/ (SOI's coefficient-of-variation workbooks).

On a plain machine, every step runs here, one after another, with python and Rscript from the path. With --slurm,
each step is submitted as a Slurm job (or one job per year) that waits for the steps it needs, and the sbatch lines are
written to <folder>/jobs.txt. REBUILD_PY_ENV and REBUILD_R_ENV are shell lines run before each Python or R command, and
REBUILD_R_LIBS is the R library; slurm/bouchet_env.sh sets all three for Bouchet. FEATURES.md describes every stage.
"""
import argparse
import glob
import json
import os
import shutil
import subprocess
import sys

PIPELINE = 'research/rebuild/pipeline'
# The last past year (a year with published data), the first projection year, and the year the projections start from.
PAST_LAST, FUTURE_FIRST, BASE_YEAR = 2023, 2024, 2023


# ---- The stages ----

def py(module, *args):
    """A Python command of a stage: the module to run, and its arguments."""
    return ('py', module, [str(a) for a in args])


def r(script, libs=False):
    """An R command of a stage: the script to run, and whether it needs the R library REBUILD_R_LIBS."""
    return ('r', f'{PIPELINE}/{script}', libs)


# Each stage's commands, run in order. 'package' is the CRS package and its two round trips (run_package).
STAGES = {
    'scf': [py('prep.roster_scf'), r('prep/build_scf.R')],
    'cex': [py('prep.roster_cex'), r('prep/build_cex.R')],
    'filing': [py('prep.filing', 'units'), r('prep/filing_requirement.R', libs=True), py('prep.filing', 'calibrate')],
    'cps': [r('prep/build_cps.R', libs=True)],
    'sipp': [py('prep.roster_sipp')],
    'soi': [py('prep.soi')],
    'soi_cv': [py('prep.soi_cv')],
    'table': [py('prep.build_table')],
    'side_files': [py('prep.build_table', 'all_years')],
    'train': [py('pretrain.train')],
    'maps': [py('scale.maps')],
    'draw': [py('generate.generate')],
    'top': [py('generate.top')],
    'reweight': [py('reweight.tilt'), py('output.report_tilt')],
    'taxsim_file': [py('output.taxsim_file')],
    'package': ['package'],
    'report': [py('output.report_cex'), py('output.report_cps'), py('output.report_sipp'), py('output.report_translation')],
    'calculator': [py('output.calculator'), py('output.report_calculator')],
    'non_micro_tables': [py('resources_build.build_non_micro_tables')],
    'filing_season': [py('resources_build.build_filing_season')],
}
# The stages of the training command, and the steps of each past year in the years command (see Run).
TRAINING = ['scf', 'cex', 'filing', 'cps', 'sipp', 'soi', 'soi_cv', 'table', 'side_files', 'train']
PAST_YEAR = ['prep', 'draw', 'national', 'chances', 'integrated']


# ---- Running a stage ----

def say(*parts):
    print(*parts, flush=True)


def shell(line, env):
    """Run a shell line in the environment env, and stop if it fails."""
    say(f'+ {line}')
    subprocess.run(['bash', '-c', line], env=env, check=True)


def python_line(module, args):
    """The shell line that runs a Python stage, with the pipeline folder first on the module path."""
    prefix = os.environ.get('REBUILD_PY_ENV', '')
    python = 'python' if prefix else sys.executable
    line = f'PYTHONPATH={os.path.abspath(PIPELINE)}${{PYTHONPATH:+:$PYTHONPATH}} {python} -m {module} {" ".join(args)}'
    return f'{prefix}; {line.strip()}' if prefix else line.strip()


def run_command(cmd, env):
    """Run one command of a stage (see py and r)."""
    if cmd == 'package':
        return run_package(env)
    kind, what, extra = cmd
    if kind == 'py':
        return shell(python_line(what, extra), env)
    prefix, libs = os.environ.get('REBUILD_R_ENV', ''), os.environ.get('REBUILD_R_LIBS', '')
    line = (f'R_LIBS={libs} ' if extra and libs else '') + f'Rscript {what}'
    shell(f'{prefix}; {line}' if prefix else line, env)


def run_package(env):
    """Build the CRS package and check that its Python merge script rebuilds the Tax-Simulator file. Then run the R
    merge script and check its result in the same way."""
    run_command(py('output.package', 'build'), env)
    out = subprocess.run(['bash', '-c', python_line('output.package', ['paths'])], env=env, check=True,
                         capture_output=True, text=True).stdout
    pkg, puf, chk = out.strip().split('\n')[-1].split()
    r_prefix, libs = os.environ.get('REBUILD_R_ENV', ''), os.environ.get('REBUILD_R_LIBS', '')
    line = (f'R_LIBS={libs} ' if libs else '') + f'Rscript {pkg}/merge_puf.R {puf} {pkg} {chk}/merged_by_r.parquet'
    shell(f'{r_prefix}; {line}' if r_prefix else line, env)
    run_command(py('output.package', 'check_r'), env)


def stage_env(folder, run=None, **extra):
    """The environment a stage runs in: the folder it reads and writes (REBUILD_OUT), its settings file, and the run's
    subfolder name (REBUILD_RUN), plus any extra variables."""
    env = dict(os.environ, REBUILD_OUT=folder, REBUILD_SETTINGS=f'{folder}/settings.json', PYTHONDONTWRITEBYTECODE='1')
    if run:
        env['REBUILD_RUN'] = run
    env.update({k: str(v) for k, v in extra.items()})
    return env


def run_stages(folder, stages, run=None, **extra):
    """Run the named stages on one folder, in order. On a file built from the real PUF, the Tax-Simulator file stage
    is followed by the CRS package."""
    env = stage_env(folder, run, **extra)
    filers = json.load(open(env['REBUILD_SETTINGS']))['generation'].get('filers', 'synthetic')
    for s in stages:
        say(f'=== {s} {folder}')
        if s == 'side_files':
            side_files(folder, f'{folder}/sidebuild')
            continue
        for cmd in STAGES[s]:
            run_command(cmd, env)
        if s == 'taxsim_file' and filers == 'real_puf':
            run_package(env)


# ---- Folders and settings ----

def link(src, dst):
    """Make dst a symbolic link to src, replacing whatever is at dst."""
    if os.path.islink(dst) or os.path.exists(dst):
        os.remove(dst)
    os.symlink(src, dst)


def run_settings(src, dest, out=None, year=None):
    """Copy the settings file src to dest with paths.out set to out, or with years.target set to year. A bare PCE file
    name is made a full path, which the R stages need."""
    s = json.load(open(src))
    if out is not None:
        s['paths']['out'] = out
        pce = s['paths']['raw']['pce']
        if os.sep not in pce:
            s['paths']['raw']['pce'] = os.path.abspath(f'research/rebuild/resources/growth/{pce}')
    if year is not None:
        s['years']['target'] = int(year)
        assert s['generation'].get('filers', 'synthetic') == 'synthetic', 'the years command builds the synthetic file'
    json.dump(s, open(dest, 'w'), indent=1)


def training_settings(train):
    """A new training folder gets a copy of the pipeline's settings.json, with paths.out set to the folder."""
    if not os.path.exists(f'{train}/settings.json'):
        os.makedirs(train, exist_ok=True)
        run_settings(f'{PIPELINE}/settings.json', f'{train}/settings.json', out=os.path.abspath(train))


def years_from(text):
    """The years in a list such as '2015-2023' or '2019,2023'."""
    out = []
    for part in text.split(','):
        lo, _, hi = part.partition('-')
        out += list(range(int(lo), int(hi or lo) + 1))
    return out


# ---- The steps of the years command ----

class Run:
    """The steps of the years command, each for one year. The output folder holds: run/<year>, a past year's draw and
    national-only solve; final/<year>, each year's final file; starts, integrated, shares and files, the state weights;
    and checks, the checks.

    A past year runs prep (targets and dollar maps), draw (the records and the top of the file), national (the
    national-only solve), chances (the state chances, state_start.py) and integrated (the integrated solve,
    state_joint.py). A projection year runs future: the national solve from the 2023 file (or from 2024's), then the
    state weights from 2023's shares. check checks every year's state weights, and taxsim writes a year's
    Tax-Simulator file."""

    def __init__(self, train, out, side):
        self.train, self.out, self.side = os.path.abspath(train), os.path.abspath(out), os.path.abspath(side)
        sw = json.load(open(f'{PIPELINE}/settings.json'))['state_weights']
        self.soi_cv, self.lib = sw['soi_cv'], sw['lib']

    def past(self, y):
        return f'{self.out}/run/{y}'

    def final(self, y):
        return f'{self.out}/final/{y}'

    def step(self, name, y=None):
        getattr(self, 'step_' + name)(y)

    def step_prep(self, y):
        """Make the past year's folder: links to the training data, the side files, the networks, SOI's errors and
        the tax calculator, and a copy of the settings for the year. Then build the year's targets and dollar maps."""
        R = self.past(y)
        assert not os.path.exists(f'{R}/gen/y{y}'), f'{R} already holds a draw'
        for d in ('data/maps', 'targets', 'gen', 'tilt'):
            os.makedirs(f'{R}/{d}', exist_ok=True)
        for f in sorted(glob.glob(f'{self.train}/data/*')):
            if os.path.basename(f) != 'maps':
                link(f, f'{R}/data/{os.path.basename(f)}')
        for f in sorted(glob.glob(f'{self.side}/data/*_all.parquet')):
            link(f, f'{R}/data/{os.path.basename(f)}')
        link(f'{self.train}/model', f'{R}/model')
        link(self.soi_cv.replace('{year}', str(y)), f'{R}/targets/soi_cv')
        link(self.lib, f'{R}/lib')
        run_settings(f'{self.train}/settings.json', f'{R}/settings.json', year=y)
        run_stages(R, ['soi', 'maps'], run=f'y{y}')

    def step_draw(self, y):
        R = self.past(y)
        assert not os.path.exists(f'{R}/top_cache.pkl'), f'{R}/top_cache.pkl is already there'
        run_stages(R, ['draw', 'top'], run=f'y{y}')

    def step_national(self, y):
        R = self.past(y)
        assert os.path.getsize(f'{R}/top_cache.pkl') > 0
        assert not os.path.exists(f'{R}/tilt/y{y}/file_{y}.parquet'), f'{R} already holds a file'
        run_stages(R, ['reweight'], run=f'y{y}', REBUILD_SAVE_SOLVE=f'{R}/solve_inputs.pkl')

    def step_chances(self, y):
        R = self.past(y)
        os.makedirs(f'{self.out}/starts', exist_ok=True)
        run_command(py('reweight.state_start', '--file', f'{R}/tilt/y{y}/file_{y}.parquet', '--year', y,
                       '--out', f'{self.out}/starts/start_{y}.npz'), stage_env(R, f'y{y}'))

    def step_integrated(self, y):
        """Run the integrated solve. If it wrote its check, make the year's final folder: the national solve's files,
        with the year file and fit.csv from the integrated solve. A solve that fails still gets its final folder when
        its check was written; the failure is raised afterward."""
        R, F, I = self.past(y), self.final(y), f'{self.out}/integrated/y{y}'
        env = stage_env(R, f'y{y}')
        n = os.environ.get('REBUILD_THREADS') or os.environ.get('SLURM_CPUS_PER_TASK')
        if n:
            env.update(OMP_NUM_THREADS=n, OPENBLAS_NUM_THREADS=n, MKL_NUM_THREADS=n)
        failed = None
        try:
            run_command(py('reweight.state_joint', '--run', f'{self.out}/run', '--out', self.out, '--year', y,
                           '--start-dir', f'{self.out}/starts'), env)
        except subprocess.CalledProcessError as e:
            failed = e
        if os.path.exists(f'{I}/check.json') and os.path.getsize(f'{I}/check.json') > 0:
            os.makedirs(f'{F}/tilt/y{y}', exist_ok=True)
            for d in ('files', 'checks'):
                os.makedirs(f'{self.out}/{d}', exist_ok=True)
            for d in ('data', 'targets', 'model', 'lib', 'gen', 'settings.json'):
                link(f'{R}/{d}', f'{F}/{d}')
            for f in sorted(glob.glob(f'{R}/tilt/y{y}/*')):
                link(f, f'{F}/tilt/y{y}/{os.path.basename(f)}')
            link(f'{I}/tilt/y{y}/file_{y}.parquet', f'{F}/tilt/y{y}/file_{y}.parquet')
            link(f'{I}/fit.csv', f'{F}/tilt/y{y}/fit.csv')
            link(f'{I}/state_weights_{y}.parquet', f'{self.out}/files/state_weights_{y}.parquet')
            link(f'{I}/check.json', f'{self.out}/checks/check_{y}.json')
        if failed:
            raise failed

    def step_future(self, y):
        """A projection year: link the 2023 file (and 2024's, for a later year) into the year's folder, run the national
        solve, then give each record its 2023 shares across the areas."""
        B, F = f'{self.out}/final', self.final(y)
        assert os.path.getsize(f'{B}/{BASE_YEAR}/tilt/y{BASE_YEAR}/file_{BASE_YEAR}.parquet') > 0
        assert not os.path.exists(f'{F}/tilt/y{y}'), f'{F} already holds a file'
        os.makedirs(f'{F}/tilt', exist_ok=True)
        for d in ('data', 'targets', 'model', 'lib'):
            link(f'{B}/{BASE_YEAR}/{d}', f'{F}/{d}')
        link(f'{B}/{BASE_YEAR}/tilt/y{BASE_YEAR}', f'{F}/tilt/y{BASE_YEAR}')
        if y > FUTURE_FIRST:
            assert os.path.getsize(f'{B}/{FUTURE_FIRST}/tilt/y{FUTURE_FIRST}/file_{FUTURE_FIRST}.parquet') > 0
            link(f'{B}/{FUTURE_FIRST}/tilt/y{FUTURE_FIRST}', f'{F}/tilt/y{FUTURE_FIRST}')
        run_settings(f'{self.train}/settings.json', f'{F}/settings.json', year=y)
        run_stages(F, ['reweight'], run=f'y{y}')
        run_command(py('reweight.state_weights', '--run', B, '--out', self.out, '--year', y, '--carry'),
                    stage_env(F, f'y{y}'))

    def step_check(self, y=None):
        years = open(f'{self.out}/years.txt').read().strip()
        env = stage_env(self.train, None)
        env['REBUILD_OUT'] = self.out
        run_command(py('reweight.state_weights', '--run', f'{self.out}/final', '--out', self.out, '--year', 2015,
                       '--check', '--years', years), env)

    def step_taxsim(self, y):
        run_stages(self.final(y), ['taxsim_file'], run=f'y{y}')


# ---- Slurm ----

CPU = ['--partition=day', '--gres=gpu:0']
# Memory requests are about twice each step's measured peak.
RESOURCES = {
    'train': ['--cpus-per-task=4', '--mem=24G', '--time=08:00:00'],
    'prep': ['--cpus-per-task=4', '--mem=16G', '--time=01:30:00'],
    'draw': ['--cpus-per-task=4', '--mem=32G', '--time=04:00:00'],
    'national': ['--cpus-per-task=4', '--mem=56G', '--time=04:00:00'],
    'chances': ['--cpus-per-task=4', '--mem=12G', '--time=01:00:00'],
    'integrated': ['--cpus-per-task=16', '--mem=56G', '--time=06:00:00'],
    'future': ['--cpus-per-task=4', '--mem=40G', '--time=02:00:00'],
    'check': ['--cpus-per-task=1', '--mem=20G', '--time=01:00:00'],
    'taxsim': ['--cpus-per-task=2', '--mem=16G', '--time=01:00:00'],
    'data': ['--cpus-per-task=4', '--mem=32G', '--time=08:00:00'],
}
GPU_STEPS = {'train', 'draw', 'chances'}


class Slurm:
    def __init__(self, folder, gpu_partition):
        self.folder, self.gpu = folder, ['--partition=' + gpu_partition, '--gres=gpu:1']
        os.makedirs(f'{folder}/logs', exist_ok=True)

    def submit(self, name, step, args, array=None, after=None, kind='aftercorr'):
        """Submit one step as a Slurm job (an array of jobs, one per year, when array is given) that waits for the job
        after. kind is the kind of wait: 'aftercorr' waits for the same year's task, 'afterok' for the whole job to
        succeed, 'afterany' for it to end. Returns the job id."""
        cmd = ['sbatch', '--parsable', f'--job-name=rb_{name}', f'--output={self.folder}/logs/%x_%A_%a.log',
               '--kill-on-invalid-dep=yes', '--export=ALL,REBUILD_ROOT=' + os.getcwd()]
        cmd += (self.gpu if step in GPU_STEPS else CPU) + RESOURCES[step]
        if array:
            cmd.append(f'--array={array}')
        if after:
            cmd.append(f'--dependency={kind}:{after}')
        cmd += [f'{PIPELINE}/slurm/stage.sbatch'] + args
        with open(f'{self.folder}/jobs.txt', 'a') as fh:
            fh.write(' '.join(cmd) + '\n')
        job = subprocess.run(cmd, check=True, capture_output=True, text=True).stdout.strip().split(';')[0]
        say(f'{name}: job {job}')
        return job


# ---- The commands ----

def models(train):
    """The run names of the networks in settings generation.models."""
    return [m['run'] for m in json.load(open(f'{train}/settings.json'))['generation']['models']]


def side_files(train, side):
    """Build the side files: the training table built again with every survey year in place of each year's mix, in a
    copy of the training data. The years command uses only <side>/data/*_all.parquet."""
    shutil.rmtree(side, ignore_errors=True)
    os.makedirs(f'{side}/data/maps')
    for f in sorted(glob.glob(f'{train}/data/*')):
        b = os.path.basename(f)
        if b in ('maps', 'table.parquet'):
            continue
        (shutil.copytree if os.path.isdir(f) else shutil.copy2)(f, f'{side}/data/{b}')
    env = stage_env(train, None)
    env['REBUILD_OUT'] = side
    run_command(STAGES['side_files'][0], env)


def training(a):
    """The training command: data preparation, then one training run per network."""
    train = os.path.abspath(a.folder)
    training_settings(train)
    if a.slurm:
        sl = Slurm(train, a.gpu_partition)
        data = sl.submit('data', 'data', ['training-data', train])
        for m in models(train):
            sl.submit(f'train_{m}', 'train', ['train-model', train, m], after=data, kind='afterok')
        return
    training_data(train)
    for m in models(train):
        train_model(train, m)


def training_data(train):
    run_stages(train, [s for s in TRAINING if s != 'train'])


def train_model(train, m):
    run_stages(train, ['train'], run=m, REBUILD_SEED=int(m.split('seed')[-1]))


def years(a):
    """The years command: every past year's steps, then the projection years, then the check (and, unless left out,
    the Tax-Simulator files). With --slurm the steps are submitted as jobs, each waiting for the ones it needs."""
    # 1. Check the inputs and write the list of years.
    run = Run(a.train, a.out, a.side or f'{a.train}/sidebuild')
    past = years_from(a.past)
    future = list(range(FUTURE_FIRST, a.last_year + 1)) if a.last_year >= FUTURE_FIRST else []
    assert not future or BASE_YEAR in past, f'future years need {BASE_YEAR} among the past years'
    assert all(y <= PAST_LAST for y in past)
    assert os.path.exists(f'{run.train}/settings.json') and os.path.isdir(f'{run.train}/model'), f'no settings or networks in {run.train}'
    assert glob.glob(f'{run.side}/data/*_all.parquet'), f'no side files in {run.side}/data'
    assert not os.path.exists(f'{run.out}/run') and not os.path.exists(f'{run.out}/final'), f'{run.out} already holds a run'
    os.makedirs(run.out, exist_ok=True)
    open(f'{run.out}/years.txt', 'w').write(','.join(str(y) for y in past + future) + '\n')
    every = past + future

    # 2. On a plain machine, run every step here, in order.
    if not a.slurm:
        for y in past:
            for s in PAST_YEAR:
                run.step(s, y)
        for y in future:
            run.step('future', y)
        run.step('check')
        if a.taxsim:
            for y in every:
                run.step('taxsim', y)
        return

    # 3. With --slurm: one job array per past-year step, each year's task waiting for the same year's previous step;
    #    then 2024 (after 2023), the later years (after 2024), the check (after everything) and the Tax-Simulator files.
    sl = Slurm(run.out, a.gpu_partition)
    base = ['year-step', run.train, run.out, '--side', run.side]
    arr = ','.join(str(y) for y in past)
    job = None
    for s in PAST_YEAR:
        job = sl.submit(s, s, base + [s], array=arr, after=job)
    done = [job]
    if future:
        j24 = sl.submit('2024', 'future', base + ['future'], array=str(FUTURE_FIRST), after=f'{job}_{BASE_YEAR}', kind='afterok')
        done.append(j24)
        if a.last_year > FUTURE_FIRST:
            done.append(sl.submit('future', 'future', base + ['future'], array=f'{FUTURE_FIRST + 1}-{a.last_year}',
                                  after=j24, kind='afterok'))
    check = sl.submit('check', 'check', base + ['check'], after=':'.join(done), kind='afterany')
    if a.taxsim:
        sl.submit('taxsim', 'taxsim', base + ['taxsim'], array=','.join(str(y) for y in every), after=check, kind='afterok')
    say(f'CHECK_JOB {check}')


def build_parser():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest='command', required=True)
    p = sub.add_parser('training')
    p.add_argument('folder')
    p.add_argument('--slurm', action='store_true')
    p.add_argument('--gpu-partition', default='gpu_h100,gpu_h200')
    p = sub.add_parser('years')
    p.add_argument('train')
    p.add_argument('out')
    p.add_argument('--past', default=f'2015-{PAST_LAST}', help='past years, e.g. 2015-2023 or 2019,2023')
    p.add_argument('--last-year', type=int, default=2075)
    p.add_argument('--side', help='the side files\' folder (default <training folder>/sidebuild)')
    p.add_argument('--no-taxsim', dest='taxsim', action='store_false', help='leave out the Tax-Simulator files')
    p.add_argument('--slurm', action='store_true')
    p.add_argument('--gpu-partition', default='gpu_h100,gpu_h200')
    p = sub.add_parser('stage')
    p.add_argument('folder')
    p.add_argument('stages', nargs='+', choices=sorted(STAGES))
    p.add_argument('--run', help='REBUILD_RUN, the run\'s subfolder name (model/, gen/, tilt/)')
    # The single steps that the Slurm jobs run
    p = sub.add_parser('training-data')
    p.add_argument('train')
    p = sub.add_parser('train-model')
    p.add_argument('train')
    p.add_argument('model')
    p = sub.add_parser('year-step')
    p.add_argument('train')
    p.add_argument('out')
    p.add_argument('step', choices=PAST_YEAR + ['future', 'check', 'taxsim'])
    p.add_argument('--side', required=True)
    p.add_argument('--year', type=int)
    return ap


def main():
    a = build_parser().parse_args()
    os.chdir(os.environ.get('REBUILD_ROOT') or os.getcwd())
    assert os.path.isdir(PIPELINE), f'not a repository root: {os.getcwd()}'
    os.umask(os.umask(0) | 0o027)        # new files are not writable by the group or readable by others
    if a.command == 'training':
        training(a)
    elif a.command == 'years':
        years(a)
    elif a.command == 'stage':
        run_stages(os.path.abspath(a.folder), a.stages, run=a.run)
    elif a.command == 'training-data':
        training_data(os.path.abspath(a.train))
    elif a.command == 'train-model':
        train_model(os.path.abspath(a.train), a.model)
    else:
        Run(a.train, a.out, a.side).step(a.step, a.year)


if __name__ == '__main__':
    main()

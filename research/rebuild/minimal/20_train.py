"""
20_train.py  (minimal example, step 20: train one model, optionally with another seed)

Runs 02_model.py's main() unchanged, with one optional setting it lacks:
  MINIMAL20_TRAIN_SEED  the training seed (02_model.SEED: initial weights,
                        batch order, which slots are hidden). Unset: 02_model's
                        own seed, so the run is exactly `python 02_model.py`.
The held-back slice does not depend on this seed (02_model.HOLDBACK_SEED).
Everything else comes from 02_model.py's environment settings
(MINIMAL_TABLE, MINIMAL_MODEL_DIR, MINIMAL_ALL_RECORDS, ...).
"""
import importlib.util
import os

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('m', os.path.join(HERE, '02_model.py'))
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)

if __name__ == '__main__':
    if os.environ.get('MINIMAL20_TRAIN_SEED'):
        m.SEED = int(os.environ['MINIMAL20_TRAIN_SEED'])      # read by m.train at call time
        print('training seed', m.SEED, flush=True)
    m.main()

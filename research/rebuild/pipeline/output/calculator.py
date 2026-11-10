"""The optional calculator stage: every record of the year file through the GPU tax calculator (tax-simulator-gpu).
Writes the calculator's results, and beside them its input, the list of input columns and the conversion notes.
Reads <tilt>/file_<year>.parquet and writes <out>/calculator/<run>/; report_calculator.py reports on it.
"""
import json
import os

import pandas as pd

from shared import common
from shared.calculator_input import CONTRACT, convert

Y = common.TARGET_YEAR
INPUT = f'{common.TILT}/file_{Y}.parquet'
OUT = f'{common.OUT}/calculator/{common.RUN}'


def main():
    # 1. Convert the year file to the calculator's input, and write the input, the column list and the notes.
    from reweight import taxes as TX
    os.makedirs(OUT, exist_ok=True)
    common.say(f'calculator: tax-simulator-gpu at {TX.S["commit"]} (settings taxes)')
    common.say(f'input {INPUT}\noutputs {OUT}')
    f = pd.read_parquet(INPUT)
    frame, notes = convert(f)
    frame.to_parquet(f'{OUT}/calculator_input_{Y}.parquet', index=False)
    pd.DataFrame(CONTRACT, columns=['column', 'status', 'from the rebuild', 'what it is, or what would fill it']) \
        .to_csv(f'{OUT}/calculator_contract.csv', index=False)
    with open(f'{OUT}/conversion_notes.txt', 'w') as fh:
        fh.write('\n'.join(notes) + '\n')
    common.say('conversion:\n  ' + '\n  '.join(notes))
    counts = pd.Series([c[1] for c in CONTRACT]).value_counts()
    common.say('columns by status: ' + ', '.join(f'{k} {v}' for k, v in counts.items()))
    # 2. Run the calculator, and write its results and a record of the run (the manifest).
    out, man = TX.run_batch(frame, Y, OUT, 'calculator')
    out.to_parquet(f'{OUT}/calculator_{Y}.parquet', index=False)
    with open(f'{OUT}/calculator_manifest.json', 'w') as fh:
        json.dump(man, fh, indent=1)


if __name__ == '__main__':
    main()

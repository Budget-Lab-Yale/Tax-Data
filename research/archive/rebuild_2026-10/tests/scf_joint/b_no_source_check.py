"""Check the source ablation and backward-compatible checkpoint loading."""
import tempfile
from pathlib import Path

import torch

from b_model import (RecordTransformer, load_checkpoint, load_meta, load_split,
                     save_checkpoint, slot_variables)


def main():
    root = Path('/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint')
    data = root / 'data/expA_clean'
    meta = load_meta(data / 'meta.json')
    baseline, ck = load_checkpoint(
        root / 'model/expA_clean/checkpoint_joint_w1_seed01.pt', meta, 'cpu')
    assert baseline.use_source
    d = load_split(data, 'val', meta, ck['sources'])
    tok = torch.as_tensor(d['tok'][:8])
    hidden = tok < 0
    year = torch.as_tensor(d['year'][:8], dtype=torch.float32)
    src0 = torch.zeros(8, dtype=torch.long)
    src1 = torch.ones(8, dtype=torch.long)
    config = dict(ck['config'], use_source=False)
    model = RecordTransformer(slot_variables(meta), len(ck['sources']), **config)
    model.load_state_dict(baseline.state_dict())
    model.eval()
    with torch.no_grad():
        expected = model(tok, hidden, src0, year)
        assert torch.equal(expected, model(tok, hidden, src1, year))
        assert not torch.equal(baseline(tok, hidden, src0, year),
                               baseline(tok, hidden, src1, year))
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / 'check.pt'
        save_checkpoint(path, model, data / 'meta.json', ck['sources'], {})
        restored, _ = load_checkpoint(path, meta, 'cpu')
        assert not restored.use_source
        with torch.no_grad():
            assert torch.equal(expected, restored(tok, hidden, src1, year))
    # Both variants consume the same initialization random draws.
    torch.manual_seed(123)
    on = RecordTransformer(slot_variables(meta), len(ck['sources']), **ck['config'])
    torch.manual_seed(123)
    off = RecordTransformer(slot_variables(meta), len(ck['sources']), **config)
    assert all(torch.equal(v, off.state_dict()[k]) for k, v in on.state_dict().items())
    print('PASS: old checkpoint, source invariance, checkpoint round trip, matched initialization')


if __name__ == '__main__':
    main()

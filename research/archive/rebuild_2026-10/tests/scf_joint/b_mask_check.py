"""Check masking support, fixed-order prefixes, and likelihood loss scaling."""
import torch
from b_train import draw_hidden_sequence, sequence_weights


def main():
    torch.set_num_threads(1)
    obs = torch.tensor([[True, False, True, True], [False, True, True, False]])
    obs = obs.repeat(10000, 1)
    order = torch.tensor([2, 0, 3, 1])
    tok = torch.where(obs, 0, -1)
    weights = torch.linspace(0.1, 2, len(obs))
    for style in ['orderless', 'ordered']:
        gen = torch.Generator().manual_seed(33)
        hidden, scored = draw_hidden_sequence(obs, order, style, gen)
        assert (hidden | obs).all()  # missing values never visible
        assert not (scored & ~obs).any()
        assert not (scored & ~hidden).any()
        assert scored.any(dim=1).all()
        adjusted = sequence_weights(tok, scored, weights)
        assert torch.allclose(adjusted * scored.sum(dim=1), weights * obs.sum(dim=1))
        nvisible = (~hidden)[::2].sum(dim=1)
        shares = torch.bincount(nvisible, minlength=3).float() / len(nvisible)
        assert ((shares - 1/3).abs() < 0.025).all(), shares
        if style == 'ordered':
            assert (scored.sum(dim=1) == 1).all()
            for i in range(100):
                observed_order = order[obs[i, order]]
                target = scored[i].nonzero().item()
                pos = (observed_order == target).nonzero().item()
                expected = torch.zeros(4, dtype=torch.bool)
                expected[observed_order[:pos]] = True
                assert torch.equal(~hidden[i], expected)
        print(style, 'PASS: valid masks, uniform prefix lengths, fixed weighted denominator')


if __name__ == '__main__':
    main()

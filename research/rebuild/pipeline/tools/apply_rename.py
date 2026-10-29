"""Apply the rename of 29-30 October (John: "there is no reason for the names to differ. at all.") to the rebuild's
files: every row of rename_map.csv with kind "renamed" turns the rebuild's old name into tax-simulator-gpu's input
name (or production Tax-Data's, where the calculator does not read the item). Written so it can be re-applied to
another branch made before the rename:

    python research/rebuild/pipeline/tools/apply_rename.py [--check] [files or folders ...]

(default: research/rebuild, less HISTORY.md, the archive and this folder). --check lists what would change and writes
nothing. Run from the repository root. Pure text; reads no data.

Rules, by the shape of the old name:
  * a name with an underscore (sch_c, w_cash, age_primary, ...) is replaced wherever it stands as a whole name,
    including its derived forms: the band column (<name>_band), the reported slot (<name>_reported,
    <name>_reported_band), the drawn column (<name>_drawn) and the uniform-draw column (u_<name>), and the survey's raw
    column (raw_<name>). "Whole name": not preceded by a letter, digit or underscore, not followed by a letter, digit
    or underscore.
  * a one-word name (interest, unemployment, childcare, mars) is also an English word or a name used in other senses,
    so it is replaced only where it is plainly a name: as an exact string ('interest', "interest", `interest`), as a
    whole value or key in YAML, as an attribute or a data.table / dplyr column in code (x.interest, x$interest,
    interest = ...), never in comments or prose.
A file whose new text would still hold a name in the rename's old list is reported (most are prose; see --check).
Names built in code from pieces ('w_' + asset) are not found by this script and are fixed by hand.
"""
import csv
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
MAP = os.path.join(HERE, 'rename_map.csv')
EXT = ('.py', '.R', '.json', '.yaml', '.md', '.csv', '.sbatch', '.txt')
SKIP = ('HISTORY.md', 'rename_map.csv', 'apply_rename.py')
# The shared tax-unit rules' own person income fields (wage, selfemp, interest, dividends, rents, ...) are not the
# file's items: 'interest' there is the person's interest or property income, read from keyword arguments the rules
# cannot see renamed (roster_cps.py), so it keeps its name in these files.
KEEP_ONE_WORD = {'tax_units.py': {'interest'}, 'filing.py': {'interest'}, 'roster_sipp.py': {'interest'},
                 'roster_cps.py': {'interest'}, 'roster_cex.py': {'interest'}, 'check_units.py': {'interest'}}
PREFIX = r'(?:u_|raw_|own_)?'
SUFFIX = r'(?:_reported_band|_reported|_band|_drawn)?'
STRING = re.compile(r"""('(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*")""")


def renames():
    rows = [r for r in csv.DictReader(open(MAP)) if r['kind'] == 'renamed']
    return [(r['old_name'], r['new_name']) for r in rows]


def multi_part(old, new):
    pat = re.compile(r'(?<![A-Za-z0-9_])(' + PREFIX + ')' + re.escape(old) + '(' + SUFFIX + r')(?![A-Za-z0-9_])')
    return lambda text: pat.sub(lambda m: m.group(1) + new + m.group(2), text)


def code_and_comment(line, ext):
    """Split a line of Python or R at its comment (a # outside quotes)."""
    q = None
    for i, ch in enumerate(line):
        if q:
            if ch == '\\':
                continue
            if ch == q:
                q = None
        elif ch in '\'"':
            q = ch
        elif ch == '#':
            return line[:i], line[i:]
    return line, ''


def one_word(old, new, ext):
    quoted = re.compile(r'([\'"`])' + re.escape(old) + r'\1')
    if ext in ('.md', '.txt'):
        tick = re.compile(r'`' + re.escape(old) + r'`')
        return lambda text: tick.sub('`' + new + '`', text)
    if ext == '.json':
        return lambda text: re.sub(r'"' + re.escape(old) + r'"', '"' + new + '"', text)
    if ext == '.csv':
        cell = re.compile(r'(?<![^,\n])' + re.escape(old) + r'(?![^,\n])')
        return lambda text: quoted.sub(lambda m: m.group(1) + new + m.group(1), cell.sub(new, text))
    if ext == '.yaml':
        # a whole scalar: the key at the start of a line, or the whole value after "key: ", "- ", "[" or ", ", ending
        # at ",", "]", "}", ":" or the end of the line (so "taxable interest", an SOI line's name, is left alone)
        scalar = re.compile(r'((?:^|[\[{,:]|^\s*-)\s*)' + re.escape(old) + r'(?=\s*(?:[,\]}:]|$))', re.M)
        def yaml_sub(text):
            out = []
            for line in text.split('\n'):
                body, comment = (line.split('#', 1) + [''])[:2] if '#' in line else (line, None)
                body = quoted.sub(lambda m: m.group(1) + new + m.group(1), scalar.sub(lambda m: m.group(1) + new, body))
                out.append(body + ('#' + comment if comment is not None else ''))
            return '\n'.join(out)
        return yaml_sub
    # Python and R code: quoted names, attributes (.x, $x), and in R bare column names in code
    attr = re.compile(r'(?<=[.$])' + re.escape(old) + r'(?![A-Za-z0-9_(])')
    bare = re.compile(r'(?<![A-Za-z0-9_.$\'"])' + re.escape(old) + r'(?![A-Za-z0-9_\'"(])')
    kwarg_pat = re.compile(r'([(,]\s*)' + re.escape(old) + r'(\s*=(?!=))')   # a keyword argument: x.assign(mars=0)

    class kwarg:   # only inside an open parenthesis, so a tuple assignment (a, mars = ...) is left alone
        @staticmethod
        def sub(f, s):
            return kwarg_pat.sub(lambda m: f(m) if s[:m.start()].count('(') > s[:m.start()].count(')') else m.group(0), s)
    def code_sub(text):
        out, in_doc = [], False
        for line in text.split('\n'):
            if ext == '.py' and line.count('"""') % 2 == 1:
                in_doc = not in_doc
                out.append(line)
                continue
            if in_doc:
                out.append(line)
                continue
            code, comment = code_and_comment(line, ext)
            parts = STRING.split(code)          # string literals at odd positions: only an exact name is renamed there
            for i, part in enumerate(parts):
                if i % 2:
                    parts[i] = quoted.sub(lambda m: m.group(1) + new + m.group(1), part)
                else:
                    parts[i] = kwarg.sub(lambda m: m.group(1) + new + m.group(2), attr.sub(new, part))
                    if ext == '.R':
                        parts[i] = bare.sub(new, parts[i])
            code = ''.join(parts)
            out.append(code + comment)
        return '\n'.join(out)
    return code_sub


def rename_text(new, pairs, ext, path):
    keep = KEEP_ONE_WORD.get(os.path.basename(path), set())
    for old, nm in pairs:
        if old in keep:
            continue
        new = (multi_part(old, nm) if ('_' in old or '.' in old) else one_word(old, nm, ext))(new)
    return new


def files(args):
    roots = args or ['research/rebuild']
    for root in roots:
        if os.path.isfile(root):
            yield root
            continue
        for d, sub, names in os.walk(root):
            sub[:] = [s for s in sub if s not in ('archive', '__pycache__', 'raw', 'tools')]
            for n in names:
                if n.endswith(EXT) and n not in SKIP:
                    yield os.path.join(d, n)


def main():
    check = '--check' in sys.argv
    args = [a for a in sys.argv[1:] if a != '--check']
    pairs = renames()
    changed = 0
    for path in files(args):
        ext = os.path.splitext(path)[1]
        text = open(path).read()
        # text between the markers <!-- old names --> and <!-- /old names --> (docs that describe the rename) is left
        pieces = re.split(r'(<!-- old names -->.*?<!-- /old names -->)', text, flags=re.S)
        new = ''.join(p if p.startswith('<!-- old names -->') else rename_text(p, pairs, ext, path) for p in pieces)
        if new != text:
            changed += 1
            n = sum(1 for a, b in zip(text.split('\n'), new.split('\n')) if a != b)
            print(f'{path}: {n} lines')
            if not check:
                with open(path, 'w') as fh:
                    fh.write(new)
    print(f'{changed} files {"would change" if check else "changed"}')


if __name__ == '__main__':
    main()

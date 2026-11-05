# Handoff: put real model probabilities and real numbers in the talk (28 October)

John's request: replace made-up probabilities with real ones from the latest model wherever possible,
and made-up numbers with real ones wherever they appear. Main deck: rebuild_talk.tex (version B plus
A's record slide plus the sentence slide). Build: module load texlive/20240312-GCC-13.3.0 && latexmk -pdf.
Render pages with python3 + fitz in a separate shell (the texlive module swaps Python).

Slides with made-up numbers (the sentence slide stays made up; it is a language-model illustration):
1. "The model's answer: a probability for every range": interest given PUF, 2015, married, wages
   ~$142k range. Needs a real model query (zero share + the 73 positive bands).
2. "Writing one record, one item at a time": every bar (married, dependents, age range, wages bands,
   interest bands) and the record itself. Best: draw one real record with the latest model and record
   the real probabilities at each step (AGI group preset 75th-90th percentile, PUF label, 2015).
3. "How has this been done before?" table (intro): made-up records; could use real PUF-style values,
   but the PUF is confidential: ask John before showing any real PUF record.
4. "Post-training" slide: already real (FEATURES.md, latest run). "Challenge: the weights": real (FEATURES.md).
5. Translation slides: real (2015 PUF vs 2016 SCF, 21 October report).

Real numbers already gathered by variant D (variants/D/NOTES.md and the comment block in its .tex):
real wage band edges (data/band_edges.json, puf/2015/wages), a real 2019 record (id 87865 in
tilt/seed20261016/file_2019.parquet), before/after fit for six targets (tilt/<run>/fit.csv).
Caution: the PUF is confidential; generated records are synthetic, but check with John before
showing band edges at the very top.

Model queries need a GPU job (sbatch, gpu partition); see research/rebuild/pipeline/model.py and
generate.py for how to load the trained models under scratch rebuild/model/<run>/.

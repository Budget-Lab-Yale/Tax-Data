Variant C: the method in pictures (age and wages as the toy example)

1. Training: learn the shape of the data. Real records as dots, then the shaded joint density the model learns; training is "hide some values, guess them from the rest, nudge toward the truth".
2. Asking the model a question: take a slice. Wages given age 45-54 (click 1) and 65+ (click 2) as probabilities over ranges; every amount has 84 ranges (zero, 10 losses, 73 positive, finer at the top).
3. Writing a record (3 clicks): draw age range; draw wages from the slice at that age, then a real dollar amount from that range; 130 drawn records land where the data are dense.
4. A source without a variable still helps: net worth given age and wages comes from the SCF only; PUF records are never asked it but shape the age-wage picture; survey-only variables are drawn after the tax ones, "as the SCF would see the household".
5. Post-training: reweight the drawn records. Same dots, shaded by weight; smallest change in weights that puts each of 2019's 746 totals within its standard error (half for SCF wealth).

Unsure or simplified:
- Generation order: the PUF variables come in a random order, with the AGI group set first; slide 3 shows age drawn first, for illustration. On the PUF, age is a range (exact ages come from the surveys); the slides use ranges.
- "Half a standard error for the SCF's wealth totals": FEATURES.md says 0.5 for the SCF target kinds, which are wealth, the wealth grid and auto loans.
- "746 targets" is the count FEATURES.md gives for 2019; check it against the latest run.
- All shapes and probabilities are made up (scratch script), marked "Illustration."
PDF pages 13-20 (slide numbers 9-13). Pictures and points made up in a scratch script; compiled with one extra pdflatex pass after the last edit.

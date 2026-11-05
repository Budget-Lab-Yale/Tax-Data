Variant B: the method section, built from econometrics (replaces the four old method slides). PDF pages 13-23 (slide numbers 9-14; slide 12 has 6 clicks, pages 16-21).

1. From one model per variable to one model for every question: before, Y given X fitted separately per variable; now the joint written as a chain of conditionals, so one model answering any "this given those" holds the joint.
2. What the model returns: a probability for every range: 84 ranges per amount (zero, 10 loss, 73 positive percentile groups, finer at the top); a full distribution like a multinomial logit; inputs are source, year and whatever is known.
3. Fitting it: maximum likelihood on randomly chosen questions: hide a random share of a real record, score the log probability on the true range, weighted by survey weight (scaled up at the top); one equation.
4. Writing a record: draw down the chain (6 clicks): AGI group set first, then each variable drawn given all earlier ones; survey items asked as the SCF / CEX would see the unit; ranges become dollars from a real record in the range, grown to the target year; totals added up from parts.
5. The stack of drawn records: 400,000 filers by AGI group, weight = group's returns / records drawn; 54,379 non-filers under the CPS label.
6. Post-training: the smallest change to the weights that hits the targets: minimize the raking distance subject to each target within +-1 SE (0.5 for SCF); solution w0 x exp(sum of multipliers); 753 of 753 met, weight change 0.05 to 3.2 (1st-99th pct), 87% effective sample kept.

Unsure: "about 1.4 million coefficients" is the network's parameter count (FEATURES.md); "each top range holds 0.01% of holders" paraphrases "0.01% of positive weight"; the distance formula on the slide uses weights where solver.py uses weight shares (said in the slide note); latest-run numbers are from FEATURES.md "How the latest run comes out" (27 October).

#!/bin/bash
#SBATCH --job-name=report_v3
#SBATCH --partition=day
#SBATCH --time=00:10:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=8G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_report_v3.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_report_v3.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026042712/baseline

# Step 1: build AM_STATUS.md from saved microdata.
Rscript src/eda/report_v3.R "$OUTPUT_DIR"

# Step 2: render to HTML (re-uses slurm_render_status.sh's R script body).
Rscript -e '
if (exists("mark_html", where = asNamespace("markdown"))) {
  body = markdown::mark_html(file = "AM_STATUS.md", template = FALSE)
} else {
  body = markdown::markdownToHTML(file = "AM_STATUS.md", fragment.only = TRUE,
                                    options = c("toc", "fragment_only"))
}
css = "
body { font-family: -apple-system, BlinkMacSystemFont, Helvetica, Arial, sans-serif;
       max-width: 1100px; margin: 2em auto; padding: 0 1em; line-height: 1.5; color: #222; }
h1 { border-bottom: 2px solid #333; padding-bottom: 0.3em; }
h2 { border-bottom: 1px solid #ccc; padding-bottom: 0.2em; margin-top: 2em; }
h3 { margin-top: 1.5em; }
code { background: #f4f4f4; padding: 0.1em 0.3em; border-radius: 3px; font-size: 0.92em; }
pre { background: #f4f4f4; padding: 0.8em; border-radius: 4px; overflow-x: auto;
      border-left: 3px solid #999; }
pre code { background: transparent; padding: 0; }
table { border-collapse: collapse; margin: 1em 0; font-variant-numeric: tabular-nums; }
th, td { border: 1px solid #bbb; padding: 0.4em 0.8em; text-align: left; }
th { background: #f0f0f0; }
td:not(:first-child) { text-align: right; }
tr:nth-child(even) { background: #fafafa; }
strong { color: #1a3a6a; }
"
html = paste0(
  "<!doctype html><html><head><meta charset=\"utf-8\">",
  "<title>Tilt Wealth Imputation — v3 Report</title>",
  "<style>", css, "</style></head><body>",
  body, "</body></html>"
)
writeLines(html, "AM_STATUS.html")
cat(sprintf("rendered AM_STATUS.html (%d bytes)\n", nchar(html)))
'

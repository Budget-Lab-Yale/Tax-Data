#!/usr/bin/env bash
#---------------------------------------------------------------------------
# build_forbes_input.sh
#
# Builds resources/forbes/forbes_billionaires_2022_2025.csv from the
# komed3/rtb-api Forbes real-time-billionaires snapshots (MIT licensed,
# https://github.com/komed3/rtb-api).
#
# One year-end (Dec 31) snapshot per list year 2022-2025. The snapshot list
# is global; we filter to US citizens (Tax-Data is US tax microdata) and
# RE-RANK within the US subset by net worth (1..N). BSYZ rank groups
# (top100 / next300 / rest) are US-tax-based, so US rank is the consistent
# bucketing basis — the raw Forbes rank is global and would misassign US
# billionaires.
#
# Net worth in the source is in $millions; we convert to dollars (× 1e6)
# because src/forbes_splice.R::read_forbes_input expects dollars.
#
# Output columns match read_forbes_input's contract:
#   required: year, rank, name, net_worth, source_category
#   optional (audit, not consumed by the v1 splice):
#            age1, male1, country, sector, notes, forbes_uri
#
# Requires: curl, jq. Run from repo root:
#   bash resources/forbes/build_forbes_input.sh
#---------------------------------------------------------------------------
set -euo pipefail

DATES=(2022-12-31 2023-12-31 2024-12-31 2025-12-31)
BASE_URL="https://raw.githubusercontent.com/komed3/rtb-api/main/api/list/rtb"
OUT="resources/forbes/forbes_billionaires_2022_2025.csv"
RAW_DIR="$(mktemp -d)"

echo "year,rank,name,net_worth,source_category,age1,male1,country,sector,notes,forbes_uri" > "$OUT"

for d in "${DATES[@]}"; do
  year="${d%%-*}"
  raw="$RAW_DIR/$d.json"

  ok=0
  for attempt in 1 2 3; do
    curl -sL --max-time 60 "$BASE_URL/$d" -o "$raw" || true
    if jq -e '.list | length > 0' "$raw" >/dev/null 2>&1; then ok=1; break; fi
    echo "  $d: fetch attempt $attempt failed, retrying" >&2
    sleep 4
  done
  [ "$ok" -eq 1 ] || { echo "ERROR: could not fetch $d" >&2; exit 1; }

  # Filter to US citizens, preserve net-worth order (source list is already
  # sorted by global rank = descending net worth), re-rank 1..N within US.
  jq -r --arg year "$year" '
    [ .list[] | select(.citizenship == "us") ]
    | to_entries
    | .[]
    | ($year | tonumber) as $y
    | (.key + 1) as $us_rank
    | .value as $b
    # Drop empty-string array members up front: some records carry
    # industry:[] with source:[""] (a one-element array holding ""), which
    # would otherwise pass a length check and join to blank.
    | ($b.industry // [] | map(select(. != ""))) as $ind
    | ($b.source   // [] | map(select(. != ""))) as $src
    | [
        $y,
        $us_rank,
        $b.name,
        (($b.networth // 0) * 1000000 | floor),
        # source_category: Forbes industry, falling back to the source/company
        # label, then "unknown". A handful of entertainers (Springsteen,
        # Seinfeld, ...) carry no industry tag. Pure audit metadata in the
        # splice, but kept non-empty so the required column is never NA.
        (if   ($ind | length) > 0 then ($ind | join(";"))
         elif ($src | length) > 0 then ($src | join(";"))
         else "unknown" end),
        ($b.age // ""),
        (if   $b.gender == "m" then 1
         elif $b.gender == "w" then 0
         else "" end),
        ($b.citizenship // ""),
        ($ind | join(";")),
        ($src | join(";")),
        ($b.uri // "")
      ]
    | @csv
  ' "$raw" >> "$OUT"

  n_us=$(jq '[ .list[] | select(.citizenship == "us") ] | length' "$raw")
  echo "  $year ($d): $n_us US billionaires"
done

rm -rf "$RAW_DIR"
echo "Wrote $OUT ($(( $(wc -l < "$OUT") - 1 )) rows)"

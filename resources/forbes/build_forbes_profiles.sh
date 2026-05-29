#!/usr/bin/env bash
#---------------------------------------------------------------------------
# build_forbes_profiles.sh
#
# Per-profile enrichment for the Forbes splice v2 (assemble-don't-copy).
# build_forbes_input.sh produces one row per (US billionaire x list year)
# with year/rank/name/net_worth and a forbes_uri. This script pulls the
# per-profile detail that v2 assembles records from:
#
#   /info   -> demographics: birthDate, gender, maritalStatus, children,
#              residence state, selfMade.type (the founder signal), family
#   /assets -> public ticker'd holdings, used to split net worth into a
#              public-equity share vs a private residual
#
# FETCH-ONCE, LATEST SNAPSHOT (not per year):
#   - /assets is latest-only at the source; v2 scales the latest composition
#     to each year's net worth (see resources/forbes/README.md).
#   - /info fields are either stable (birthDate, gender) or point-in-time
#     facts we accept at their latest value (maritalStatus, children).
#   One pull per UNIQUE uri (~1k) instead of per (uri x year) (~3.2k) avoids
#   thousands of redundant calls.
#
# Output: resources/forbes/forbes_profiles.csv, one row per uri. Joined onto
# the per-year base by read_forbes_input() (src/forbes_splice.R).
#
# Raw JSON is cached under resources/forbes/cache/ (gitignored) so a re-run
# resumes instead of re-fetching — a ~2k-request job is fragile over one pass.
#
# Requires: curl, jq. Run from repo root:
#   bash resources/forbes/build_forbes_profiles.sh
#---------------------------------------------------------------------------
set -euo pipefail

BASE_URL="https://cdn.statically.io/gh/komed3/rtb-api/main/api/profile"
IN="resources/forbes/forbes_billionaires_2022_2025.csv"
OUT="resources/forbes/forbes_profiles.csv"
CACHE="resources/forbes/cache"
mkdir -p "$CACHE"

[ -f "$IN" ] || { echo "ERROR: $IN not found; run build_forbes_input.sh first" >&2; exit 1; }

# Unique non-empty forbes_uri values. forbes_uri is the last CSV column;
# strip surrounding quotes, drop blanks, dedupe.
mapfile -t URIS < <(tail -n +2 "$IN" | awk -F',' '{print $NF}' \
  | sed 's/"//g' | grep -v '^$' | sort -u)
n_total=${#URIS[@]}
echo "Enriching $n_total unique Forbes profiles..."

# Fetch $1 (endpoint) for uri $2 into cache file $3, with retries. Treats a
# JSON parse as the success signal (the CDN occasionally returns HTML/empty
# on a cold edge). A 404 (no such endpoint for that profile) is cached as a
# sentinel so we don't retry it forever.
fetch_json() {
  local endpoint="$1" uri="$2" dest="$3"
  if [ -s "$dest" ] && jq -e . "$dest" >/dev/null 2>&1; then return 0; fi
  for attempt in 1 2 3; do
    curl -sL --max-time 30 "$BASE_URL/$uri/$endpoint" -o "$dest" || true
    if jq -e . "$dest" >/dev/null 2>&1; then return 0; fi
    sleep 2
  done
  echo "null" > "$dest"   # sentinel: unparseable after retries
  return 0
}

# CSV header. self_made_type is the founder signal (self-made vs inherited);
# public_equity_interactive sums the "live tracker" direct stake, _all sums
# every holding row (options + share classes) for diagnostics / fallback.
echo "uri,birth_date,gender,marital_status,children,family,self_made,self_made_type,residence_state,industry,n_assets,public_equity_interactive,public_equity_all" > "$OUT"

i=0
for uri in "${URIS[@]}"; do
  i=$((i + 1))
  info="$CACHE/${uri}.info.json"
  assets="$CACHE/${uri}.assets.json"
  fetch_json info   "$uri" "$info"
  fetch_json assets "$uri" "$assets"

  # --- /info row (sentinel "null" -> all-empty, handled by jq // "") --------
  info_row=$(jq -r '
    [ .uri,
      (.birthDate // ""),
      (.gender // ""),
      (.maritalStatus // ""),
      (.children // ""),
      (.family // false),
      (.selfMade._is // false),
      (.selfMade.type // ""),
      (.residence.state // ""),
      ((.industry // []) | map(select(. != "")) | join(";"))
    ] | @csv' "$info" 2>/dev/null || echo "")

  if [ -z "$info_row" ]; then
    # /info unparseable: emit a uri-only row so the join still resolves.
    info_row="\"$uri\",\"\",\"\",\"\",\"\",false,false,\"\",\"\",\"\""
  fi

  # --- /assets aggregates ---------------------------------------------------
  # Value a holding as numberOfShares x sharePrice in its native currency.
  # exchangeRate is left out deliberately: ~all US-billionaire holdings are
  # USD (exchangeRate 1.0), the composition is reconciled to net_worth
  # downstream so only the public/total RATIO matters, and the rate's
  # direction is unlabeled in the source. Foreign holdings are a documented
  # minor gap. Options carry interactive=false; the live direct stake is
  # interactive=true. We report both sums and let R reconcile.
  asset_row=$(jq -r '
    if type == "array" then
      [ (length),
        ([ .[] | select(.interactive == true)
           | (.numberOfShares // 0) * (.sharePrice // 0) ] | add // 0),
        ([ .[] | (.numberOfShares // 0) * (.sharePrice // 0) ] | add // 0)
      ] | @csv
    else "0,0,0" end' "$assets" 2>/dev/null || echo "0,0,0")
  [ -n "$asset_row" ] || asset_row="0,0,0"

  echo "${info_row},${asset_row}" >> "$OUT"

  if [ $((i % 100)) -eq 0 ]; then echo "  $i / $n_total"; fi
done

echo "Wrote $OUT ($(( $(wc -l < "$OUT") - 1 )) profiles)"

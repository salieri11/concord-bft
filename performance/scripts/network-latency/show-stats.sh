#!/bin/bash

raw_to_json() {
  pingparsing raw/*/* >stats.json
}

json_to_tsv() {
  local header="Source          Target          Min     Max     Avg     Mdev"
  echo "$header"
  echo "============================================================="

  local filter="to_entries | sort_by(.value.rtt_avg)[] | [(.key | split(\"/\")[1]), .value.destination, .value.rtt_min, .value.rtt_max, .value.rtt_avg, .value.rtt_mdev] | @tsv"
  jq -r "$filter" "stats.json"
}

main() {
  raw_to_json
  json_to_tsv
}

main

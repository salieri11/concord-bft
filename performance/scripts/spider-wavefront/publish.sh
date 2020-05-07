#!/bin/bash

# input: https://github.com/gatling/gatling/blob/master/gatling-charts/src/main/scala/io/gatling/charts/template/GlobalStatsJsonTemplate.scala
# output: https://docs.wavefront.com/wavefront_data_format.html#metrics-data-format-syntax

# Response time for each request
response_time() {
  local file="$dir/simulation.log"
  local metric="chessplus.raw.responseTime.ms"

  awk -v m=$metric -v s="$source" -v d="$date" \
    'BEGIN {OFMT = "%.0f"} /^REQUEST/ {print m, $5-$4, $4/1000, "source="s, "type="$3, "status="$6, "user="$2, "date="d}' "$file"
}

# Count of initiated requests, grouped by second
request_rate() {
  local metric="chessplus.rps.initiated"
  local index="0"
  rps $metric $index
}

# Count of completed requests, grouped by second
response_rate() {
  local metric="chessplus.rps.completed"
  local index="1"
  rps $metric $index
}

# Count of either initiated or completed requests, grouped by second
rps() {
  local metric=$1
  local index=$2
  local file="$dir/simulation.log"

  awk -v i="$index" \
    'BEGIN {OFMT = "%.0f"} /^REQUEST/ {print $3, $(4+i)/1000 ,$6}' "$file" |
    sort |
    uniq -c |
    awk -v m="$metric" -v s="$source" -v d="$date" \
      'BEGIN {OFMT = "%.0f"} {print m, $1, $3, "source="s, "type="$2, "status="$4, "date="d}'
}

# Statistics from aggregate functions
aggregate_stats() {
  local file="$dir/js/stats.json"
  local prefix="chessplus.stats."
  local tags="[\"concurrency=$concurrency\", \"blockchain=$blockchain\"]"

  local filter="
      def round: . * 100 + 0.5 | floor / 100;
      (\"$date\" | fromdateiso8601) as \$timestamp
      | .stats, .contents[].stats | delpaths([[\"group1\"],[\"group2\"],[\"group3\"],[\"group4\"]])
      | (.name |  sub(\"\\\\s+\"; \"-\") | ascii_downcase) as \$type | del(.name) | to_entries[]
      | (.key | \"$prefix\" + \$metrics[0][.]) as \$metric | .value | to_entries[]
      | [\$metric, (.value | tonumber | round), \$timestamp, \"source=$source\", \"type=\"+\$type, \"status=\"+.key ] + $tags
      | @tsv
  "
  jq --slurpfile metrics metrics.json --raw-output "$filter" "$file"
}

publish() {
  nc -N "$host" 2878
}

main() {
  init

  response_time | publish
  request_rate | publish
  response_rate | publish
  aggregate_stats | publish
}

# Assign global parameters
init() {
  # Run date extracted from the report directory name and converted to ISO 8601 format.
  date=$(echo "$dir" | sed -E 's/.*-([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{3})\/?$/\1-\2-\3T\4:\5:\6Z/g')

  # Concurrency number extracted from the simulation logs.
  concurrency=$(grep RUN "$dir"/simulation.log | sed -E 's/.*--concurrency ([0-9]+).*/\1/g')
}

# help
usage() {
  echo "This script sends Gatling report to Wavefront."
  echo "options:"
  echo "-g     Gatling report directory"
  echo "-w     Wavefront proxy IP"
  echo "-s     Wavefront tag for source"
  echo "-b     Wavefront tag for blockchain"
}

# collect parameters
while [[ "$#" -gt 0 ]]; do
  case $1 in
  -g)
    dir="$2"
    shift
    ;;
  -w)
    host="$2"
    shift
    ;;
  -s)
    source="$2"
    shift
    ;;
  -b)
    blockchain="$2"
    shift
    ;;
  *)
    echo "Unknown parameter: $1"
    usage
    exit 1
    ;;
  esac
  shift
done

# validate parameters
if [ ! "$dir" ] || [ ! "$host" ] || [ ! "$source" ] || [ ! "$blockchain" ]; then
  usage
  exit 1
fi

# validate presence input dir
if [ ! -d "$dir" ]; then
  echo "Report [$dir] does not exist"
  exit
fi

# validate format of IP
if [[ ! "$host" =~ ^([0-9]+\.){3}[0-9]+$ ]]; then
  echo "Wavefront proxy [$host] is not a valid IP"
  exit
fi

main

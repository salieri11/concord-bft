IFS='
'
# dont use -it since this is not an interactive session
for x in $(sudo docker exec "$1" /concord/concord-ctl perf list); do
  y=$(echo "$x"|tr -d '\r')
  if [ -z "$y" ]; then
    continue
  fi
  y=$(echo "$x"|tr -d '\r')
  echo "$y"
  sudo docker exec "$1" /concord/concord-ctl perf snapshot "$y"
  res=$(sudo docker exec "$1" /concord/concord-ctl perf get "$y")
  echo "$res"
done
# Integration of CHESS+ Spider with Wavefront

## Overview

CHESS+ load-runner generates [Gatling reports](https://gatling.io/docs/current/general/reports/) at end of a test run. This script converts this report to [Wavefront data format](https://docs.wavefront.com/wavefront_data_format.html#metrics-data-format-syntax) and sends it to Wavefront.

## Prerequisites

[jq](https://stedolan.github.io/jq/) is used here to convert from JSON to TSV.

```bash
sudo apt install jq
```

## Details

### Raw Metrics

simulation.log is parsed to get the following metrics.

* Metrics: chessplus.raw.responseTime.ms
* Tags
    * source
    * type
    * status
    * user
    * concurrency
    * blockchain
    * date

#### Example
```
chessplus.raw.responseTime.ms 6019 1585937897 source=spider type=fix-trade-async status=OK user=4 concurrency=8 blockchain=0.6.13 date=2020-04-03T18:16:07Z
```

### Aggregate Stats

js/stats.json is parsed to get the following metrics. 

* Metrics: refer [metrics.json](metrics.json)
* Tags
    * source
    * type
    * status
    * concurrency
    * blockchain
    * date

#### Example
```
chessplus.stats.mean.NumberOfRequestsPerSecond 2.55 1585941693 source=spider type=fix-trade-async status=ok concurrency=16	blockchain=1123	date=2020-04-03T18:21:33Z
```

## Usage

```bash
./publish.sh 
This script sends Gatling report to Wavefront.
options:
-g     Gatling report directory
-w     Wavefront proxy IP
-s     Wavefront tag for source
-b     Wavefront tag for blockchain
```

#### Example

```
./publish.sh -w 10.40.205.201 -s spider -b 0.6.13 -g ~/chess/load-runner-20200403-181559/standardfixtradesimulation-20200403181607152/
```
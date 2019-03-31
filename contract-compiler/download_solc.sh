#!/bin/bash
set -e

mkdir -p public/solc-bin/bin
cd public/solc-bin/bin
versions=(v0.5.4+commit.9549d8ff v0.5.3+commit.10d17f24
v0.5.2+commit.1df8f40c v0.5.1+commit.c8a2cb62 v0.5.0+commit.1d4f565a v0.4.25+commit.59dbf8f1
v0.4.24+commit.e67f0147 v0.4.23+commit.124ca40d v0.4.22+commit.4cb486ee v0.4.21+commit.dfe3193c
v0.4.20+commit.3155dd80 v0.4.19+commit.c4cbbb05 v0.4.18+commit.9cf6e910 v0.4.17+commit.bdeb9e52
v0.4.16+commit.d7661dd9 v0.4.15+commit.bbb8e64f v0.4.14+commit.c2215d46 v0.4.13+commit.fb4cb1a
v0.4.12+commit.194ff033 v0.4.11+commit.68ef5810 v0.4.10+commit.f0d539ae v0.4.9+commit.364da425
v0.4.8+commit.60cc1668 v0.4.7+commit.822622cf v0.4.6+commit.2dabbdf0 v0.4.5+commit.b318366e
v0.4.4+commit.4633f3de v0.4.3+commit.2353da71 v0.4.2+commit.af6afb04 v0.4.1+commit.4fc6fc2c
v0.4.0+commit.acd334c9)
for version in ${versions[@]}; do
  url="https://ethereum.github.io/solc-bin/bin/soljson-${version}.js"
  echo "Downloading $url"
  curl $url -o soljson-${version}.js
done

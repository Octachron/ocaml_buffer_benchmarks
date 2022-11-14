#!/usr/bin/env sh

addition=$1
reset=$2
samples=$3


run_once () {
local mode=$1
dune exec ./run/run.exe -- -reset=$reset -addition=$addition -samples=$samples -mode=$mode \
    > data/mode=${mode}_addition=${addition}_reset=${reset}_samples=${samples}.data
gnuplot -e "addition=${addition}" -e "reset=${reset}" -e "nsample=${samples}" \
    -e "mode=\"${mode}\"" plot/quantiles.plot
}

run_once "char"
run_once "string"

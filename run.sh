#!/usr/bin/env sh

addition=$1
reset=$2
size=$3
samples=$4


run_once () {
    local mode=$1
    local params="addition=${addition}_reset=${reset}"
dune exec ./run/run.exe -- -reset=$reset -addition=$addition -samples=$samples -mode=$mode \
    -o "data/mode=${mode}_${params}_samples=${samples}.data"
gnuplot -e "params=\"${params}\"" -e "nsample=${samples}" \
    -e "mode=\"${mode}\"" plot/quantiles.plot
}

run_size () {
    local mode=$1
    local params="size=${size}"
dune exec ./run/run.exe -- -size=$size -samples=$samples -mode=$mode \
    -o "data/mode=${mode}_${params}_samples=${samples}.data" \
    > /dev/null
gnuplot -e "params=\"${params}\"" -e "nsample=${samples}" \
    -e "mode=\"${mode}\"" plot/quantiles.plot
}


run_once "char"
run_once "string"
run_size "fasta3"
run_size "tree"

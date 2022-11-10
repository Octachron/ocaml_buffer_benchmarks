#!/usr/bin/env sh

niter=$1
nsample=$2
mode=$3

dune exec ./run/run.exe -- -niter=$niter -nsample=$nsample -mode=$mode > data/${mode}_${niter}_${nsample}.data
gnuplot -e "niter=$1" -e "nsample=$2" -e "mode=\"$3\"" plot/quantiles.plot

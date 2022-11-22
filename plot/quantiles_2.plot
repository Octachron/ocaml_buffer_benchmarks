set title "Quantile-aligned time comparison"
set term pdf

set key outside below center

set ylabel "times"
set xlabel "rank"

q="data/round=2_mode=".mode."_".params."_samples=".nsample.".data"


set output "graphics/round=2_mode=".mode."_".params."_sample=".nsample.".pdf"

set key autotitle columnheader

plot for [i=1:*] q using 0:i w l

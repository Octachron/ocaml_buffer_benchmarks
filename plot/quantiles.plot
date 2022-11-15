set title "Quantile-aligned time comparison"
set term pdf

set key outside;
set key right top;

set ylabel "times"
set xlabel "rank"

q="data/mode=".mode."_".params."_samples=".nsample.".data"


set output "graphics/mode=".mode."_".params."_sample=".nsample.".pdf"
plot q using 0:1 w l t "std", \
q using 0:2 w l t "std nospill", \
q using 0:3 w l t "safe", \
q using 0:4 w l t "safe nospill", \
q using 0:5 w l t "data safe", \
q using 0:6 w l t "simplified"

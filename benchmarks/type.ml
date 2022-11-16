type parameters = {
  reset:int;
  addition:int;
  size:int;
}

type benchmark =
    Benchmark: { init: parameters -> 'a; run: 'a -> unit} -> benchmark

type t = {
  name:string;
  string: benchmark;
  char: benchmark;
  fasta3: benchmark;
  tree: benchmark;
  markov: benchmark;
}

open Runner
module Arg = struct

let argv ~out ~size ~reset ~addition ~samples mode  = [
  "-o", Arg.String (fun s -> out := Some s), "file output";
  "-reset", Arg.Int ((:=) reset), "number of resets in a buffer test";
  "-addition", Arg.Int ((:=) addition), "number of additions in a buffer test";
  "-size", Arg.Int ((:=) size), "size of test for fasta3 test";
  "-samples", Arg.Int ((:=) samples), "number of sample";
  "-mode", Arg.String ((:=) mode), "string orchar test"
   ]

let parse () =
  let reset = ref 1_000 and addition = ref 1_000 and samples = ref 1_00
  and size = ref 1_000_000
  and mode = ref "char" and out = ref None in
  Arg.parse (argv ~reset ~size ~addition ~out ~samples mode) ignore "run -nreset=<%d> -nsample=<%d> -mode=<char|string>";
  let reset = !reset and samples = !samples and addition = !addition
  and size = !size
  and out = !out
  and mode = match !mode with
    | "string" -> String
    | "char" -> Char
    | "fasta3" -> Fasta3
    | "tree" -> Tree
    | "markov" -> Markov
    | _ -> failwith "bad mode"
  in
  let parameters = { Ty.reset; addition; size} in
 {parameters; samples; out; mode}
end

let () =
  let args = Arg.parse () in
  exec args

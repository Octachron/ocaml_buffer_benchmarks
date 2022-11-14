open Benchmarks.Type
module Variants = Benchmarks.All

module M = Map.Make(String)

let impls = Variants.[|
    std;
    std_nospill;
    safe;
    safe_nospill;
    data;
    simplified;
|]


let sample_once ~reset ~addition impl  =
  let time_start = Sys.time () in
  let () = ignore (impl  ~reset ~addition) in
  let time = Sys.time () in
  time -. time_start

let sample ~samples ~reset ~addition impl =
  let a = Array.init samples (fun _ -> sample_once ~reset ~addition impl) in
  Array.sort Stdlib.compare a;
  a

type mode = String | Char
let (.%()) impl mode = match mode with
  | Char -> impl.char
  | String -> impl.string

module Arg = struct
type args = { reset:int; samples:int; addition:int; mode:mode }

let argv ~reset ~addition ~samples mode  = [
  "-reset", Arg.Int ((:=) reset), "number of resets in a buffer test";
  "-addition", Arg.Int ((:=) addition), "number of additions in a buffer test";
  "-samples", Arg.Int ((:=) samples), "number of sample";
  "-mode", Arg.String ((:=) mode), "string orchar test"
   ]

let parse () =
  let reset = ref 1_000 and addition = ref 1_000 and samples = ref 1_00
  and mode = ref "char" in
  Arg.parse (argv ~reset ~addition ~samples mode) ignore "run -nreset=<%d> -nsample=<%d> -mode=<char|string>";
  let reset = !reset and samples = !samples and addition = !addition
  and mode = match !mode with
    | "string" -> String
    | "char" -> Char
    | _ -> failwith "bad mode"
  in
 {reset; addition; samples; mode}
end

let () =
  let {Arg.reset; addition ; samples;mode} = Arg.parse () in
  let sample_arrays = Array.map (fun impl -> sample ~samples ~reset ~addition impl.%(mode)) impls in
  Format.printf "@[<v>@[<h>#";
  Array.iter (fun i -> Format.printf "%s@ " i.name) impls;
  Format.printf "@]@,";
  for i = 0 to samples - 1 do
    Array.iter (fun array ->
        Format.printf "%f " array.(i)
      )
      sample_arrays;
      Format.printf "@,"
  done;
  Format.printf "@]@."

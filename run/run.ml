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


let sample_once parameters impl  =
  let counter = Mtime_clock.counter () in
  let () = ignore (impl parameters) in
  Mtime.Span.to_s @@ Mtime_clock.count counter

let sample ~samples parameters impl =
  let a = Array.init samples (fun _ -> sample_once parameters impl) in
  Array.sort Stdlib.compare a;
  a

type mode =
  | String
  | Char
  | Fasta3
let (.%()) impl mode = match mode with
  | Char -> impl.char
  | String -> impl.string
  | Fasta3 -> impl.fasta3

module Ty = Benchmarks.Type
module Arg = struct


type args = { parameters: Ty.parameters; samples:int; out: string option; mode:mode }

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
    | _ -> failwith "bad mode"
  in
  let parameters = { Ty.reset; addition; size} in
 {parameters; samples; out; mode}
end

let () =
  let {Arg.parameters ;out;samples;mode} = Arg.parse () in
  let ppf = match out with
    | None -> Format.std_formatter
    | Some x ->
      let chan = Out_channel.open_text x in
      Format.formatter_of_out_channel chan
  in
  let sample_arrays = Array.map (fun impl -> sample ~samples parameters impl.%(mode)) impls in
  Format.fprintf ppf "@[<v>@[<h>#";
  Array.iter (fun i -> Format.fprintf ppf "%s@ " i.name) impls;
  Format.fprintf ppf "@]@,";
  for i = 0 to samples - 1 do
    Array.iter (fun array ->
        Format.fprintf ppf "%f " array.(i)
      )
      sample_arrays;
      Format.fprintf ppf "@,"
  done;
  Format.fprintf ppf "@]@."

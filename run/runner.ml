
module Ty = Benchmarks.Type
module Variants = Benchmarks.All

module M = Map.Make(String)

let impls_first_round = Variants.[|
    std;
    std_nospill;
    safe;
    safe_nospill;
    indirection;
    simplified;
|]

let impls_second_round = Variants.[|
    std;
    indirection;
    indirection_rec;
    indirection_resize_once;
    indirection_resize_once_spill;
|]


let sample_once parameters impl  =
  let counter = Mtime_clock.counter () in
  let () = ignore (impl parameters) in
  Mtime.Span.to_s @@ Mtime_clock.count counter

let sample ~samples parameters (Ty.Benchmark {run;init}) =
  let parameters = init parameters in
  let a = Array.init samples (fun _ -> sample_once parameters run) in
  Array.sort Stdlib.compare a;
  a

type mode =
  | String
  | Char
  | Fasta3
  | Tree
  | Markov


let mode_name = function
  | String -> "string"
  | Char -> "char"
  | Fasta3 -> "fasta3"
  | Tree -> "tree"
  | Markov -> "markov"


let (.%()) (impl:Ty.t) mode = match mode with
  | Char -> impl.char
  | String -> impl.string
  | Fasta3 -> impl.fasta3
  | Tree -> impl.tree
  | Markov -> impl.markov

type round =
  | One
  | Two

let round_ordinal = function
  | One -> 1
  | Two -> 2

let parse_round = function
  | "one" -> One
  | "two" -> Two
  | s -> failwith (Format.asprintf "Unknown benchmark round:%s" s)

let impls = function
  | One -> impls_first_round
  | Two -> impls_second_round

type args = {
  parameters: Ty.parameters;
  samples:int;
  out: string option;
  mode:mode;
  round:round
}

let exec {parameters;out;samples;mode;round} =
  let ppf = match out with
    | None -> Format.std_formatter
    | Some x ->
      let chan = Out_channel.open_text x in
      Format.formatter_of_out_channel chan
  in
  let impls = impls round in
  let sample_arrays = Array.map (fun impl -> sample ~samples parameters impl.%(mode)) impls in
  Format.fprintf ppf "@[<v>@[<h>";
  Array.iter (fun i -> Format.fprintf ppf "%s@ " i.Ty.name) impls;
  Format.fprintf ppf "@]@,";
  for i = 0 to samples - 1 do
    Array.iter (fun array ->
        Format.fprintf ppf "%f " array.(i)
      )
      sample_arrays;
      Format.fprintf ppf "@,"
  done;
  Format.fprintf ppf "@]@."

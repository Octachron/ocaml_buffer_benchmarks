open Benchmarks.Type
module Variants = Benchmarks.All

module M = Map.Make(String)

let impls = Variants.[|
    std;
    std_nospill;
    safe;
    safe_nospill;
    data
|]


let sample niter impl  =
  let time_start = Sys.time () in
  let () = ignore (impl niter) in
  let time = Sys.time () in
  time -. time_start

let samples nsample niter impl =
  let a = Array.init nsample (fun _ -> sample niter impl) in
  Array.sort Stdlib.compare a;
  a

type mode = String | Char
let (.%()) impl mode = match mode with
  | Char -> impl.char
  | String -> impl.string

module Arg = struct
type args = { niter:int; nsample:int; mode:mode }

let argv niter nsample mode  = [
  "-niter", Arg.Int ((:=) niter), "number of iteration in a buffer test";
  "-nsample", Arg.Int ((:=) nsample), "number of sample";
  "-mode", Arg.String ((:=) mode), "string orchar test"
   ]

let parse () =
  let niter = ref 1_000 and nsample = ref 1_00 and mode = ref "char" in
  Arg.parse (argv niter nsample mode) ignore "run -niter=<%d> -nsample=<%d> -mode=<char|string>";
  let niter = !niter and nsample = !nsample
  and mode = match !mode with
    | "string" -> String
    | "char" -> Char
    | _ -> failwith "bad mode"
  in
 {niter; nsample; mode}
end

let () =
  let {Arg.niter;nsample;mode} = Arg.parse () in
  let sample_arrays = Array.map (fun impl -> samples nsample niter impl.%(mode)) impls in
  Format.printf "@[<v>@[<h>#";
  Array.iter (fun i -> Format.printf "%s@ " i.name) impls;
  Format.printf "@]@,";
  for i = 0 to nsample - 1 do
    Array.iter (fun array ->
        Format.printf "%f " array.(i)
      )
      sample_arrays;
      Format.printf "@,"
  done;
  Format.printf "@]@."

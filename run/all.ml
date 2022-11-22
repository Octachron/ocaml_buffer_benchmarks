open Runner

let (let*) x f = List.concat_map f x
let dir = "data"
let round = Two

let param_map params ppf =
  let pp ppf (key,v) = Format.fprintf ppf {|%s=%d|} key v in
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "_")
    pp
    ppf params

let plot_script = function
  | One -> "plot/quantiles.plot"
  | Two -> "plot/quantiles_2.plot"

let plot samples mode params () =
  let command = Format.asprintf
      {|gnuplot -e "mode=\"%s\"" -e "nsample=%d" -e "params=\"%t\"" %s|}
      (mode_name mode) samples
      params (plot_script round)
  in
  Sys.command command



module Basic = struct
  let modes = [String; Char]
  let small, medium, huge =
    let mk x y = { Ty.size=0; reset=x; addition=y} in
    [ mk 10 1000; mk 10 10_000; mk 10 100_000;
      mk 100 100; mk 100 1_000; mk 100 10_000;
      mk 1000 10; mk 1000  100; mk 1000 1_000;
    ],
    [ mk 10 1000; mk 10 10_000; mk 10 100_000; mk 10 1_000_000;
      mk 100 100; mk 100 1_000; mk 100 10_000; mk 100 100_000;
      mk 1000 10; mk 1000  100; mk 1000 1_000; mk 1000 10_000;
      mk 10000 10; mk 10000 100; mk 10000 1_000;
    ],
    [ mk 10 1000;  mk 10 10_000;  mk 10 100_000;  mk 10 1_000_000; mk 10 10_000_000;
      mk 100 100;  mk 100 1_000;  mk 100 10_000;  mk 100 100_000;  mk 100 1_000_000;
      mk 1000 10;  mk 1000  100;  mk 1000 1_000;  mk 1000 10_000;  mk 1000  100_000;
      mk 10000 10; mk 10000 100; mk 10000 1000;   mk 10000 10_000; mk 10000 100_000;
    ]




  let tests (set: Ty.parameters list) samples =
    let* mode = modes in
    let* parameters = set in
    let params = param_map [
        "addition", parameters.addition;
        "reset", parameters.reset;
      ]
    in
    let out =
      Format.asprintf "%s/round=%d_mode=%s_%t_samples=%d.data"
        dir
        (round_ordinal round)
        (mode_name mode)
        params
        samples
    in
    [plot samples mode params, { round; samples; out = Some out; parameters; mode }]
end

module Composite = struct
  let modes = [Fasta3; Tree; Markov]

  let small, medium, huge =
    let mk x = { Ty.reset=0; addition=0; size = x} in
    List.map mk [1_000; 10_000; 1_00_000],
    List.map mk [1_000; 10_000; 1_00_000; 1_000_000],
    List.map mk [1_000; 10_000; 1_00_000; 1_000_000; 10_000_000]

  let tests (set: Ty.parameters list) samples =
    let* mode = modes in
    let* parameters = set in
    let params = param_map ["size", parameters.size] in
    let out =
      Format.asprintf "%s/round=%d_mode=%s_%t_samples=%d.data"
        dir
        (round_ordinal round)
        (mode_name mode)
        params
        samples
    in
    [plot samples mode params, { samples; round; out=Some out; parameters; mode }]
end

let with_plot = ref false
type size = Small | Medium | Large | Day
let size = ref Medium
let parse_size = function
  | "small" -> size := Small;
  | "medium" -> size := Medium
  | "large" -> size := Large
  | "day" -> size := Day
  | s ->
    Format.eprintf "Unknown size: %s. Expected small, medium, large, or day." s;
    exit 2
let args =
  [ "-size", Arg.String parse_size,
    "size of the test: small ~10s, medium: ~10mn, large: ~2h, day: ~24h";
    "-plot", Arg.Set with_plot, "plot the result with gnuplot"
  ]

let () =
  let () = Arg.parse args ignore "all [-plot] [-size=<small|medium|large|day>" in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o775;
  let with_plot = !with_plot in
  let size = !size in
  let tests = match size with
    | Small ->
      let sample = 10 in
      Basic.(tests small) sample @ Composite.(tests small) sample
    | Medium ->
      let sample = 100 in
       Basic.(tests medium) sample @ Composite.(tests medium) sample
    | Large ->
      let sample = 1000 in
      Basic.(tests medium) sample @ Composite.(tests medium) sample
    | Day ->
      let sample = 1000 in
      Basic.(tests huge) sample @ Composite.(tests huge) sample

  in
  let exec (plot, x) =
    exec x;
    if with_plot then ignore (plot ())
  in
  List.iter exec tests

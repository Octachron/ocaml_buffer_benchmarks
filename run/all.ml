open Runner

let (let*) x f = List.concat_map f x
let modes = [String; Char]
let size_modes = [Fasta3; Tree; Markov]
let original =
  let mk x y = { Ty.size=0; reset=x; addition=y} in
  [ mk 10 1000; mk 10 10_000; mk 10 100_000;
    mk 100 100; mk 100 1_000; mk 100 10_000;
    mk 1000 10; mk 1000 1000; mk 1000 1_000;
  ]

let sizes =
  let mk x = { Ty.reset=0; addition=0; size = x} in
  List.map mk [1_000; 10_000; 1_00_000]

let dir = "data"
let samples = 10

let basic_tests =
  let* mode = modes in
  let* parameters = original in
  let out =
    Some (Format.asprintf "%s/mode=%s_reset=%d_addition=%d_samples=%d.data"
      dir
      (mode_name mode)
      parameters.reset parameters.addition
      samples
         )
  in
  [{ samples; out; parameters; mode }]

let composite_tests =
  let* mode = size_modes in
  let* parameters = sizes in
  let out =
    Some (Format.asprintf "%s/mode=%s_size=%d_samples=%d.data"
            dir
            (mode_name mode)
            parameters.size
            samples
         )
  in
  [{ samples; out; parameters; mode }]

let () =
  List.iter exec basic_tests;
  List.iter exec composite_tests

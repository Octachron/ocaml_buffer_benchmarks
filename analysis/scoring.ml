
let tokenize x =
  x |> String.split_on_char ' ' |> List.filter (fun x -> String.trim x <> "")

let float_of_string x =
  match float_of_string x with
  | x -> x
  | exception Failure _ ->
    Format.eprintf "Conversion failure: %S@." x; exit 2

let parse_line s =
  Array.map float_of_string @@ Array.of_list @@ tokenize s

let parse_header s =
  match tokenize s with
  | "#" :: q -> Array.of_list q
  | a :: q when a.[0]='#' ->
    Array.of_list @@ String.sub a 1 (String.length a - 1) :: q
  | [] -> assert false
  | _ -> assert false

let read_file file =
  let chan = In_channel.open_text file in
  let line () =
    let line = In_channel.input_line chan in
    Option.map parse_line line
  in
  let rec read_all () =
    match line () with
    | Some x -> x :: read_all ()
    | None -> []
  in
  let header = match In_channel.input_line chan with
    | None -> assert false
    | Some x -> parse_header x
  in
  let a = Array.of_list @@ read_all () in
  In_channel.close chan;
  header, a

let quantile p a =
  let pos = int_of_float @@ p *. float (Array.length a - 1) in
  a.(pos)

let add_to_score scores indices line =
  let result = Array.mapi (fun i x -> i, line.(x) ) indices in
  let () = Array.sort (fun (_,(x:float)) (_,y) -> compare x y ) result in
  let with_rank = Array.mapi (fun rank (index,_) -> (index,rank) ) result in
  let add (i,rank) =
    let scorei = scores.(i) in
    scorei.(rank) <- scorei.(rank) + 1
  in
  Array.iter add with_rank

let count ~quantiles ~scores ~participants file =
  Array.iter (fun q -> add_to_score scores participants (quantile q file))
    quantiles;
  add_to_score scores participants file.(0)




let quantiles = [| 0.1; 0.25; 0.5; 0.9 |]


let std = 0
let _std_nospill = 1
let safe = 2
let indirect = 4
let simplified = 5

let participants = [|indirect;simplified;std;safe|]

let pp_index ppf header scores i pos =
  let name = header.(pos) in
  let score = scores.(i) in
  Format.fprintf ppf "| %s   | %d      |     %d   | %d      |@,"
    name score.(0) score.(1) score.(2)


let pp header participants ppf scores =
  Format.fprintf ppf "@[<v>|            |1st rank | 2nd rank | 3rd rank|@,";
  Array.iteri (pp_index ppf header scores) participants;
  Format.fprintf ppf "@,total tests: %d@]@."
    (Array.fold_left (+) 0 scores.(0))


let () =
  let np = Array.length participants in
  let scores = Array.init np (fun _ -> Array.make np 0) in
  let header = ref None in
  let analyze_file file =
    let new_header, contents = read_file file in
    begin match !header with
    | None -> header := Some new_header
    | Some h -> assert (h = new_header)
    end;
    count ~scores ~participants ~quantiles contents
  in
  let () = Arg.parse [] analyze_file "scoring <file list>" in
  match !header with
  | None -> assert false
  | Some header ->
    Format.printf "%a" (pp header participants) scores

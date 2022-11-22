[@@@warning "-32"]

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
  | _ :: _ as x -> Array.of_list x


module String_map = Map.Make(String)

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
    | None -> Format.eprintf "Empty file %s?@." file; exit 2
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




let _quantiles = [| 0.1; 0.25; 0.5; 0.9 |]

let pp_index ppf scores i name =
  let score = scores.(i) in
  let len = Array.length score in
  let total = Array.fold_left (+) 0 score in
  let per x = (100. *. float x) /. float total in
  let percent i =
    if i >= len then None
    else Some (per score.(i))
  in
  let opt ppf = function
    | None -> Format.fprintf ppf "N/A"
    | Some x -> Format.fprintf ppf "%3.1f%%" x
  in
  let other =
    if len >= 3 then
      Some (per @@ total - score.(0) - score.(1) - score.(2))
    else None in
  let padding ppf n =
    for _i=1 to n do
      Format.fprintf ppf " ";
    done
  in
  Format.fprintf ppf "| %s%a| %a    | %a    | %a    | %a |@,"
    name padding (30 - String.length name)
    opt (percent 0)
    opt (percent 1)
    opt (percent 2)
    opt other


let pp participants ppf scores =
  Format.fprintf ppf "@[<v>|                               | 1st rank | 2nd rank | 3rd rank | other |@,";
  Format.fprintf ppf      "|-------------------------------|----------|----------|----------|-------|@,";

  Array.iteri (pp_index ppf scores) participants;
  Format.fprintf ppf "@,total tests: %d@]@."
    (Array.fold_left (+) 0 scores.(0))

type quantile_kind =
  | Low
  | Median
  | High
  | All

let parse_qkind = function
  | "low" -> Low
  | "median" -> Median
  | "high" -> High
  | "all" -> All
  | _ -> failwith "Unknown option"

let reverse_header header =
  let iheader = Array.mapi (fun i x -> x, i) header in
  String_map.of_seq (Array.to_seq iheader)

let num_participants rheader participants =
  Array.map (fun name -> String_map.find name rheader) participants

let parse_rows s =
  String.split_on_char ',' s |> Array.of_list

let args ~quantile_kind ~rows =
  ["-quantile", Arg.String (fun s -> quantile_kind := parse_qkind s), "which quantiles?";
   "-rows", Arg.String (fun s -> rows := parse_rows s ), "which tests?"
  ]


let quantiles_choice quantile_kind =
  let qlow = [| 0.1 |] and qhigh = [| 0.9|] and median = [|0.5 |]
  and all = [| 0.1; 0.25; 0.5; 0.75; 0.9 |] in
  match quantile_kind with
  | All -> all
  | Low -> qlow
  | High -> qhigh
  | Median -> median


let analyze_file ~header ~participants ~scores ~quantiles file =
  let new_header, contents = read_file file in
  let rev_header = match !header with
    | None ->
      let r = reverse_header new_header in
      header := Some (r, new_header);
      r
    | Some (r,h) -> assert (h = new_header); r
  in
  let participants = num_participants rev_header participants in
  count ~scores ~participants ~quantiles contents


let () =
  let quantile_kind = ref Median
  and rows = ref [|
      "indirection";
      "indirection+rec";
      "indirection+resize-once";
      "indirection+resize-once+spill";
      "std"
    |]
  in
  let files = ref [] in
  let record_file s = files := s :: !files in
  let () = Arg.parse (args ~quantile_kind ~rows)
      record_file
      "scoring <file list>"
  in
  let participants = !rows
  and files = List.rev !files in
  let np = Array.length participants in
  let scores = Array.init np (fun _ -> Array.make np 0) in
  let header = ref None in
  let quantiles = quantiles_choice !quantile_kind in
  let () = List.iter (analyze_file ~header ~participants ~scores ~quantiles) files in
match !header with
| None -> assert false
| Some (_rev_header, _header) ->
  Format.printf "%a" (pp participants) scores

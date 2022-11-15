module Inner = struct

type 'a t = { value:'a; subtrees:'a t list }

let repeat_string buffer n s =
  for _ = 1 to n do
    Buffer.add_string buffer s
  done

let repeat_char buffer n c =
  for _ = 1 to n do
    Buffer.add_char buffer c
  done

type indent =
  | Active of int | Inactive of int

type line_kind =
  | Last
  | Empty
  | Mid

let rec add_indent b pos = function
  | [] -> ()
  | [Active n | Inactive n] ->
    let pos =
      match pos with
      | Empty -> "┃"
      | Last -> "┗"
      | Mid -> "┣"
    in
    repeat_string b n " ";
    Buffer.add_string b pos
  | Active n :: q ->
    repeat_char b n ' ';
    Buffer.add_string b "┃";
    add_indent b pos q
  | Inactive n :: q ->
    repeat_char b n ' ';
    Buffer.add_string b " ";
    add_indent b pos q


let rec inactive = function
  | []  as q -> q
  | [Active a] -> [Inactive a]
  | a :: q -> a :: inactive q

let rec add_node b indentation pos x =
  add_indent b pos indentation;
  Buffer.add_string b "━━( ";
  Buffer.add_string b x.value;
  Buffer.add_string b " )";
  Buffer.add_string b "\n";
  let len = 3 + String.length x.value / 2 in
  let indentation = indentation @ [Active len] in
  begin match x.subtrees with
    | [] -> ()
    | _ :: _ ->
      add_indent b Empty indentation;
      Buffer.add_string b "\n";
      add_indent b Empty indentation;
      Buffer.add_string b "\n";
  end;
  add_subtrees b indentation x.subtrees
and add_subtrees b indentation = function
  | [] -> ()
  | [a] ->
    add_node b (inactive indentation) Last a
  | a :: q ->
    add_node b indentation Mid a;
    add_subtrees b indentation q

let add_tree b x = add_node b [] Mid x

let string n =
  let char () = Char.chr @@ 30 + Random.int 97 in
  String.init (1 + Random.int n) (fun _ -> char ())

let rec tree value_width nsubtrees d =
  let value = string value_width in
  let subtrees =
    if d = 0 then []
    else  List.init nsubtrees (fun _ -> tree value_width nsubtrees (d-1))
  in
  { value; subtrees }


let run ~value_width ~nsubtree ~depth =
  let t = tree value_width nsubtree depth in
  let b = Buffer.create 1000 in
  let () = add_tree b t in
  print_string @@ Buffer.contents b
end

let tree {Type.size; _ } =
  let value_width = 20 in
  let nsubtree = 10 in
  let depth = int_of_float ( log (float size) /. log (float nsubtree)) in
  Inner.run ~value_width ~nsubtree ~depth

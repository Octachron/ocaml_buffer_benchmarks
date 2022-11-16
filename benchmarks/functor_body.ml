let char {Type.reset; addition; _} =
  let buf = Buffer.create 1 in
  for _ = 1 to reset do
    Buffer.reset buf;
    for _ = 1 to addition do
      Buffer.add_char buf 'a';
    done;
  done

let string {Type.reset; addition; _ } =
  let buf = Buffer.create 1 in
  for _ = 1 to reset do
    Buffer.reset buf;
    for i = 1 to addition do
      Buffer.add_string buf (Strings.get i);
    done;
  done

#include "fasta3.ml"
#include "tree_printer.ml"


let benchmark = {Type.name;char;string;fasta3;tree}

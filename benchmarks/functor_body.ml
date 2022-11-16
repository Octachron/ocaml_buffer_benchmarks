
module Char_b = struct
  let run {Type.reset; addition; _} =
    let buf = Buffer.create 1 in
    for _ = 1 to reset do
      Buffer.reset buf;
      for _ = 1 to addition do
        Buffer.add_char buf 'a';
      done;
    done
end

let char = Type.Benchmark { init = Fun.id; run = Char_b.run }

module String_b = struct
  let run {Type.reset; addition; _ } =
    let buf = Buffer.create 1 in
    for _ = 1 to reset do
      Buffer.reset buf;
      for i = 1 to addition do
        Buffer.add_string buf (Strings.get i);
      done;
    done
end
let string = Type.Benchmark { init = Fun.id; run = String_b.run }

module Fasta3 = struct
#include "fasta3.ml"
end
let fasta3 = Fasta3.benchmark

module Markov = struct
#include "markov_text.ml"
end
let markov = Markov.benchmark



#include "tree_printer.ml"

let benchmark = {Type.name;char;string;fasta3;tree;markov}

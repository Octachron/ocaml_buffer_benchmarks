(* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Troestler Christophe
 * modified by Mauricio Fernandez
 * changed use of Strings to Bytes where mutation is needed, by Tony Tavener
 *)

(* Random number generator *)

let im = 139968
and ia = 3877
and ic = 29573

let last = ref 42 and im_f = float im
let gen_random  max =
  let n = (!last * ia + ic) mod im in
    last := n;
    max *. float n /. im_f

module Cumul_tbl =
struct
  type t = { probs : float array; chars : char array }

  let make a = let p = ref 0.0 in
    {
      probs = Array.map (fun (_, p1) -> p := !p +. p1; !p) a;
      chars = Array.map fst a;
    }

  let rand_char t =
    let p = gen_random 1.0 in
    let i = ref 0 and ps = t.probs in
      while p >= ps.(!i) do incr i done;
      t.chars.(!i)
end

let width = 60

let make_random_fasta id desc table n b =
  Buffer.add_char b '>';
  Buffer.add_string b id;
  Buffer.add_char b ' ';
  Buffer.add_string b desc;
  Buffer.add_string b "\n";
  let table = Cumul_tbl.make table in
  for _i = 1 to n / width do
    for _j = 0 to width - 1 do
      Buffer.add_char b (Cumul_tbl.rand_char table)
    done;
    Buffer.add_string b "\n"
  done;
  let w = n mod width in
  if w > 0 then (
    for _j = 1 to w do Buffer.add_char b (Cumul_tbl.rand_char table); done;
    Buffer.add_string b "\n"
  )

(* [write s i0 l w] outputs [w] chars of [s.[0 .. l]], followed by a
   newline, starting with [s.[i0]] and considering the substring [s.[0
   .. l]] as a "circle".
   One assumes [0 <= i0 <= l <= String.length s].
   @return [i0] needed for subsequent writes.  *)
let rec write s i0 l w b =
  let len = l - i0 in
  if w <= len then (Buffer.add_substring b s i0 w; Buffer.add_char b '\n'; i0 + w)
  else (Buffer.add_substring b s i0 len; write s 0 l (w - len) b)

let make_repeat_fasta id desc src n b =
  Buffer.add_char b '>';
  Buffer.add_string b id;
  Buffer.add_char b ' ';
  Buffer.add_string b desc;
  Buffer.add_string b "\n";
  let l = String.length src
  and i0 = ref 0 in
  for _i = 1 to n / width do
    i0 := write src !i0 l width b;
  done;
  let w = n mod width in
  if w > 0 then ignore(write src !i0 l w b)


let alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

let iub = [| ('a', 0.27);  ('c', 0.12);  ('g', 0.12);  ('t', 0.27);
	     ('B', 0.02);  ('D', 0.02);  ('H', 0.02);  ('K', 0.02);
	     ('M', 0.02);  ('N', 0.02);  ('R', 0.02);  ('S', 0.02);
	     ('V', 0.02);  ('W', 0.02);  ('Y', 0.02);  |]

let homosapiens = [| ('a', 0.3029549426680);    ('c', 0.1979883004921);
		     ('g', 0.1975473066391);    ('t', 0.3015094502008);  |]

let with_buffer f =
  let b = Buffer.create 1000 in
  f b;
  output_string stdout (Buffer.contents b);
  flush stdout

let run {Type.size=n; _ } =
  with_buffer @@ make_repeat_fasta "ONE" "Homo sapiens alu" alu (n*2);
  with_buffer @@ make_random_fasta "TWO" "IUB ambiguity codes" iub (n*3);
  with_buffer @@ make_random_fasta "THREE" "Homo sapiens frequency" homosapiens (n*5)

let benchmark = Type.Benchmark { init=Fun.id; run }

type parameters = { reset:int; addition:int; size:int }

type t = {
  name:string;
  string: parameters -> unit;
  char: parameters -> unit;
  fasta3: parameters -> unit;
}

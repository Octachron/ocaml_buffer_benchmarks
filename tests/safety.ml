module type buffer = sig
  type t
  val reset: t -> unit
  val add_string: t -> string -> unit
  val create: int -> t
end

module Test(Buffer: buffer) = struct

let worker b n () =
  let s = String.make 500 'x' in
  while true do
    let () = Buffer.reset b in
    try
    Buffer.add_string b s
    with e -> Format.eprintf "domain %d:%s\n@." n (Printexc.to_string e)
  done

let _ =
  let buffer = Buffer.create 1024 in
  let _ = Domain.spawn (worker buffer 1)   in
  let _ = Domain.spawn (worker buffer 2)   in
  let _ = Domain.spawn (worker buffer 3)   in
  let _ = Domain.spawn (worker buffer 4)  in
  let _ = Domain.spawn (worker buffer 5)   in
  let _ = Domain.spawn (worker buffer 6)   in
  let _ = Domain.spawn (worker buffer 7)   in
  let _ = Domain.spawn (worker buffer 8)   in
  for _i = 0 to 1_000_000_000 do Domain.cpu_relax () done

end


module Safe = Test(Buffer.Safe)
module Safe_nospill = Test(Buffer.Safe_nospill)
module Data_Sage = Test(Buffer.Data)

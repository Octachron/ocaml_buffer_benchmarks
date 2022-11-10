module Buffer= Buffer.Simplified
let name = "simplified"


let char n =
  let buf = Buffer.create 1 in
  for _ = 1 to n do
    Buffer.reset buf;
    for _ = 1 to 1050 do
      Buffer.add_char buf 'a';
    done;
  done

let string n =
  let buf = Buffer.create 1 in
  for _ = 1 to n do
    Buffer.reset buf;
    for i = 1 to 1050 do
      Buffer.add_string buf (Strings.get i);
    done;
  done

let benchmark = {Type.name;char;string}

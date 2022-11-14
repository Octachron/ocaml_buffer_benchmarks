module Buffer= Buffer.Std
let name = "std"


let char ~reset ~addition =
  let buf = Buffer.create 1 in
  for _ = 1 to reset do
    Buffer.reset buf;
    for _ = 1 to addition do
      Buffer.add_char buf 'a';
    done;
  done

let string ~reset ~addition =
  let buf = Buffer.create 1 in
  for _ = 1 to reset do
    Buffer.reset buf;
    for i = 1 to addition do
      Buffer.add_string buf (Strings.get i);
    done;
  done

let benchmark = {Type.name;char;string}

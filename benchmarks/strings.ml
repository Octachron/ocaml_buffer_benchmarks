let n = 50
let size = 10
let data = Array.init n
    (fun _ -> String.init
       (Random.int size)
       (fun _ -> Char.chr @@ Random.int 127)
    )

let get i = data.(i mod n)

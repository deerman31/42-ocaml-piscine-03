let shuffle lst =
  let insert lst element pos =
    let rec loop lst n =
      match lst with
      | [] -> if n = pos then [ element ] else []
      | first :: rest ->
          if n = pos then element :: first :: loop rest (n + 1)
          else first :: loop rest (n + 1)
    in
    loop lst 0
  in
  Random.self_init ();
  let rec loop acc lst n =
    match lst with
    | [] -> acc
    | first :: rest ->
        let randum_num = Random.int n in
        loop (insert acc first randum_num) rest (n + 1)
  in
  loop [] lst 1

let rec range n1 n2 = if n1 > n2 then [] else n1 :: range (n1 + 1) n2
let lst = range 1 100
let print_lst lst = List.iter (fun x -> print_endline (string_of_int x)) lst;;

print_lst lst;
print_endline "-----------------------";

let random_lst = shuffle lst in
print_lst random_lst

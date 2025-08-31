module Color = struct
  (* Color *)
  type t = Spade | Heart | Diamond | Club

  let all = [ Spade; Heart; Diamond; Club ]

  let toString t =
    match t with Spade -> "S" | Heart -> "H" | Diamond -> "D" | Club -> "C"

  let toStringVerbose t =
    match t with
    | Spade -> "Spade"
    | Heart -> "Heart"
    | Diamond -> "Diamond"
    | Club -> "Club"
end

module Value = struct
  (* Value *)
  type t =
    | T2
    | T3
    | T4
    | T5
    | T6
    | T7
    | T8
    | T9
    | T10
    | Jack
    | Queen
    | King
    | As

  let all = [ T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As ]

  let toInt t =
    match t with
    | T2 -> 2
    | T3 -> 3
    | T4 -> 4
    | T5 -> 5
    | T6 -> 6
    | T7 -> 7
    | T8 -> 8
    | T9 -> 9
    | T10 -> 10
    | Jack -> 11
    | Queen -> 12
    | King -> 13
    | As -> 1

  let toString t =
    match t with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"

  let toStringVerbose t =
    match t with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "As"

  let next t =
    match t with
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> As
    | As -> invalid_arg "The next value of AS does not exist"

  let previous t =
    match t with
    | T2 -> invalid_arg "The previous value of T2 does not exist"
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | As -> King
end

(* module Card = struct *)
type t = { color : Color.t; value : Value.t }

let newCard v c = { color = c; value = v }
let allhelper color = List.map (fun x -> newCard x color) Value.all
let allSpades = allhelper Color.Spade
let allHearts = allhelper Color.Heart
let allDiamonds = allhelper Color.Diamond
let allClubs = allhelper Color.Club

let all =
  List.fold_left
    (fun acc color -> List.append acc (allhelper color))
    [] Color.all

let getValue t = match t with { color = _; value = v } -> v
let getColor t = match t with { color = c; value = _ } -> c
let toString t = Value.toString (getValue t) ^ Color.toString (getColor t)

let toStringVerbose t =
  "Card("
  ^ Value.toStringVerbose (getValue t)
  ^ ", "
  ^ Color.toStringVerbose (getColor t)
  ^ ")"

let compare t1 t2 =
  let rec loop acc v2 = if acc = v2 then -1 else loop (Value.next acc) v2 in
  let v1 = getValue t1 in
  let v2 = getValue t2 in
  try if v1 = v2 then 0 else loop v1 v2 with Invalid_argument _ -> 1

let max t1 t2 = if getValue t1 >= getValue t2 then t1 else t2
let min t1 t2 = if getValue t1 <= getValue t2 then t1 else t2

let best lst =
  match lst with
  | [] -> invalid_arg "The list is empty"
  | first :: rest -> List.fold_left max first rest

let isOf t ct = getColor t = ct
let isSpade t = isOf t Color.Spade
let isHeart t = isOf t Color.Heart
let isDiamond t = isOf t Color.Diamond
let isClub t = isOf t Color.Club
(* end *)

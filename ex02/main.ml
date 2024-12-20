open Card

let card_test () =
  print_endline "[CARD-TEST]";
  let new_card_test () =
    print_string "[newCard] -> ";
    let card = Card.newCard Value.As Color.Spade in
    let v = Card.getValue card in
    let c = Card.getColor card in
    if v = Value.As && c = Color.Spade then print_endline "OK"
    else print_endline "NG"
  in

  let all_card_test_helper lst f =
    if List.length lst != 13 then false
    else
      let rec loop lst vt =
        match lst with
        | [] -> true
        | first :: rest ->
            if f first && Card.getValue first = vt then
              if vt = Value.As then loop rest vt else loop rest (Value.next vt)
            else false
      in
      loop lst Value.T2
  in

  let allSpades_test () =
    print_string "[allSpades] -> ";
    if all_card_test_helper Card.allSpades Card.isSpade then print_endline "OK"
    else print_endline "NG"
  in

  let allHearts_test () =
    print_string "[allHearts] -> ";
    if all_card_test_helper Card.allHearts Card.isHeart then print_endline "OK"
    else print_endline "NG"
  in
  let allDiamonds_test () =
    print_string "[allDiamonds] -> ";
    if all_card_test_helper Card.allDiamonds Card.isDiamond then
      print_endline "OK"
    else print_endline "NG"
  in
  let allClubs_test () =
    print_string "[allClubs] -> ";
    if all_card_test_helper Card.allClubs Card.isClub then print_endline "OK"
    else print_endline "NG"
  in
  let all_test () =
    print_string "[all] -> ";
    let cards = Card.all  in
    let test1 = List.length cards = 4 * 13 in
    let test2 =
      all_card_test_helper (List.filter Card.isSpade cards) Card.isSpade
    in
    let test3 =
      all_card_test_helper (List.filter Card.isHeart cards) Card.isHeart
    in
    let test4 =
      all_card_test_helper (List.filter Card.isDiamond cards) Card.isDiamond
    in
    let test5 =
      all_card_test_helper (List.filter Card.isClub cards) Card.isClub
    in
    if test1 && test2 && test3 && test4 && test5 then print_endline "OK"
    else print_endline "NG"
  in
  let getValue_test () =
    print_string "[getValue] -> ";
    let card = Card.newCard Value.As Color.Diamond in
    print_endline (if Card.getValue card = Value.As then "OK" else "NG")
  in
  let getColor_test () =
    print_string "[getColor] -> ";
    let card = Card.newCard Value.As Color.Diamond in
    print_endline (if Card.getColor card = Color.Diamond then "OK" else "NG")
  in
  let toString_test () =
    print_string "[toString] -> ";
    let card = Card.newCard Value.T2 Color.Spade in
    print_endline (if Card.toString card = "2S" then "OK" else "NG")
  in
  let toStringVerbose_test () =
    print_string "[toStringVerbose] -> ";
    let card = Card.newCard Value.T7 Color.Diamond in
    print_endline
      (if Card.toStringVerbose card = "Card(7, Diamond)" then "OK" else "NG")
  in
  let compare_test () =
    print_string "[compare] -> ";
    let test1 =
      Card.compare
        (Card.newCard Value.T5 Color.Spade)
        (Card.newCard Value.T2 Color.Spade)
      = 1
    in
    let test2 =
      Card.compare
        (Card.newCard Value.T5 Color.Spade)
        (Card.newCard Value.As Color.Spade)
      = -1
    in
    let test3 =
      Card.compare
        (Card.newCard Value.T5 Color.Spade)
        (Card.newCard Value.T5 Color.Spade)
      = 0
    in
    print_endline (if test1 && test2 && test3 then "OK" else "NG")
  in
  let max_test () =
    print_string "[max] -> ";
    let t1 = Card.newCard Value.T7 Color.Spade in
    let t2 = Card.newCard Value.T2 Color.Spade in
    let t3 = Card.newCard Value.As Color.Spade in
    let t4 = Card.newCard Value.T7 Color.Diamond in
    let test1 = Card.max t1 t2 = t1 in
    let test2 = Card.max t1 t4 = t1 in
    let test3 = Card.max t1 t3 = t3 in
    print_endline (if test1 && test2 && test3 then "OK" else "NG")
  in
  let min_test () =
    print_string "[min] -> ";
    let t1 = Card.newCard Value.T7 Color.Spade in
    let t2 = Card.newCard Value.T2 Color.Spade in
    let t3 = Card.newCard Value.As Color.Spade in
    let t4 = Card.newCard Value.T7 Color.Diamond in
    let test1 = Card.min t1 t2 = t2 in
    let test2 = Card.min t1 t4 = t1 in
    let test3 = Card.min t1 t3 = t1 in
    print_endline (if test1 && test2 && test3 then "OK" else "NG")
  in
  let best_test () =
    print_string "[best] -> ";
    let test1 =
      Card.best (Card.allSpades ) = Card.newCard Value.As Color.Spade
    in
    let test2 =
      Card.best (Card.allHearts ) = Card.newCard Value.As Color.Heart
    in
    let test3 =
      Card.best (Card.allDiamonds ) = Card.newCard Value.As Color.Diamond
    in
    let test4 =
      Card.best (Card.allClubs ) = Card.newCard Value.As Color.Club in
    print_endline (if test1 && test2 && test3 && test4 then "OK" else "NG")
  in
  let is_test () =
    print_string "[isSpade, isHeart, isDiamond, isClub] -> ";
    let test1 = Card.isSpade (Card.newCard Value.As Color.Spade) in
    let test2 = Card.isHeart (Card.newCard Value.As Color.Heart) in
    let test3 = Card.isDiamond (Card.newCard Value.As Color.Diamond) in
    let test4 = Card.isClub (Card.newCard Value.As Color.Club) in
    print_endline (if test1 && test2 && test3 && test4 then "OK" else "NG")
  in

  new_card_test ();
  allSpades_test ();
  allHearts_test ();
  allDiamonds_test ();
  allClubs_test ();
  all_test ();
  getValue_test ();
  getColor_test ();
  toString_test ();
  toStringVerbose_test ();
  compare_test ();
  max_test ();
  min_test ();
  best_test ();
  is_test ()
;;

card_test ()

   
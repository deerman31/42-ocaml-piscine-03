open Deck

let color_test () =
  let test_all () =
    print_string "[all] -> ";
    print_endline
      (if
         Deck.Card.Color.all
         = [
             Deck.Card.Color.Spade;
             Deck.Card.Color.Heart;
             Deck.Card.Color.Diamond;
             Deck.Card.Color.Club;
           ]
       then "OK"
       else "NG")
  in

  let test_toString () =
    print_string "[toString] -> ";
    let test_cases =
      [
        (Deck.Card.Color.Spade, "S");
        (Deck.Card.Color.Heart, "H");
        (Deck.Card.Color.Diamond, "D");
        (Deck.Card.Color.Club, "C");
      ]
    in
    print_endline
      (if
         List.for_all
           (fun (c, expected) -> Deck.Card.Color.toString c = expected)
           test_cases
       then "OK"
       else "NG")
  in

  let test_toStringVerbose () =
    print_string "[toStringVerbose] -> ";
    let test_cases =
      [
        (Deck.Card.Color.Spade, "Spade");
        (Deck.Card.Color.Heart, "Heart");
        (Deck.Card.Color.Diamond, "Diamond");
        (Deck.Card.Color.Club, "Club");
      ]
    in
    print_endline
      (if
         List.for_all
           (fun (c, expected) -> Deck.Card.Color.toStringVerbose c = expected)
           test_cases
       then "OK"
       else "NG")
  in
  print_endline "[COLOR-TEST]";
  test_all ();
  test_toString ();
  test_toStringVerbose ();
  print_newline ()
;;

color_test ()

let value_test () =
  print_endline "[VALUE-TEST]";
  let test_all () =
    print_string "[all] -> ";
    let test1 =
      Deck.Card.Value.all
      = [
          Deck.Card.Value.T2;
          Deck.Card.Value.T3;
          Deck.Card.Value.T4;
          Deck.Card.Value.T5;
          Deck.Card.Value.T6;
          Deck.Card.Value.T7;
          Deck.Card.Value.T8;
          Deck.Card.Value.T9;
          Deck.Card.Value.T10;
          Deck.Card.Value.Jack;
          Deck.Card.Value.Queen;
          Deck.Card.Value.King;
          Deck.Card.Value.As;
        ]
    in
    print_endline (if test1 then "OK" else "NG")
  in
  let test_toInt () =
    print_string "[toInt] -> ";
    let test_cases =
      [
        (Deck.Card.Value.T2, 2);
        (Deck.Card.Value.T3, 3);
        (Deck.Card.Value.T4, 4);
        (Deck.Card.Value.T5, 5);
        (Deck.Card.Value.T6, 6);
        (Deck.Card.Value.T7, 7);
        (Deck.Card.Value.T8, 8);
        (Deck.Card.Value.T9, 9);
        (Deck.Card.Value.T10, 10);
        (Deck.Card.Value.Jack, 11);
        (Deck.Card.Value.Queen, 12);
        (Deck.Card.Value.King, 13);
        (Deck.Card.Value.As, 1);
      ]
    in
    print_endline
      (if
         List.for_all
           (fun (v, expected) -> Deck.Card.Value.toInt v = expected)
           test_cases
       then "OK"
       else "NG")
  in
  let test_toString () =
    print_string "[toString] -> ";
    let test_cases =
      [
        (Deck.Card.Value.T2, "2");
        (Deck.Card.Value.T3, "3");
        (Deck.Card.Value.T4, "4");
        (Deck.Card.Value.T5, "5");
        (Deck.Card.Value.T6, "6");
        (Deck.Card.Value.T7, "7");
        (Deck.Card.Value.T8, "8");
        (Deck.Card.Value.T9, "9");
        (Deck.Card.Value.T10, "10");
        (Deck.Card.Value.Jack, "J");
        (Deck.Card.Value.Queen, "Q");
        (Deck.Card.Value.King, "K");
        (Deck.Card.Value.As, "A");
      ]
    in
    print_endline
      (if
         List.for_all
           (fun (v, expected) -> Deck.Card.Value.toString v = expected)
           test_cases
       then "OK"
       else "NG")
  in
  let test_toStringVerbose () =
    print_string "[toStringVerbose] -> ";
    let test_cases =
      [
        (Deck.Card.Value.T2, "2");
        (Deck.Card.Value.T3, "3");
        (Deck.Card.Value.T4, "4");
        (Deck.Card.Value.T5, "5");
        (Deck.Card.Value.T6, "6");
        (Deck.Card.Value.T7, "7");
        (Deck.Card.Value.T8, "8");
        (Deck.Card.Value.T9, "9");
        (Deck.Card.Value.T10, "10");
        (Deck.Card.Value.Jack, "Jack");
        (Deck.Card.Value.Queen, "Queen");
        (Deck.Card.Value.King, "King");
        (Deck.Card.Value.As, "As");
      ]
    in
    print_endline
      (if
         List.for_all
           (fun (v, expected) -> Deck.Card.Value.toStringVerbose v = expected)
           test_cases
       then "OK"
       else "NG")
  in
  let test_next () =
    print_string "[next] -> ";
    let test_cases =
      [
        (Deck.Card.Value.T2, Deck.Card.Value.T3);
        (Deck.Card.Value.T3, Deck.Card.Value.T4);
        (Deck.Card.Value.T4, Deck.Card.Value.T5);
        (Deck.Card.Value.T5, Deck.Card.Value.T6);
        (Deck.Card.Value.T6, Deck.Card.Value.T7);
        (Deck.Card.Value.T7, Deck.Card.Value.T8);
        (Deck.Card.Value.T8, Deck.Card.Value.T9);
        (Deck.Card.Value.T9, Deck.Card.Value.T10);
        (Deck.Card.Value.T10, Deck.Card.Value.Jack);
        (Deck.Card.Value.Jack, Deck.Card.Value.Queen);
        (Deck.Card.Value.Queen, Deck.Card.Value.King);
        (Deck.Card.Value.King, Deck.Card.Value.As);
      ]
    in
    let test1 =
      List.for_all
        (fun (v, expected) -> Deck.Card.Value.next v = expected)
        test_cases
    in
    let test2 =
      try
        let _ = Deck.Card.Value.next Deck.Card.Value.As in
        false
      with Invalid_argument _ -> true
    in
    print_endline (if test1 && test2 then "OK" else "NG")
  in

  let test_previous () =
    print_string "[previous] -> ";
    let test_cases =
      [
        (Deck.Card.Value.T3, Deck.Card.Value.T2);
        (Deck.Card.Value.T4, Deck.Card.Value.T3);
        (Deck.Card.Value.T5, Deck.Card.Value.T4);
        (Deck.Card.Value.T6, Deck.Card.Value.T5);
        (Deck.Card.Value.T7, Deck.Card.Value.T6);
        (Deck.Card.Value.T8, Deck.Card.Value.T7);
        (Deck.Card.Value.T9, Deck.Card.Value.T8);
        (Deck.Card.Value.T10, Deck.Card.Value.T9);
        (Deck.Card.Value.Jack, Deck.Card.Value.T10);
        (Deck.Card.Value.Queen, Deck.Card.Value.Jack);
        (Deck.Card.Value.King, Deck.Card.Value.Queen);
        (Deck.Card.Value.As, Deck.Card.Value.King);
      ]
    in
    let test1 =
      List.for_all
        (fun (v, expected) -> Deck.Card.Value.previous v = expected)
        test_cases
    in
    let test2 =
      try
        let _ = Deck.Card.Value.previous Deck.Card.Value.T2 in
        false
      with Invalid_argument _ -> true
    in
    print_endline (if test1 && test2 then "OK" else "NG")
  in

  test_all ();
  test_toInt ();
  test_toString ();
  test_toStringVerbose ();
  test_next ();
  test_previous ();
  print_newline ()
;;

value_test ()

let card_test () =
  print_endline "[CARD-TEST]";
  let new_card_test () =
    print_string "[newCard] -> ";
    let card = Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Spade in
    let v = Deck.Card.getValue card in
    let c = Deck.Card.getColor card in
    if v = Deck.Card.Value.As && c = Deck.Card.Color.Spade then
      print_endline "OK"
    else print_endline "NG"
  in

  let all_card_test_helper lst f =
    if List.length lst != 13 then false
    else
      let rec loop lst vt =
        match lst with
        | [] -> true
        | first :: rest ->
            if f first && Deck.Card.getValue first = vt then
              if vt = Deck.Card.Value.As then loop rest vt
              else loop rest (Deck.Card.Value.next vt)
            else false
      in
      loop lst Deck.Card.Value.T2
  in

  let allSpades_test () =
    print_string "[allSpades] -> ";
    if all_card_test_helper Deck.Card.allSpades Deck.Card.isSpade then
      print_endline "OK"
    else print_endline "NG"
  in

  let allHearts_test () =
    print_string "[allHearts] -> ";
    if all_card_test_helper Deck.Card.allHearts Deck.Card.isHeart then
      print_endline "OK"
    else print_endline "NG"
  in
  let allDiamonds_test () =
    print_string "[allDiamonds] -> ";
    if all_card_test_helper Deck.Card.allDiamonds Deck.Card.isDiamond then
      print_endline "OK"
    else print_endline "NG"
  in
  let allClubs_test () =
    print_string "[allClubs] -> ";
    if all_card_test_helper Deck.Card.allClubs Deck.Card.isClub then
      print_endline "OK"
    else print_endline "NG"
  in
  let all_test () =
    print_string "[all] -> ";
    let cards = Deck.Card.all in
    let test1 = List.length cards = 4 * 13 in
    let test2 =
      all_card_test_helper
        (List.filter Deck.Card.isSpade cards)
        Deck.Card.isSpade
    in
    let test3 =
      all_card_test_helper
        (List.filter Deck.Card.isHeart cards)
        Deck.Card.isHeart
    in
    let test4 =
      all_card_test_helper
        (List.filter Deck.Card.isDiamond cards)
        Deck.Card.isDiamond
    in
    let test5 =
      all_card_test_helper (List.filter Deck.Card.isClub cards) Deck.Card.isClub
    in
    if test1 && test2 && test3 && test4 && test5 then print_endline "OK"
    else print_endline "NG"
  in
  let getValue_test () =
    print_string "[getValue] -> ";
    let card = Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Diamond in
    print_endline
      (if Deck.Card.getValue card = Deck.Card.Value.As then "OK" else "NG")
  in
  let getColor_test () =
    print_string "[getColor] -> ";
    let card = Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Diamond in
    print_endline
      (if Deck.Card.getColor card = Deck.Card.Color.Diamond then "OK" else "NG")
  in
  let toString_test () =
    print_string "[toString] -> ";
    let card = Deck.Card.newCard Deck.Card.Value.T2 Deck.Card.Color.Spade in
    print_endline (if Deck.Card.toString card = "2S" then "OK" else "NG")
  in
  let toStringVerbose_test () =
    print_string "[toStringVerbose] -> ";
    let card = Deck.Card.newCard Deck.Card.Value.T7 Deck.Card.Color.Diamond in
    print_endline
      (if Deck.Card.toStringVerbose card = "Card(7, Diamond)" then "OK"
       else "NG")
  in
  let compare_test () =
    print_string "[compare] -> ";
    let test1 =
      Deck.Card.compare
        (Deck.Card.newCard Deck.Card.Value.T5 Deck.Card.Color.Spade)
        (Deck.Card.newCard Deck.Card.Value.T2 Deck.Card.Color.Spade)
      = 1
    in
    let test2 =
      Deck.Card.compare
        (Deck.Card.newCard Deck.Card.Value.T5 Deck.Card.Color.Spade)
        (Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Spade)
      = -1
    in
    let test3 =
      Deck.Card.compare
        (Deck.Card.newCard Deck.Card.Value.T5 Deck.Card.Color.Spade)
        (Deck.Card.newCard Deck.Card.Value.T5 Deck.Card.Color.Spade)
      = 0
    in
    print_endline (if test1 && test2 && test3 then "OK" else "NG")
  in
  let max_test () =
    print_string "[max] -> ";
    let t1 = Deck.Card.newCard Deck.Card.Value.T7 Deck.Card.Color.Spade in
    let t2 = Deck.Card.newCard Deck.Card.Value.T2 Deck.Card.Color.Spade in
    let t3 = Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Spade in
    let t4 = Deck.Card.newCard Deck.Card.Value.T7 Deck.Card.Color.Diamond in
    let test1 = Deck.Card.max t1 t2 = t1 in
    let test2 = Deck.Card.max t1 t4 = t1 in
    let test3 = Deck.Card.max t1 t3 = t3 in
    print_endline (if test1 && test2 && test3 then "OK" else "NG")
  in
  let min_test () =
    print_string "[min] -> ";
    let t1 = Deck.Card.newCard Deck.Card.Value.T7 Deck.Card.Color.Spade in
    let t2 = Deck.Card.newCard Deck.Card.Value.T2 Deck.Card.Color.Spade in
    let t3 = Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Spade in
    let t4 = Deck.Card.newCard Deck.Card.Value.T7 Deck.Card.Color.Diamond in
    let test1 = Deck.Card.min t1 t2 = t2 in
    let test2 = Deck.Card.min t1 t4 = t1 in
    let test3 = Deck.Card.min t1 t3 = t1 in
    print_endline (if test1 && test2 && test3 then "OK" else "NG")
  in
  let best_test () =
    print_string "[best] -> ";
    let test1 =
      Deck.Card.best Deck.Card.allSpades
      = Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Spade
    in
    let test2 =
      Deck.Card.best Deck.Card.allHearts
      = Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Heart
    in
    let test3 =
      Deck.Card.best Deck.Card.allDiamonds
      = Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Diamond
    in
    let test4 =
      Deck.Card.best Deck.Card.allClubs
      = Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Club
    in
    print_endline (if test1 && test2 && test3 && test4 then "OK" else "NG")
  in
  let is_test () =
    print_string "[isSpade, isHeart, isDiamond, isClub] -> ";
    let test1 =
      Deck.Card.isSpade
        (Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Spade)
    in
    let test2 =
      Deck.Card.isHeart
        (Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Heart)
    in
    let test3 =
      Deck.Card.isDiamond
        (Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Diamond)
    in
    let test4 =
      Deck.Card.isClub
        (Deck.Card.newCard Deck.Card.Value.As Deck.Card.Color.Club)
    in
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
  is_test ();
  print_newline ()
;;

card_test ()

let deck_test () =
  print_endline "[DECK-TEST]";
  let newDeck_test () =
    print_string "[newDeck, toStringList, toStringListVerbose] -> ";

    let deck1 = Deck.newDeck () in
    let deck2 = Deck.newDeck () in
    let toString_lst1 = Deck.toStringList deck1 in
    let toString_lst2 = Deck.toStringList deck2 in
    let test1 = List.length toString_lst1 = 52 in
    let test2 = toString_lst1 <> toString_lst2 in
    let toStringListVerbose_lst1 = Deck.toStringListVerbose deck1 in
    let test3 = List.length toStringListVerbose_lst1 = 52 in
    print_endline (if test1 && test2 && test3 then "OK" else "NG")
  in

  let drawCard_test () =
    print_string "[drawCard] -> ";
    let rec loop acc num =
      try
        let c, d = Deck.drawCard acc in
        if List.length (Deck.toStringList acc) != num then false
        else loop d (num - 1)
      with Failure _ -> if num = 0 then true else false
    in
    print_endline (if loop (Deck.newDeck ()) 52 then "OK" else "NG")
  in
  newDeck_test ();
  drawCard_test ()
;;

deck_test ()

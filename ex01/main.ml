open Value

let () =
  (* Test all function *)
  let all_cards = all () in
  assert (List.length all_cards = 13);
  assert (List.mem T2 all_cards);
  assert (List.mem T3 all_cards);
  assert (List.mem T4 all_cards);
  assert (List.mem T5 all_cards);
  assert (List.mem T6 all_cards);
  assert (List.mem T7 all_cards);
  assert (List.mem T8 all_cards);
  assert (List.mem T9 all_cards);
  assert (List.mem T10 all_cards);
  assert (List.mem Jack all_cards);
  assert (List.mem Queen all_cards);
  assert (List.mem King all_cards);
  assert (List.mem As all_cards);
  Printf.printf "all() test passed\n";

  (* Test toInt *)
  assert (toInt T2 = 2);
  assert (toInt T3 = 3);
  assert (toInt T4 = 4);
  assert (toInt T5 = 5);
  assert (toInt T6 = 6);
  assert (toInt T7 = 7);
  assert (toInt T8 = 8);
  assert (toInt T9 = 9);
  assert (toInt T10 = 10);
  assert (toInt Jack = 11);
  assert (toInt Queen = 12);
  assert (toInt King = 13);
  assert (toInt As = 1);
  Printf.printf "toInt() test passed\n";

  (* Test toString *)
  assert (toString T2 = "2");
  assert (toString T3 = "3");
  assert (toString T4 = "4");
  assert (toString T5 = "5");
  assert (toString T6 = "6");
  assert (toString T7 = "7");
  assert (toString T8 = "8");
  assert (toString T9 = "9");
  assert (toString T10 = "10");
  assert (toString Jack = "J");
  assert (toString Queen = "Q");
  assert (toString King = "K");
  assert (toString As = "A");
  Printf.printf "toString() test passed\n";

  (* Test toStringVerbose *)
  assert (toStringVerbose T2 = "2");
  assert (toStringVerbose T3 = "3");
  assert (toStringVerbose T4 = "4");
  assert (toStringVerbose T5 = "5");
  assert (toStringVerbose T6 = "6");
  assert (toStringVerbose T7 = "7");
  assert (toStringVerbose T8 = "8");
  assert (toStringVerbose T9 = "9");
  assert (toStringVerbose T10 = "10");
  assert (toStringVerbose Jack = "Jack");
  assert (toStringVerbose Queen = "Queen");
  assert (toStringVerbose King = "King");
  assert (toStringVerbose As = "As");
  Printf.printf "toStringVerbose() test passed\n";

  (* Test next *)
  assert (next T2 = T3);
  assert (next T3 = T4);
  assert (next T4 = T5);
  assert (next T5 = T6);
  assert (next T6 = T7);
  assert (next T7 = T8);
  assert (next T8 = T9);
  assert (next T9 = T10);
  assert (next T10 = Jack);
  assert (next Jack = Queen);
  assert (next Queen = King);
  assert (next King = As);

  (* Test next with invalid argument *)
  try
    let _ = next As in
    Printf.printf "next() test failed: should raise Invalid_argument\n";
    exit 1
  with Invalid_argument s -> (
    assert (s = "The next value of AS does not exist");
    Printf.printf "next() test passed\n";

    (* Test previous *)
    assert (previous T3 = T2);
    assert (previous T4 = T3);
    assert (previous T5 = T4);
    assert (previous T6 = T5);
    assert (previous T7 = T6);
    assert (previous T8 = T7);
    assert (previous T9 = T8);
    assert (previous T10 = T9);
    assert (previous Jack = T10);
    assert (previous Queen = Jack);
    assert (previous King = Queen);
    assert (previous As = King);

    (* Test previous with invalid argument *)
    try
      let _ = previous T2 in
      Printf.printf "previous() test failed: should raise Invalid_argument\n";
      exit 1
    with Invalid_argument s ->
      assert (s = "The previous value of T2 does not exist");
      Printf.printf "previous() test passed\n";

      (* Additional test: sequence of operations *)
      assert (T5 |> next |> next = T7);
      assert (Queen |> previous |> previous = T10);
      Printf.printf "sequence operations test passed\n";

      Printf.printf "All tests passed!\n")

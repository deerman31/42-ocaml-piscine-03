open Value

let value_test () =
  print_endline "[VALUE-TEST]";
  let test_all () =
    print_string "[all] -> ";
    let test1 =
      Value.all = [ T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As ]
    in
    print_endline (if test1 then "OK" else "NG")
  in
  let test_toInt () =
    print_string "[toInt] -> ";
    let test_cases =
      [
        (Value.T2, 2);
        (Value.T3, 3);
        (Value.T4, 4);
        (Value.T5, 5);
        (Value.T6, 6);
        (Value.T7, 7);
        (Value.T8, 8);
        (Value.T9, 9);
        (Value.T10, 10);
        (Value.Jack, 11);
        (Value.Queen, 12);
        (Value.King, 13);
        (Value.As, 1);
      ]
    in
    print_endline
      (if
         List.for_all (fun (v, expected) -> Value.toInt v = expected) test_cases
       then "OK"
       else "NG")
  in
  let test_toString () =
    print_string "[toString] -> ";
    let test_cases =
      [
        (Value.T2, "2");
        (Value.T3, "3");
        (Value.T4, "4");
        (Value.T5, "5");
        (Value.T6, "6");
        (Value.T7, "7");
        (Value.T8, "8");
        (Value.T9, "9");
        (Value.T10, "10");
        (Value.Jack, "J");
        (Value.Queen, "Q");
        (Value.King, "K");
        (Value.As, "A");
      ]
    in
    print_endline
      (if
         List.for_all
           (fun (v, expected) -> Value.toString v = expected)
           test_cases
       then "OK"
       else "NG")
  in
  let test_toStringVerbose () =
    print_string "[toStringVerbose] -> ";
    let test_cases =
      [
        (Value.T2, "2");
        (Value.T3, "3");
        (Value.T4, "4");
        (Value.T5, "5");
        (Value.T6, "6");
        (Value.T7, "7");
        (Value.T8, "8");
        (Value.T9, "9");
        (Value.T10, "10");
        (Value.Jack, "Jack");
        (Value.Queen, "Queen");
        (Value.King, "King");
        (Value.As, "As");
      ]
    in
    print_endline
      (if
         List.for_all
           (fun (v, expected) -> Value.toStringVerbose v = expected)
           test_cases
       then "OK"
       else "NG")
  in
  let test_next () =
    print_string "[next] -> ";
    let test_cases =
      [
        (Value.T2, Value.T3);
        (Value.T3, Value.T4);
        (Value.T4, Value.T5);
        (Value.T5, Value.T6);
        (Value.T6, Value.T7);
        (Value.T7, Value.T8);
        (Value.T8, Value.T9);
        (Value.T9, Value.T10);
        (Value.T10, Value.Jack);
        (Value.Jack, Value.Queen);
        (Value.Queen, Value.King);
        (Value.King, Value.As);
      ]
    in
    let test1 =
      List.for_all (fun (v, expected) -> Value.next v = expected) test_cases
    in
    let test2 =
      try
        let _ = Value.next Value.As in
        false
      with Invalid_argument _ -> true
    in
    print_endline (if test1 && test2 then "OK" else "NG")
  in

  let test_previous () =
    print_string "[previous] -> ";
    let test_cases =
      [
        (Value.T3, Value.T2);
        (Value.T4, Value.T3);
        (Value.T5, Value.T4);
        (Value.T6, Value.T5);
        (Value.T7, Value.T6);
        (Value.T8, Value.T7);
        (Value.T9, Value.T8);
        (Value.T10, Value.T9);
        (Value.Jack, Value.T10);
        (Value.Queen, Value.Jack);
        (Value.King, Value.Queen);
        (Value.As, Value.King);
      ]
    in
    let test1 =
      List.for_all (fun (v, expected) -> Value.previous v = expected) test_cases
    in
    let test2 =
      try
        let _ = Value.previous Value.T2 in
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
  test_previous ()
;;

value_test ()

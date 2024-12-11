(* main.ml *)

open Color

let color_test () =
  let test_all () =
    print_string "[all] -> ";
    print_endline
      (if Color.all = [ Spade; Heart; Diamond; Club ] then "OK" else "NG");
    print_endline "--------------------------------------"
  in

  let test_toString () =
    print_string "[toString] -> ";
    let test_cases =
      [
        (Color.Spade, "S");
        (Color.Heart, "H");
        (Color.Diamond, "D");
        (Color.Club, "C");
      ]
    in
    print_endline
      (if
         List.for_all
           (fun (c, expected) -> Color.toString c = expected)
           test_cases
       then "OK"
       else "NG");
    print_endline "--------------------------------------"
  in

  let test_toStringVerbose () =
    print_string "[toStringVerbose] -> ";
    let test_cases =
      [
        (Color.Spade, "Spade");
        (Color.Heart, "Heart");
        (Color.Diamond, "Diamond");
        (Color.Club, "Club");
      ]
    in
    print_endline
      (if
         List.for_all
           (fun (c, expected) -> Color.toStringVerbose c = expected)
           test_cases
       then "OK"
       else "NG");
    print_endline "--------------------------------------"
  in

  print_endline "[COLOR-TEST]";
  test_all ();
  test_toString ();
  test_toStringVerbose ()
;;

color_test ()

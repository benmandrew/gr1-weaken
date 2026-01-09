open Core
open Gr1_weaken

let%expect_test "print lasso in tabular format" =
  let states =
    [
      [ ("p", true); ("q", false) ];
      [ ("p", false); ("q", true) ];
      [ ("p", true); ("q", true) ];
    ]
  in
  Lasso.print (Lasso.of_states states 1);
  [%expect
    {|
             0 1 2
    p       │●│ │●│
    q       │ │●│●│
    =Lasso=    └─┘
    |}]

let%expect_test "print lasso with longer prefix" =
  let states =
    [ [ ("a", true) ]; [ ("a", true) ]; [ ("a", false) ]; [ ("a", true) ] ]
  in
  Lasso.print (Lasso.of_states states 0);
  [%expect
    {|
             0 1 2 3
    a       │●│●│ │●│
    =Lasso=  └─────┘
    |}]

let%expect_test "print lasso with multiple variables" =
  let states =
    [
      [ ("busy", false); ("grant", false) ];
      [ ("busy", true); ("grant", false) ];
      [ ("busy", false); ("grant", true) ];
    ]
  in
  Lasso.print (Lasso.of_states states 2);
  [%expect
    {|
             0 1 2
    busy    │ │●│ │
    grant   │ │ │●│
    =Lasso=      ⊔
    |}]

let%expect_test "get future states" =
  let states =
    [ [ ("p", true) ]; [ ("p", false) ]; [ ("p", true) ]; [ ("p", true) ] ]
  in
  let lasso = Lasso.of_states states 2 in
  let future_0 = Lasso.get_future_states lasso 0 in
  let future_1 = Lasso.get_future_states lasso 1 in
  let future_2 = Lasso.get_future_states lasso 2 in
  let future_3 = Lasso.get_future_states lasso 3 in
  let future_4 = Lasso.get_future_states lasso 4 in
  printf "Number of future states from\n";
  printf "  index 0: %d\n" (List.length future_0);
  printf "  index 1: %d\n" (List.length future_1);
  printf "  index 2: %d\n" (List.length future_2);
  printf "  index 3: %d\n" (List.length future_3);
  printf "  index 4: %d\n" (List.length future_4);
  [%expect
    {|
    Number of future states from
      index 0: 4
      index 1: 3
      index 2: 2
      index 3: 2
      index 4: 2
    |}]

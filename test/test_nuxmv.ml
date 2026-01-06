open Core
open Why3
open Gr1_weaken

let make_var name =
  let id = Ident.id_fresh name in
  let ty = Ty.ty_bool in
  Term.create_vsymbol id ty

let%expect_test "nuXmv counterexample xml" =
  let p = make_var "p" in
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[] ~asm_liveness:[]
      ~gnt_init:Term.t_true
      ~gnt_safety:[ Term.t_var p ]
      ~gnt_liveness:[]
  in
  match Nuxmv.check spec with
  | Valid -> print_endline "VALID"
  | Error msg -> printf "ERROR: %s\n" msg
  | Invalid xml ->
      xml
      |> String.split_lines
         (* |> List.filter ~f:(Fn.non (String.is_prefix ~prefix:"*** "))
		  |> List.filter ~f:(Fn.non String.is_empty) *)
      |> List.iter ~f:print_endline;
      [%expect
        {|
        <?xml version="1.0" encoding="UTF-8"?>
        <counter-example type="0" id="1" desc="LTL Counterexample" >
        <node>
        <state id="1">
        <value variable="p">FALSE</value>
        </state>
        </node>
        <node>
        <state id="2">
        <value variable="p">TRUE</value>
        </state>
        </node>
        <node>
        <state id="3">
        <value variable="p">TRUE</value>
        </state>
        </node>
        <loops> 2 </loops>
        </counter-example>
        |}]

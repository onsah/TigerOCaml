open OUnit2

let test_break_outside_loop _ = failwith "TODO"

let suite =
  "SemantTest" >::: [ "test_break_outside_loop" >:: test_break_outside_loop ]


let () = run_test_tt_main suite

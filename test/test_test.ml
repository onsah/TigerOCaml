open OUnit2

let test_1 _ = assert_equal 2 (1 + 1)

let suite = "TestSuit1" >::: [ "test_1" >:: test_1 ]

let () = run_test_tt_main suite

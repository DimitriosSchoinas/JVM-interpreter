open OUnit2
open Lap2324P2
open Runner

let program0 = [
  "Start: PUSH 0";
  "PUSH 99";
  "JMP Loop";
  "Loop: DUP";
  "PUSH 0";
  "CMP";
  "JNZ Exit";
  "SWP";
  "OVER";
  "L0: ADD";
  "SWP";
  "PUSH 1";
  "SUB";
  "JMP Loop";
  "Exit: POP";
  "RETURN";
]

let program1 = [
  "Start: PUSH 1";
  "PUSH 2";
  "ADD";
  "RETURN"
]

let program2 = [
  "Start: PUSH 1";
  "PUSH 2";
  "L0: ADD";
  "RETURN"
]

let program3 = [
  "Start: PSH 1";
  "PUSH 2";
  "L0: ADD";
  "RETURN"
]

let program4 = [
  "Start: PSH 1 : 2";
  "PUSH 2";
  "L0: ADD";
  "RETURN"
]

let program5 = [
  "PUSH 1";
  "PUSH 2";
  "L0: ADD";
  "RETURN"
]

let program6 = [
  "Start:PUSH 1";
  "L0: ADD";
  "RETURN"
]

let program7 = [
  "Start:PUSH 1";
  "PUSH 1";
  "PUSH 1";
  "L0: ADD";
  "RETURN"
]

let program8 = [
  "Start:PUSH 1";
  "PUSH 1";
  "PUSH 1";
  "L0: ADD"
]

let program9 = [
  "Start: PUSH 1";
  "PUSH 2";
  "CMP";
  "JNZ L0";
  "PUSH 1";
  "RETURN";
  "L0: PUSH 0";
  "RETURN"
]

let program10 = [
  "Start: PUSH 1";
  "PUSH 1";
  "CMP";
  "JNZ L0";
  "PUSH 1";
  "RETURN";
  "L0: PUSH 0";
  "RETURN"
]

let program11 = [
  "Start: PUSH 1";
  "PUSH 1";
  "CMP";
  "JNZ L0";
  "PUSH 1";
  "RETURN";
  "L1: PUSH 0";
  "RETURN"
]

let program12 = [
  "Start: PUSH 1";
  "PUSH 0";
  "DIV";
  "RETURN";
]

let program13 = [
  "Start:PUSH 1";
  "PUSH 2";
  "JMP Start";
  "ADD";
  "RETURN";
]

let program14 = [
]

let invalid_run p str_err =
  try ignore (run p "Start")
  with Failure s -> assert_equal str_err s

let tests_runner = "test suite for runner" >::: [
    "runner good program 1" >::
    (fun _ -> assert_equal 4950 (run program0 "Start"));
    "runner good program 2" >::
    (fun _ -> assert_equal 3 (run program1 "Start"));
    "runner good program 3" >::
    (fun _ -> assert_equal 3 (run program2 "Start"));
    "runner good program 4" >::
    (fun _ -> assert_equal 1 (run program9 "Start"));
    "runner good program 5" >::
    (fun _ -> assert_equal 0 (run program10 "Start"));
    "runner invalid instruction" >::
    (fun _ -> invalid_run program3 "Invalid instruction");
    "runner invalid line" >::
    (fun _ -> invalid_run program4 "Invalid line");
    "runner expecting label" >::
    (fun _ -> invalid_run program5 "Expecting label");
    "runner unexpected empty stack" >::
    (fun _ -> invalid_run program6 "Unexpected empty stack");
    "runner empty not stack" >::
    (fun _ -> invalid_run program7 "Stack not empty");
    "runner no return instruction" >::
    (fun _ -> invalid_run program8 "No return instruction");
    "runner label not found" >::
    (fun _ -> invalid_run program11 "Label not found:L0");
    "runner division by zero" >::
    (fun _ -> invalid_run program12 "Division by zero");
    "runner expecting label" >::
    (fun _ -> invalid_run program13 "Expecting label");
    "runner label not found" >::
    (fun _ -> invalid_run program14 "Label not found:Start");
  ]

let _ = run_test_tt_main tests_runner

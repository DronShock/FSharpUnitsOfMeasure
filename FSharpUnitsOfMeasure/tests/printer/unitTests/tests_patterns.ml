(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Patterns
open Parse.Constants

(************************** Idents **************************)

let%expect_test "single underscore should be parsed as a wildcard" =
  pp pp_pattern parse_pat {| _ |};
  [%expect {| Pattern_wild |}]
;;

let%expect_test "parse ident as pattern ident" =
  pp pp_pattern parse_pat {| x |};
  [%expect {| (Pattern_ident_or_op "x") |}]
;;

let%expect_test "parse + inside parentheses as pattern ident" =
  pp pp_pattern parse_pat {| (+) |};
  [%expect {| (Pattern_ident_or_op "+") |}]
;;

let%expect_test "parse +. operator inside parentheses as pattern ident" =
  pp pp_pattern parse_pat {| (+.) |};
  [%expect {| (Pattern_ident_or_op "+.") |}]
;;

(************************** Consts **************************)

let%expect_test "parse const int as pattern const int" =
  pp pp_pattern parse_pat {| 1 |};
  [%expect {| (Pattern_const (Const_int 1)) |}]
;;

(************************** Tuples **************************)

let%expect_test "parse pattern tuple with 0 elements should fail" =
  pp pp_pattern parse_pat {| , |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse pattern tuple with 1 element should fail" =
  pp pp_pattern parse_pat {| 1, |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse pattern tuple with 2 elements" =
  pp pp_pattern parse_pat {| 1, 2 |};
  [%expect
    {|
      (Pattern_tuple ((Pattern_const (Const_int 1)), (Pattern_const (Const_int 2)),
         [])) |}]
;;

let%expect_test "parse pattern tuple with 3 elements" =
  pp pp_pattern parse_pat {| 1, 2, myname |};
  [%expect
    {|
    (Pattern_tuple ((Pattern_const (Const_int 1)), (Pattern_const (Const_int 2)),
       [(Pattern_ident_or_op "myname")])) |}]
;;

let%expect_test "parse pattern tuples of tuples" =
  pp pp_pattern parse_pat {| (1, 2), (3, 4) |};
  [%expect
    {|
    (Pattern_tuple (
       (Pattern_tuple ((Pattern_const (Const_int 1)),
          (Pattern_const (Const_int 2)), [])),
       (Pattern_tuple ((Pattern_const (Const_int 3)),
          (Pattern_const (Const_int 4)), [])),
       [])) |}]
;;

let%expect_test "parse pattern tuple of lists" =
  pp pp_pattern parse_pat {| ( [1; 2], [3; 4] ) |};
  [%expect
    {|
    (Pattern_tuple (
       (Pattern_list
          [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2))]),
       (Pattern_list
          [(Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))]),
       []))  |}]
;;

(************************** Lists **************************)

let%expect_test "parse pattern empty list" =
  pp pp_pattern parse_pat {| [] |};
  [%expect {| (Pattern_list [])  |}]
;;

let%expect_test "parse pattern list of 1 element" =
  pp pp_pattern parse_pat {| [a] |};
  [%expect {| (Pattern_list [(Pattern_ident_or_op "a")])  |}]
;;

let%expect_test "parse pattern list of 2 element" =
  pp pp_pattern parse_pat {| [a; b] |};
  [%expect {| (Pattern_list [(Pattern_ident_or_op "a"); (Pattern_ident_or_op "b")])  |}]
;;

let%expect_test "parse pattern list of list" =
  pp pp_pattern parse_pat {| [ [ 1; 2; 3] ] |};
  [%expect
    {|
    (Pattern_list
       [(Pattern_list
           [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2));
             (Pattern_const (Const_int 3))])
         ])  |}]
;;

let%expect_test "parse pattern list of tuples without parentheses" =
  pp pp_pattern parse_pat {| [ 1, 2; 3, 4 ] |};
  [%expect
    {|
    (Pattern_list
       [(Pattern_tuple ((Pattern_const (Const_int 1)),
           (Pattern_const (Const_int 2)), []));
         (Pattern_tuple ((Pattern_const (Const_int 3)),
            (Pattern_const (Const_int 4)), []))
         ])  |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse const in parentheses as pattern const" =
  pp pp_pattern parse_pat {| ( 1 ) |};
  [%expect {| (Pattern_const (Const_int 1)) |}]
;;

let%expect_test "parse pattern in unbalanced parentheses should fail" =
  pp pp_pattern parse_pat {| ( 1 ))) |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse pattern tuple in parentheses" =
  pp pp_pattern parse_pat {| (1, 2, 3, 4) |};
  [%expect
    {|
    (Pattern_tuple ((Pattern_const (Const_int 1)), (Pattern_const (Const_int 2)),
       [(Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))])) |}]
;;

let%expect_test "parse pattern list in parentheses" =
  pp pp_pattern parse_pat {| ([1; 2; 3; 4]) |};
  [%expect
    {|
    (Pattern_list
       [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2));
         (Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))]) |}]
;;

(************************** OR patterns **************************)

let%expect_test "parse OR pattern of two ident patterns" =
  pp pp_pattern parse_pat {| x | y |};
  [%expect {|
    (Pattern_or ((Pattern_ident_or_op "x"), (Pattern_ident_or_op "y"))) |}]
;;

let%expect_test "parse OR pattern of three ident patterns" =
  pp pp_pattern parse_pat {| x | y | z |};
  [%expect
    {|
    (Pattern_or (
       (Pattern_or ((Pattern_ident_or_op "x"), (Pattern_ident_or_op "y"))),
       (Pattern_ident_or_op "z"))) |}]
;;

let%expect_test "parse OR pattern of tuple and ident patterns" =
  pp pp_pattern parse_pat {| x | (y, z) |};
  [%expect
    {|
    (Pattern_or ((Pattern_ident_or_op "x"),
       (Pattern_tuple ((Pattern_ident_or_op "y"), (Pattern_ident_or_op "z"), []))
       )) |}]
;;

let%expect_test "parse OR pattern of ident and tuple patterns" =
  pp pp_pattern parse_pat {| (x, y) | z |};
  [%expect
    {|
    (Pattern_or (
       (Pattern_tuple ((Pattern_ident_or_op "x"), (Pattern_ident_or_op "y"), [])),
       (Pattern_ident_or_op "z"))) |}]
;;

(************************** Typed patterns **************************)

let%expect_test "parse pattern with builtin type" =
  pp pp_pattern parse_pat {| x : int |};
  [%expect {|
    (Pattern_typed ((Pattern_ident_or_op "x"), (Type_ident "int"))) |}]
;;

let%expect_test "parse typed pattern without colon should fail" =
  pp pp_pattern parse_pat {| x int |};
  [%expect {|
    : end_of_input |}]
;;

let%expect_test "parse typed tuple" =
  pp pp_pattern parse_pat {| (x, b) : int * string |};
  [%expect
    {|
    (Pattern_typed (
       (Pattern_tuple ((Pattern_ident_or_op "x"), (Pattern_ident_or_op "b"), [])),
       (Type_tuple ((Type_ident "int"), (Type_ident "string"), [])))) |}]
;;

let%expect_test "parse typed tuple of typed identificators should fail" =
  pp pp_pattern parse_pat {| (x : int, b : string) : int * string |};
  [%expect {|
    : no more choices |}]
;;

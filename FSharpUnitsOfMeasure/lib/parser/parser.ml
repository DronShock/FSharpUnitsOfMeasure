(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Angstrom

let empty_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let lower_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let upper_letter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let ident_symbol = function
  | c -> lower_letter c || upper_letter c || digit c || Char.equal c '_'
;;

let keywords = function
  | "let"
  | "rec"
  | "fun"
  | "in"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "function"
  | "measure" -> true
  | _ -> false
;;

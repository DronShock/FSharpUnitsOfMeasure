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

let take_empty = take_while empty_space
let take_empty1 = take_while1 empty_space
let token s = take_empty *> s
let token1 s = take_empty1 *> s
let stoken s = take_empty *> string s
let stoken1 s = take_empty1 *> string s
let brackets p = stoken "(" *> p <* stoken ")"

let pwild = Pattern_wild
let pconst x = Pattern_const x
let pident x = Pattern_ident x
let ptuple t = Pattern_tuple t
let por______________N p1 p2 = Pattern_or (p1, p2)

let parse_int =
  let parse_sign =
    choice [ stoken "+" *> return 1; stoken "-" *> return (-1); stoken "" *> return 1 ]
  in
  let parse_digit = take_while1 digit in
  lift2 (fun s v -> fint @@ (s * Int.of_string v)) parse_sign parse_digit
;;

let parse_id =
  let length_id res = String.length res = 0 in
  let first_char res =
    (Char.is_digit @@ String.get res 0) || (Char.is_uppercase @@ String.get res 0)
  in
  take_empty *> take_while1 ident_symbol
  >>= fun res ->
  match length_id res, keywords res, first_char res with
  | true, _, _ -> fail "Not identifier"
  | _, true, _ -> fail "You can not use keywords as vars"
  | _, _, true -> fail "The first character of the identifier is a small letter"
  | _ -> return res
;;

let parse_pconst = parse_types >>| fun x -> pconst x
let parse_pvar = parse_id >>| fun x -> pident x
let parse_pwild = stoken "_" *> return pwild

let eifelse i t e = Expr_ifthenelse (i, t, e)
let elet is_rec name body = Expr_let (is_rec, name, body)
let etuple z = Expr_tuple z
let efun id body = Expr_fun (id, body)
let eapp f a = Expr_apply (f, a)
let eident x = Expr_ident x
let ematch c pl = Expr_match (c, pl)

let parse_eident = parse_id >>| eident
let parse_econst = parse_types >>| fun x -> Expr_const x
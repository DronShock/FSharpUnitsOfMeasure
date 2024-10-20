(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Ast
open Angstrom
open Common

let parse_pat_wild = char '_' *> return Pattern_wild
let parse_pat_ident = parse_ident >>| fun i -> Pattern_ident i
let parse_pat_const = parse_const >>| fun c -> Pattern_const c
let parse_pat = choice [ parse_pat_ident; parse_pat_wild; parse_pat_const ]

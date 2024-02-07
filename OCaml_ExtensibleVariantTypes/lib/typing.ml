(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type type_variable_number = int

type ground =
  | GInt (* Int *)
  | GBool (* Bool *)
  | GNil (* Empty list — [] *)
  | GUnit (* Unit — () *)
[@@deriving eq, show { with_path = false }]

type typ =
  | TVar of type_variable_number
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ
  | TGround of ground

let tint = TGround GInt
let tbool = TGround GBool
let tnil = TGround GNil
let tunit = TGround GUnit
let tarrow left_type right_type = TArr (left_type, right_type)
let ttuple type_list = TTuple type_list
let tlist typ = TList typ
let tvar n = TVar n

let rec pp_type fmt typ =
  let open Format in
  let arrow = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TGround x ->
    (match x with
     | GInt -> fprintf fmt "int"
     | GBool -> fprintf fmt "bool"
     | GNil -> fprintf fmt "nil"
     | GUnit -> fprintf fmt "Unit")
  | TVar var -> fprintf fmt "%s" ("'" ^ Char.escaped (Char.chr (var + 97)))
  | TTuple value_list ->
    let pp_tuple value_list =
      let pp_el fmt typ =
        let s =
          match typ with
          | TTuple _ -> format_of_string "(%a)"
          | _ -> format_of_string "%a"
        in
        fprintf fmt s pp_type typ
      in
      match value_list with
      | hd :: tl ->
        fprintf fmt "%a" pp_el hd;
        List.fold_left (fun _ typ -> fprintf fmt " * %a" pp_el typ) () tl
      | _ -> ()
    in
    pp_tuple value_list
  | TList typ ->
    fprintf
      fmt
      ((match typ with
        | TGround _ | TVar _ | TList _ -> "%a"
        | _ -> "(%a)")
       ^^ " list")
      pp_type
      typ
  | TArr (typ_left, typ_right) ->
    fprintf fmt (arrow typ_left ^^ " -> %a") pp_type typ_left pp_type typ_right
;;

let print_typ ?(carriage = false) typ =
  Format.printf ("%s" ^^ if carriage then "\n" else "") (Format.asprintf "%a" pp_type typ)
;;

type error =
  | OccursCheck
  | UnboundValue of ident
  | MismatchValues of typ * typ (* For pattern matching errors *)
  | UnificationFailed of typ * typ
  | ParserAvoidedError

let pp_error fmt err =
  let open Format in
  match err with
  | OccursCheck -> fprintf fmt "Occurs check failed"
  | MismatchValues (t1, t2) ->
    fprintf fmt "This pattern matches values of type ";
    pp_type fmt t1;
    fprintf fmt " but a pattern was expected which matches values of type ";
    pp_type fmt t2
  | UnboundValue identifier -> fprintf fmt "Unbound value %s" identifier
  | UnificationFailed (t1, t2) ->
    fprintf fmt "This expression has type ";
    pp_type fmt t1;
    fprintf fmt " but an expression was expected of ";
    pp_type fmt t2
  | ParserAvoidedError ->
    fprintf
      fmt
      "Use the parser to get the AST: the parser does some optimizations of expressions"
;;

let print_type_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "Typecheck error: %s\n" s
;;

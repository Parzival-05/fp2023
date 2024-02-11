(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( *> ) : ('a, 'e) t -> ('a, 'e) t -> ('a, 'e) t
end

type environment = (ident, value, Base.String.comparator_witness) Base.Map.t

and value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VList of value list
  | VTuple of value list
  | VFun of pattern list * expr * rec_flag * environment

type error =
  | UnboundValue of string
  | ParserAvoidedError
  | UnsupportedOperation
  | Division_by_zero
  | NonExhaustivePatternMatching
  | TypeError
  | UnexpectedState (* Unexpected state related to standard functions *)

open Format

let rec pp_value fmt =
  let pp_list fmt delimiter =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
      (fun fmt value -> pp_value fmt value)
      fmt
  in
  function
  | VInt value -> fprintf fmt "%d" value
  | VBool value -> fprintf fmt "%B" value
  | VUnit -> fprintf fmt "()"
  | VList list -> fprintf fmt "[%a]" (fun fmt -> pp_list fmt "; ") list
  | VTuple tuple -> fprintf fmt "(%a)" (fun fmt -> pp_list fmt ", ") tuple
  | VFun _ -> fprintf fmt "<fun>"
;;

let print_value = printf "%a" pp_value

let pp_error fmt = function
  | UnboundValue id -> fprintf fmt "Unbound value %s" id
  | UnsupportedOperation -> fprintf fmt "Unsupported operation"
  | Division_by_zero -> fprintf fmt "Division by zero"
  | NonExhaustivePatternMatching -> fprintf fmt "This pattern-matching is not exhaustive"
  | ParserAvoidedError ->
    fprintf
      fmt
      "Use the parser to get the AST: the parser does some transformations of expressions"
  | TypeError -> fprintf fmt "Use type checking"
  | UnexpectedState -> fprintf fmt "Unexpected state related to standard functions"
;;

let print_error = printf "%a" pp_error

module Environment (M : MONAD_FAIL) = struct
  open M

  let find map key =
    match Base.Map.find map key with
    | Some value -> return value
    | None -> fail (UnboundValue key)
  ;;

  let update environment key value = Base.Map.update environment key ~f:(fun _ -> value)
  let empty = Base.Map.empty (module Base.String)
end

module Interpret (M : MONAD_FAIL) : sig
  val run : prog -> (environment, error) M.t
end = struct
  open M
  open Environment (M)

  let rec update_env pat value env =
    match pat, value with
    | PId id, v -> return (update env id v)
    | PConst x, v ->
      (match x, v with
       | CInt n1, VInt n2 when n1 = n2 -> return env
       | CInt _, VInt _ -> fail NonExhaustivePatternMatching
       | CBool n1, VBool n2 when n1 = n2 -> return env
       | CBool _, VBool _ -> fail NonExhaustivePatternMatching
       | CNil, VList [] -> return env
       | _ -> fail TypeError)
    | PList (hd, tl), VList (hd_v :: tl_v) ->
      let* env = update_env hd hd_v env in
      update_env tl (VList tl_v) env
    | PTuple (hd :: []), VTuple (hd_v :: []) ->
      let* env = update_env hd hd_v env in
      return env
    | PTuple (hd :: tl), VTuple (hd_v :: tl_v) ->
      if tl = [] || tl_v = []
      then fail NonExhaustivePatternMatching
      else
        let* env = update_env hd hd_v env in
        update_env (PTuple tl) (VTuple tl_v) env
    | _ -> fail NonExhaustivePatternMatching

  and eval_decl environment = function
    | DLet (Not_recursive, id, expr) ->
      let* res = eval expr environment in
      return (update environment id res)
    | DLet (Recursive, id, expr) ->
      let* res = eval expr environment in
      (match res with
       | VFun (pat_list, expr, _, env) ->
         let new_env = update env id res in
         return (update new_env id (VFun (pat_list, expr, Recursive, new_env)))
       | _ -> return (update environment id res))

  and eval expr environment =
    match expr with
    | EConst const ->
      (match const with
       | CInt x -> return @@ VInt x
       | CBool x -> return @@ VBool x
       | CNil -> return @@ VList []
       | CUnit -> return @@ VUnit)
    | EBinop (left_expr, op, right_expr) ->
      let* left_value = eval left_expr environment in
      let* right_value = eval right_expr environment in
      (match left_value, op, right_value with
       | VInt x, Add, VInt y -> return @@ VInt (x + y)
       | VInt x, Sub, VInt y -> return @@ VInt (x - y)
       | VInt x, Mul, VInt y -> return @@ VInt (x * y)
       | VInt x, Div, VInt y ->
         if y = 0 then fail Division_by_zero else return @@ VInt (x / y)
       | VBool x, And, VBool y -> return @@ VBool (x && y)
       | _, (Add | Sub | Mul | Div | And), _ -> fail TypeError
       | _, Or, _ -> fail ParserAvoidedError
       | left, op, right ->
         let comparison_operation = function
           | Eq -> return Base.Poly.( = )
           | Less -> return Base.Poly.( < )
           | Leq -> return Base.Poly.( <= )
           | _ -> fail ParserAvoidedError
           (* Gre, Geq must be replaced by Less and Leq after parsing*)
         in
         let compare x y =
           let* compare = comparison_operation op in
           return @@ VBool (compare x y)
         in
         (match left, right with
          | VInt x, VInt y -> compare x y
          | VBool x, VBool y -> compare x y
          | VTuple x, VTuple y -> compare x y
          | VList x, VList y -> compare x y
          | _, _ -> fail UnsupportedOperation))
    | EId id ->
      let* res =
        run
          (find environment id)
          ~ok:(fun res -> return res)
          ~err:(fun error ->
            match error with
            | UnboundValue _ ->
              (match id with
               | "print_int" ->
                 return @@ VFun ([ PId "x" ], EStd id, Not_recursive, environment)
               | "print_newline" ->
                 return @@ VFun ([ PId "x" ], EStd id, Not_recursive, environment)
               | _ -> fail error)
            | _ -> fail error)
      in
      return
        (match res with
         | VFun (pat_list, expr, Recursive, env) ->
           let updated_env = update env id res in
           VFun (pat_list, expr, Recursive, updated_env)
         | _ -> res)
    | EFun (pat, expr) ->
      let rec get_pat_list acc = function
        | EFun (pat_next, expr) -> get_pat_list (pat_next :: acc) expr
        | expr -> acc, expr
      in
      let pat_list, function_body = get_pat_list [ pat ] expr in
      return (VFun (List.rev pat_list, function_body, Not_recursive, environment))
    | EApp (func, arg) ->
      let* func_val = eval func environment in
      let* arg_val = eval arg environment in
      (match func_val with
       | VFun (pat_list, expr, rec_flag, env) ->
         (match pat_list with
          | hd :: tl ->
            let* updated_env = update_env hd arg_val env in
            (match tl with
             | [] ->
               let _ =
                 match expr with
                 | EStd x ->
                   (match x with
                    | "print_int" ->
                      (match arg_val with
                       | VInt n ->
                         let _ = print_int n in
                         return ()
                       | _ -> fail TypeError)
                    | "print_newline" ->
                      (match arg_val with
                       | VUnit ->
                         let _ = print_newline () in
                         return ()
                       | _ -> fail TypeError)
                    | _ -> fail UnexpectedState)
                 | _ -> return ()
               in
               eval expr updated_env
             | _ -> return @@ VFun (tl, expr, rec_flag, updated_env))
          | [] -> fail TypeError)
       | _ -> fail TypeError)
    | EIf (cond, thn, els) ->
      let* value = eval cond environment in
      (match value with
       | VBool true -> eval thn environment
       | VBool false -> eval els environment
       | _ -> fail TypeError)
    | EUnop (op, expr) ->
      let* value = eval expr environment in
      (match op, value with
       | Plus, VInt x -> return @@ VInt x
       | Minus, VInt x -> return @@ VInt (-x)
       | Not, VBool x -> return @@ VBool (not x)
       | _ -> fail TypeError)
    | EList (hd, tl) ->
      let* hd = eval hd environment in
      let* tl = eval tl environment in
      (match hd, tl with
       | x, VList l -> return @@ VList (x :: l)
       | _ -> fail TypeError)
    | ETuple list ->
      let* list =
        List.fold_right
          (fun expr acc ->
            let* acc = acc in
            let* value = eval expr environment in
            return (value :: acc))
          list
          (return [])
      in
      return @@ VTuple list
    | EClsr (binding, expr) ->
      let* env_binding = eval_decl environment binding in
      eval expr env_binding
    | EMatch (expr, cases) ->
      let* value = eval expr environment in
      let rec check_patterns = function
        | (pat, res) :: tl ->
          run
            (update_env pat value environment)
            ~ok:(fun env -> eval res env)
            ~err:(fun _ -> check_patterns tl)
        | [] -> fail NonExhaustivePatternMatching
      in
      check_patterns cases
    | EStd x ->
      (match x with
       | "print_int" -> return VUnit
       | "print_newline" -> return VUnit
       | _ -> fail UnexpectedState)
  ;;

  let run (program : decl list) =
    let env = empty in
    let rec helper env = function
      | head :: tail ->
        let* head_res = eval_decl env head in
        helper head_res tail
      | [] -> return env
    in
    helper env program
  ;;
end

module InterpretResult = Interpret (struct
    include Base.Result

    let run x ~ok ~err =
      match x with
      | Ok v -> ok v
      | Error e -> err e
    ;;

    let ( let* ) monad f = bind monad ~f
    let ( *> ) l r = l >>= fun _ -> r
  end)

let run_and_pp s =
  match Parser.parse_program s with
  | Ok ast ->
    (match Inferencer.run_inference ast with
     | Ok env_inf ->
       (match InterpretResult.run ast with
        | Ok env ->
          Base.Map.fold env ~init:() ~f:(fun ~key ~data _ ->
            match Base.Map.find env_inf key with
            | None -> ()
            | Some (_, typ) ->
              if Inferencer.is_printable key typ
              then (
                let _ =
                  printf "val %s" key;
                  print_string " : ";
                  Typing.print_typ typ;
                  print_string " = ";
                  print_value data;
                  printf "\n"
                in
                ())
              else ())
        | Error err ->
          printf "Interpretation error: ";
          print_error err;
          printf "\n")
     | Error x -> Typing.print_type_error x)
  | Error x -> Format.printf "%s\n" x
;;

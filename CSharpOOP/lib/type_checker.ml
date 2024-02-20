(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open State_type
open State_type.TypeCheck
open Monad.Monad_TypeCheck

let val_to_type = function
  | VInt _ -> TInt
  | VChar _ -> TChar
  | VBool _ -> TBool
  | VString _ -> TString
;;

let vart_to_type = function
  | TVar t -> return t
;;

let vardec_to_type = function
  | Var_Declaration (t, _) -> vart_to_type t
;;

let find_mem_type = function
  | VType (TVar n) | Field { f_type = TVar n } | Method { m_type = TRetrun n } ->
    return (Some n)
  | Method { m_type = TVoid } -> return None
  | _ -> fail Mismatch
;;

let name_to_type n = read_local_el n >>= fun mem -> find_mem_type mem

let eq e el1 el2 =
  match e el1 el2 with
  | true -> return el1
  | false -> fail Mismatch
;;

let eq_type t1 t2 = eq equal_typ t1 t2
let get_cur_class_ctx = get_cur_class_name >>= fun n -> read_global_el n

let get_parent =
  get_cur_class_ctx
  >>= function
  | Class (_, _, n, _), _ | Interface (_, _, n, _), _ ->
    (match n with
     | Some n -> return n
     | None -> fail (Other "This class doesn't have a parent"))
;;

let is_access_modifier p_name ctx ~acc_modifier:m ~default:d =
  match m with
  | Some Public -> return ctx
  (*TODO: кажется, это не работает. исправить!!!!*)
  | Some Protected ->
    get_parent
    >>= fun name0 ->
    (match equal_name name0 p_name with
     | true -> return ctx
     | false -> fail Mismatch)
  | Some Private -> fail Mismatch
  | None ->
    (match d with
     | false -> fail Mismatch
     | true -> return ctx)
;;

let tc_class_modifier p_name = function
  | CField (f, _) ->
    is_access_modifier p_name (Field f) ~acc_modifier:f.f_modifier ~default:false
  | CMethod (m, _) ->
    is_access_modifier p_name (Method m) ~acc_modifier:m.m_acc_modifier ~default:false
  | CConstructor (c, _) -> return (Constructor c)
;;

let tc_interface_modifier p_name = function
  | IField f ->
    is_access_modifier p_name (Field f) ~acc_modifier:f.f_modifier ~default:true
  | IMethod m ->
    is_access_modifier p_name (Method m) ~acc_modifier:m.m_acc_modifier ~default:true
;;

let eq_name_return_ctx n1 n2 m m_t =
  match equal_name n1 n2 with
  | true -> Some (m_t m)
  | false -> None
;;

let get_class_mem name = function
  | CField (f, _) -> eq_name_return_ctx f.f_name name f (fun f -> Field f)
  | CMethod (m, _) -> eq_name_return_ctx m.m_name name m (fun m -> Method m)
  | CConstructor (c, _) -> eq_name_return_ctx c.c_name name c (fun c -> Constructor c)
;;

let get_interface_mem name = function
  | IField f -> eq_name_return_ctx f.f_name name f (fun f -> Field f)
  | IMethod m -> eq_name_return_ctx m.m_name name m (fun m -> Method m)
;;

(*TODO: добавить провверку модифаеров*)
let find_member_from_obj obj_name name =
  let find_mem b name foo =
    let f n acc m =
      match acc with
      | None -> foo n m
      | mem -> mem
    in
    List.fold_left (f name) None b
  in
  let find_cl_mem b name = find_mem b name get_class_mem in
  let find_itf_mem b name = find_mem b name get_interface_mem in
  read_global_el obj_name
  >>= function
  | Class (_, _, _, b), _ -> find_cl_mem b name |> return
  | Interface (_, _, _, b), _ -> find_itf_mem b name |> return
;;

let tc_access_by_p e1 e2 =
  let find_obj_mem_with_fail n_obj n_mem =
    find_member_from_obj n_obj n_mem
    >>= function
    | Some m -> return m
    | None -> fail (Other "Point access check error")
  in
  let get_next_mem_class_name = function
    | Field { f_type = TVar (TObj n) } -> return n
    | _ -> fail (Other "Point access check error")
  in
  let rec acc_by_p n_obj e =
    match e with
    | Exp_Name n_mem -> find_obj_mem_with_fail n_obj n_mem
    | Access_By_Point (e1, e2) ->
      (match e1 with
       | Exp_Name n_mem ->
         find_obj_mem_with_fail n_obj n_mem
         >>= fun n -> get_next_mem_class_name n >>= fun p_name -> acc_by_p p_name e2
       | _ -> fail (Other "Point access check error"))
    | _ -> fail (Other "Point access check error")
  in
  let get_first_mem_class_name = function
    | VType (TVar (TObj n)) | Field { f_type = TVar (TObj n) } -> return n
    | _ -> fail (Other "Point access check error")
  in
  let foo = function
    | Exp_Name n_mem -> read_local_el n_mem
    | Access_By_Point (e1, e2) ->
      (match e1 with
       | Exp_Name n ->
         read_local_el n
         >>= fun mem -> get_next_mem_class_name mem >>= fun n_obj -> acc_by_p n_obj e2
       | _ -> fail (Other "Point access check error"))
    | _ -> fail (Other "Point access check error")
  in
  (*TODO: добавить проверку модифаеров*)
  match e1 with
  | Exp_Name n_mem ->
    let get_el = read_local_el n_mem in
    let get_inst_tp = get_el >>= get_first_mem_class_name in
    let eq_name cl_n cur_cl_n =
      equal_name cl_n cur_cl_n
      |> function
      | true -> foo e2
      | false -> acc_by_p cl_n e2
    in
    get_inst_tp >>= fun cl_name -> get_cur_class_name >>= eq_name cl_name
  | _ -> fail (Other "Point access check error")
;;

(* let tc_method_invoke = fail *)

(*
   let tc_bin_op b =
   match b with
   | Plus | Asterisk | Dash | Slash | Mod -> return b
   | Less | LessOrEqual | More | MoreOrEqual -> return b
   | Equal | NotEqual -> return b
   | And | Or -> return b
   | Assign -> return b
   ;; *)

let tc_expr = function
  | Exp_Const t -> return (Some (val_to_type t))
  | Exp_Name n -> name_to_type n
  (* | Access_By_Point (e1, e2) -> tc_access_by_p e1 e2 *)
  (*
     | Bin_op (b, e1, e2) -> tc_bin_op b e1 e2
     | Un_op (b, e1)
     | Method_invoke
  *)
  | _ -> fail Mismatch
;;

(*
   A1 -> 1
A2 -> 2

local_ctx_st = (_, adr, memory, _)

memory = {
   adr : MapState := { fild_name : (value); ... } ;
   ...
}

A1.meth(){         # (_, 1, memory, _)
  A2.meth2();      # (_, 2, memory, _)
}
*)

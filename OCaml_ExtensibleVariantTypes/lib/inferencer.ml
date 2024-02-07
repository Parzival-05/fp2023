(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typing

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun monad f state ->
    let last, result = monad state in
    match result with
    | Error e -> last, Error e
    | Ok value -> f value last
  ;;

  let fail error state = state, Base.Result.fail error
  let return value last = last, Base.Result.return value
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f state ->
    match x state with
    | state, Ok x -> state, Ok (f x)
    | state, Error e -> state, Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RList = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run monad = snd (monad 0)
end

type fresh = int

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TTuple typ_list -> Base.List.exists typ_list ~f:(occurs_in v)
    | TList typ -> occurs_in v typ
    | TGround _ -> false
  ;;

  let free_vars =
    let empty_set = Base.Set.empty (module Base.Int) in
    let rec helper acc = function
      | TVar n -> Base.Set.add acc n
      | TArr (left, right) -> helper (helper acc left) right
      | TTuple typ_list ->
        Base.List.fold_right
          typ_list
          ~f:(fun typ acc -> Base.Set.union (helper empty_set typ) acc)
          ~init:acc
      | TList typ -> helper acc typ
      | TGround _ -> acc
    in
    helper empty_set
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : ?is_rec:bool -> fresh -> typ -> t R.t

  (** Getting value from substitution *)
  val find_exn : fresh -> t -> typ

  val find : fresh -> t -> typ option
  val apply : t -> typ -> typ
  val unify : ?is_rec:bool -> typ -> typ -> t R.t

  (** Compositon of substitutions *)
  val compose : ?is_rec:bool -> t -> t -> t R.t

  val compose_all : ?is_rec:bool -> t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  (* An association list. In real world replace it by Map *)
  type t = (fresh, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  let mapping ?(is_rec = false) key value =
    if (not is_rec) && Type.occurs_in key value
    then fail OccursCheck
    else return (key, value)
  ;;

  let singleton ?(is_rec = false) key value =
    let* key, value = mapping ~is_rec key value in
    return @@ Base.Map.update empty key ~f:(fun _ -> value)
  ;;

  let find_exn key subst = Base.Map.find_exn subst key
  let find key subst = Base.Map.find subst key
  let remove subst key = Base.Map.remove subst key

  let apply s =
    let rec helper = function
      | TVar n ->
        (match find n s with
         | None -> tvar n
         | Some x -> x)
      | TArr (left, right) -> tarrow (helper left) (helper right)
      | TTuple typ_list -> ttuple @@ Base.List.map typ_list ~f:helper
      | TList typ -> tlist @@ helper typ
      | ground -> ground
    in
    helper
  ;;

  let rec unify ?(is_rec = false) l r =
    match l, r with
    | TGround GUnit, _ | _, TGround GUnit -> return empty
    | TGround l, TGround r when l = r -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar b, t | t, TVar b -> singleton ~is_rec b t
    | TArr (f1, s1), TArr (f2, s2) ->
      let* subst1 = unify ~is_rec f1 f2 in
      let* subst2 = unify ~is_rec (apply subst1 s1) (apply subst1 s2) in
      compose ~is_rec subst1 subst2
    | TTuple typ_list_l, TTuple typ_list_r ->
      (match Base.List.zip typ_list_l typ_list_r with
       | Base.List.Or_unequal_lengths.Unequal_lengths -> fail (UnificationFailed (l, r))
       | Base.List.Or_unequal_lengths.Ok zipped_list ->
         Base.List.fold_right
           zipped_list
           ~f:(fun (typ_l, typ_r) subst ->
             let* subst_pair = unify ~is_rec typ_l typ_r in
             let* subst = subst in
             compose ~is_rec subst_pair subst)
           ~init:(return empty))
    | TList typ1, TList typ2 -> unify ~is_rec typ1 typ2
    | _ -> fail (UnificationFailed (l, r))

  and extend ?(is_rec = false) key value subst =
    match find key subst with
    | None ->
      let v = apply subst value in
      let* s2 = singleton ~is_rec key v in
      RList.fold_left
        subst
        ~f:(fun k v acc ->
          let v = apply s2 v in
          let* k, v = mapping ~is_rec k v in
          return @@ Base.Map.update acc k ~f:(fun _ -> v))
        ~init:(return s2)
    | Some v2 ->
      let* s2 = unify ~is_rec value v2 in
      compose ~is_rec subst s2

  and compose ?(is_rec = false) s1 s2 =
    RList.fold_left s2 ~init:(return s1) ~f:(extend ~is_rec)
  ;;

  let compose_all ?(is_rec = false) substs =
    Base.List.fold_left substs ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose ~is_rec acc subst)
  ;;
end

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

module Scheme = struct
  type t = scheme

  let empty = Base.Set.empty (module Base.Int)

  let occurs_in key = function
    | set, t -> (not (Base.Set.mem set key)) && Type.occurs_in key t
  ;;

  let free_vars = function
    | set, t -> Base.Set.diff set (Type.free_vars t)
  ;;

  let apply subst (s, t) =
    let s2 = Base.Set.fold s ~init:subst ~f:(fun acc k -> Subst.remove acc k) in
    s, Subst.apply s2 t
  ;;
end

module TypeEnv = struct
  type t = (ident, scheme, Base.String.comparator_witness) Base.Map.t

  let extend env id scheme = Base.Map.update env id ~f:(fun _ -> scheme)
  let empty = Base.Map.empty (module Base.String)

  let free_vars : t -> (type_variable_number, Base.Int.comparator_witness) Base.Set.t =
    Base.Map.fold
      ~init:(Base.Set.empty (module Base.Int))
      ~f:(fun ~key:_ ~data acc -> Base.Set.union acc (Scheme.free_vars data))
  ;;

  let apply s env = Base.Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Base.Map.find_exn ~equal:String.equal map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| tvar

let instantiate ?(is_rec = false) =
  let fold_right set init f =
    Base.Set.fold_right set ~init ~f:(fun x acc ->
      let* acc = acc in
      f acc x)
  in
  fun (set, t) ->
    fold_right set (return t) (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton ~is_rec name f1 in
      return @@ Subst.apply s typ)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env typ ->
  let free = Base.Set.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  free, typ
;;

let lookup_env id map =
  match Base.Map.find map id with
  | None -> fail (UnboundValue id)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let infer_ptrn ?(env = TypeEnv.empty) pat =
  let rec helper env = function
    | PConst x ->
      let* typ =
        match x with
        | CInt _ -> return tint
        | CBool _ -> return tbool
        | CNil ->
          let* fresh_variable = fresh_var in
          return (tlist fresh_variable)
      in
      return (env, typ)
    | PId x ->
      if x <> "_"
      then
        let* fresh_var = fresh_var in
        let env = TypeEnv.extend env x (Scheme.empty, fresh_var) in
        return (env, fresh_var)
      else return (env, tunit)
    | PList (hd, tl) ->
      let* env, typ_hd = helper env hd in
      let* env, typ_tl = helper env tl in
      let* subst = Subst.unify (tlist typ_hd) typ_tl in
      let env = TypeEnv.apply subst env in
      return (env, tlist (Subst.apply subst typ_hd))
    | PTuple tuple ->
      let rec tuple_pat env acc = function
        | hd :: tl ->
          let* env, typ_hd = helper env hd in
          tuple_pat env (typ_hd :: acc) tl
        | [] -> return (env, ttuple (List.rev acc))
      in
      tuple_pat env [] tuple
  in
  helper env pat
;;

let rec infer env = function
  | EConst c ->
    let* typ =
      match c with
      | CInt _ -> return tint
      | CBool _ -> return tbool
      | CNil ->
        let* fresh_var = fresh_var in
        return (tlist fresh_var)
    in
    return (Subst.empty, typ)
  | EBinop (left, binop, right) ->
    let* subst_left, typ_left = infer env left in
    let* subst_right, typ_right = infer (TypeEnv.apply subst_left env) right in
    let* subst_result = Subst.compose_all [ subst_right; subst_left ] in
    (match binop with
     | Add | Sub | Mul | Div ->
       let* subst_1 = unify typ_left tint in
       let* subst_2 = unify typ_right tint in
       let* subst_result = Subst.compose_all [ subst_1; subst_2; subst_result ] in
       return (subst_result, tint)
     | Eq | Less | Leq ->
       let* subst = unify typ_left typ_right in
       let* subst_result = Subst.compose_all [ subst; subst_result ] in
       return (subst_result, tbool)
     | And ->
       let* subst_1 = unify typ_left tbool in
       let* subst_2 = unify typ_right tbool in
       let* subst_result = Subst.compose_all [ subst_1; subst_2; subst_result ] in
       return @@ (subst_result, tbool)
     | _ -> fail @@ ParserAvoidedError)
  | EId ident -> lookup_env ident env
  | EUnop (unop, expr) ->
    let* subst, typ = infer env expr in
    (match unop with
     | Minus | Plus ->
       let* subst_1 = unify typ tint in
       let* subst_result = Subst.compose subst_1 subst in
       return (subst_result, tint)
     | Not ->
       let* subst_1 = unify typ tbool in
       let* subst_result = Subst.compose subst_1 subst in
       return (subst_result, tbool))
  | EApp (e1, e2) ->
    let* subst_1, typ_1 = infer env e1 in
    let* subst_2, typ_2 = infer (TypeEnv.apply subst_1 env) e2 in
    let* type_variable = fresh_var in
    let* subst_3 = unify (tarrow typ_2 type_variable) (Subst.apply subst_2 typ_1) in
    let type_result = Subst.apply subst_3 type_variable in
    let* subst_result = Subst.compose_all [ subst_1; subst_2; subst_3 ] in
    return (subst_result, type_result)
  | EFun (pat, expr) ->
    let* env_pat, typ_pat = infer_ptrn pat ~env in
    let* subst_expr, typ_expr = infer env_pat expr in
    return (subst_expr, tarrow (Subst.apply subst_expr typ_pat) typ_expr)
  | EIf (cond, thn, els) ->
    let* subst_cond, typ_cond = infer env cond in
    let* subst_thn, typ_thn = infer env thn in
    let* subst_els, typ_els = infer env els in
    let* subst_1 = unify (Subst.apply subst_els typ_cond) tbool in
    let* subst_2 = unify typ_thn typ_els in
    let* subst_result =
      Subst.compose_all [ subst_cond; subst_thn; subst_els; subst_1; subst_2 ]
    in
    let type_result = Subst.apply subst_result typ_thn in
    return (subst_result, type_result)
  | EList (hd, tl) ->
    let* subst_hd, typ_hd = infer env hd in
    let typ_lhd = tlist typ_hd in
    let* subst_tl, typ_tl = infer env tl in
    let* s3 = unify typ_lhd typ_tl in
    let* subst_result = Subst.compose_all [ subst_hd; subst_tl; s3 ] in
    return (subst_result, Subst.apply subst_result typ_lhd)
  | ETuple x ->
    let* s, t =
      List.fold_left
        (fun acc expr ->
          let* subst, typ = infer env expr in
          let* subst_acc, typ_acc = acc in
          let* subst = Subst.compose subst subst_acc in
          return (subst, typ :: typ_acc))
        (return (Subst.empty, []))
        x
    in
    return (s, ttuple @@ List.rev t)
  | EClsr (decl, expr) ->
    let* subst_decl, env = infer_decl env decl in
    let* subst, typ_expr = infer env expr in
    let* subst_result = Subst.compose_all [ subst; subst_decl ] in
    return (subst_result, typ_expr)
  | EMatch (e, patmatch) ->
    let rec check_cases typ_res typ_e subst_e = function
      | (pat, expr_res) :: tl ->
        let* env_new, typ_pat = infer_ptrn pat ~env in
        let* subst_pat =
          match run (unify typ_pat typ_e) with
          | Ok subst_pat -> return subst_pat
          | Error _ -> fail @@ MismatchValues (typ_pat, typ_e)
        in
        let* subst_res_2, typ_res_2 = infer (TypeEnv.apply subst_pat env_new) expr_res in
        let* subst_u = unify typ_res_2 (Subst.apply subst_res_2 typ_res) in
        let* subst_e = Subst.compose_all [ subst_e; subst_u; subst_res_2; subst_pat ] in
        let typ_res_2 = Subst.apply subst_e typ_res_2 in
        (match tl with
         | [] -> return (subst_e, typ_res_2)
         | _ -> check_cases typ_res_2 (Subst.apply subst_e typ_pat) subst_e tl)
      | [] -> fail ParserAvoidedError
    in
    let* subst_e, typ_e = infer env e in
    let* type_variable = fresh_var in
    check_cases type_variable typ_e subst_e patmatch

and infer_decl env = function
  | DLet (Not_recursive, id, expr) ->
    let* subst, typ = infer env expr in
    let env = TypeEnv.extend env id (generalize env typ) in
    return @@ (subst, env)
  | DLet (Recursive, id, expr) ->
    let* type_variable = fresh_var in
    let env = TypeEnv.extend env id (Scheme.empty, type_variable) in
    let* subst, typ = infer env expr in
    let* subst_2 = unify (Subst.apply subst type_variable) typ in
    let* subst = Subst.compose ~is_rec:true subst_2 subst in
    let type_variable = Subst.apply subst type_variable in
    let scheme = generalize env type_variable in
    return @@ (subst, TypeEnv.extend env id scheme)
;;

let run_inference prog =
  run
    (Base.List.fold_left prog ~init:(return TypeEnv.empty) ~f:(fun env decl ->
       let* env = env in
       let* _, env = infer_decl env decl in
       return env))
;;

let print_env env =
  match env with
  | Ok env ->
    Base.Map.fold env ~init:() ~f:(fun ~key ~data _ ->
      let _, typ = data in
      let _ = Format.printf "val %s : " key in
      print_typ ~carriage:true typ)
  | Error x -> print_type_error x
;;

(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_lib.Type_checker
open Csharp_lib.Monad
open Csharp_lib.Parser
open Csharp_lib.Ast

let show_wrap = function
  | Some x ->
    (match type_checker x with
     | _, Result.Ok _ -> Format.print_string "All right\n"
     | _, Result.Error er -> Format.printf "%a\n%!" pp_error er)
  | _ -> Format.print_string "Some error\n"
;;

let print_tc ps str = show_wrap (parse_to_some ps str)
let test_ast = print_tc p_ast

let%expect_test _ =
  test_ast
    {|
    class Test {
      int Fac(int num) {
        if (num == 1) {
          return 1;
        }
        else 
        {
          return num * Fac(num - 1);
        }
      }
    } |};
  [%expect {| All right |}]
;;

let%expect_test _ =
  test_ast
    {|
    class Test {
      int Fac(int num) {
        if (num == 1) {
          return "woow";
        }
      }
    } |};
  [%expect {| Returned type does not match the function type |}]
;;

let%expect_test _ =
  test_ast {| 
  class Test {
    int a = 5;
    int b = 9;
    int a = 9;
  } |};
  [%expect {| This variable is already declared |}]
;;

let%expect_test _ =
  test_ast
    {|
     interface I
     {
        void a();
        public void b();
     }
     class C : I
     {
        public void a(){
        }
        public void b(){
        }
     } |};
  [%expect {| All right |}]
;;

let%expect_test _ =
  test_ast
    {|
     interface I
     {
        void a();
        public void b();
     }
     class C : I
     {
        public void a(){
        }
        public int b(){
          return 1;
        }
        private void c(){
        }
     } |};
  [%expect {| Class incorrectly inherited from an interface |}]
;;

let%expect_test _ =
  test_ast
    {|
    interface I1 {
      void a();
    }
    interface I2 : I1 {
      void b();
    }
    class C : I2 {
      public void a(){
      }
      public void b(){
      }
    } |};
  [%expect {| All right |}]
;;

let%expect_test _ =
  test_ast
    {|
    interface I1 {
      void a();
    }
    interface I2 : I1 {
      void b();
    }
    class C : I2 {
      public void b(){
      }
    } |};
  [%expect {| Class incorrectly inherited from an interface |}]
;;

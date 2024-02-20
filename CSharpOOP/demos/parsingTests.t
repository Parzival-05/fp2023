(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ./demoParse.exe <<-EOF
  > class Test {
  >   static int Fac(int num) 
  >   {
  >      if (num == 1)
  >      {
  >         return 1;
  >      }
  >      else
  >      {
  >         return num * Fac(num - 1);
  >      }
  >   }
  > }
  > EOF
  Some error

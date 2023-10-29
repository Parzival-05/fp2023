(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ./demoParse.exe < fac.cs
  (Method ((Some [Static]), (TNot_nullable_type (TBase TInt)), (Name "Fac"),
     (Exp_Args
        [(Var_Declaration ((TNot_nullable_type (TBase TInt)), (Name "num")))]),
     (Exp_Body
        [(If_Else (
            (Bin_op (Equal, (Exp_Name (Name "num")), (Exp_Val (VInt 1)))),
            (Exp_Body [(Return (Some (Exp_Val (VInt 1))))]),
            (Some (Exp_Body
                     [(Return
                         (Some (Bin_op (Asterisk, (Exp_Name (Name "num")),
                                  (Method_invoke ((Exp_Name (Name "Fac")),
                                     (Exp_Args
                                        [(Bin_op (Dash,
                                            (Exp_Name (Name "num")),
                                            (Exp_Val (VInt 1))))
                                          ])
                                     ))
                                  ))))
                       ]))
            ))
          ])
     ))

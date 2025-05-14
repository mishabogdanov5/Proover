(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type expr =
  | Var of string
  | Inv of expr
  | Disj of expr * expr
  | Conj of expr * expr
  | Impl of expr * expr
  | Equiv of expr * expr

val parse_expr : string -> (expr, string) result
val pp : expr -> string

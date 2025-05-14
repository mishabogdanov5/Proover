(** Copyright 2025-2025, mishabogdanov5 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

type expr =
  | Var of string
  | Inv of expr
  | Disj of expr * expr
  | Conj of expr * expr
  | Impl of expr * expr
  | Equiv of expr * expr

let ws = skip_while (function ' ' | '\t' | '\n' -> true | _ -> false)

let var_p =
  ws *> take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
  >>= fun s -> return (Var s)

let parens p = ws *> char '(' *> p <* ws <* char ')'
let inv_op = ws *> char '!' *> return (fun x -> Inv x)
let disj_op = ws *> char '|' *> return (fun x y -> Disj (x, y))
let conj_op = ws *> char '&' *> return (fun x y -> Conj (x, y))
let impl_op = ws *> char '=' *> char '>' *> return (fun x y -> Impl (x, y))

let equiv_op =
  ws *> char '<' *> char '=' *> char '>' *> return (fun x y -> Equiv (x, y))

let rec left_assoc_parse_bin elem op =
  elem >>= fun x ->
  op >>= (fun f -> left_assoc_parse_bin elem op >>| fun y -> f x y) <|> return x

let parse_unary op elem =
  let rec parse_ops acc =
    op
    >>= (fun f -> parse_ops (f :: acc))
    <|> (elem >>| fun x -> List.fold_left (fun x f -> f x) x (List.rev acc))
  in
  parse_ops []

let left_assoc_parse_un op elem = parse_unary op elem
(*elem >>= fun x ->
  op
  >>= (fun f ->
  left_assoc_parse_un op elem >>| fun y -> f y)
  <|> return x *)

let expr_p =
  fix (fun expr ->
      let factor = choice [ var_p; parens expr ] in
      let lv1 = left_assoc_parse_un inv_op factor in
      let lv2 = left_assoc_parse_bin lv1 conj_op in
      let lv3 = left_assoc_parse_bin lv2 disj_op in
      let lv4 = left_assoc_parse_bin lv3 impl_op in
      left_assoc_parse_bin lv4 equiv_op)

let parse_expr s = parse_string ~consume:All expr_p s

let rec pp = function
  | Var s -> "(Var " ^ s ^ ")"
  | Inv e -> "(Inv " ^ pp e ^ ")"
  | Conj (e1, e2) -> "(Conj " ^ pp e1 ^ ", " ^ pp e2 ^ ")"
  | Disj (e1, e2) -> "(Disj " ^ pp e1 ^ ", " ^ pp e2 ^ ")"
  | Impl (e1, e2) -> "(Impl " ^ pp e1 ^ ", " ^ pp e2 ^ ")"
  | Equiv (e1, e2) -> "(Equiv " ^ pp e1 ^ ", " ^ pp e2 ^ ")"

let print_expr s =
  match parse_expr s with
  | Ok result -> print_string (pp result)
  | Error e -> print_string e

let%expect_test "parse-log-test-1" =
  print_expr "a&b&c&!d";
  [%expect {|(Conj (Var a), (Conj (Var b), (Conj (Var c), (Inv (Var d)))))|}]

let%expect_test "parse-log-test-2" =
  print_expr "!a<=>(b|c)";
  [%expect {|(Equiv (Inv (Var a)), (Disj (Var b), (Var c)))|}]

let%expect_test "parse-log-test-3" =
  print_expr "!!!!!v=>!!d";
  [%expect
    {|(Impl (Inv (Inv (Inv (Inv (Inv (Var v)))))), (Inv (Inv (Var d))))|}]

let%expect_test "parse-log-test-4" =
  print_expr "(a&b|c=>!d) <=> !b|!c";
  [%expect
    {|(Equiv (Impl (Disj (Conj (Var a), (Var b)), (Var c)), (Inv (Var d))), (Disj (Inv (Var b)), (Inv (Var c))))|}]

(*[@@@ocaml.text "/*"]

(** Copyright 2025-2025, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

[@@@ocaml.warnerror "-32"]

open Angstrom

(** Expression parser without associativity *)
let prio expr table =
  let len = Array.length table in
  let loop = List.fold_left (fun acc (op, r) -> op acc r) in
  let rec helper level =
    if level >= len then expr
    else
      let xs = table.(level) in
      return loop
      <*> helper (level + 1)
      <*> many
            (choice
               (List.map
                  (fun (op, f) ->
                    op *> helper (level + 1) >>= fun r -> return (f, r))
                  xs))
  in
  helper 0

type assoc = Left | Right

(** Expression parser with associativity *)
let prio_assoc expr table =
  let len = Array.length table in
  let rec loop acc = function
    | [] -> acc
    | (Left, op, r) :: tl -> loop (op acc r) tl
    | (Right, op, r) :: tl -> op acc (loop r tl)
  in
  let rec helper level =
    if level >= len then expr
    else
      let xs = table.(level) in
      return loop
      <*> helper (level + 1)
      <*> many
            (choice
               (List.map
                  (fun (ass, op, f) ->
                    op *> helper (level + 1) >>= fun r -> return (ass, f, r))
                  xs))
  in
  helper 0

let digit =
  let open Angstrom in
  let* c = any_char in
  if c <= '9' && '0' <= c then return (Char.code c - Char.code '0')
  else fail "digit "

let ws =
  let open Angstrom in
  many (char ' ' <|> char '\n')

module Expr = struct
  type bin_op = Plus | Dash | Asterisk | Slash | Power
  [@@deriving show { with_path = false }]

  type expr = Const of int | Binop of bin_op * expr * expr
  [@@deriving show { with_path = false }]

  let const c = Const c
  let binop op a b = Binop (op, a, b)
  let expr_small = digit >>| const

  let expr =
    prio expr_small
      [|
        [
          (ws *> char '+' <* ws, fun a b -> Binop (Plus, a, b));
          (ws *> char '-' <* ws, fun a b -> Binop (Dash, a, b));
        ];
        [
          (ws *> char '*' <* ws, fun a b -> Binop (Asterisk, a, b));
          (ws *> char '/' <* ws, fun a b -> Binop (Slash, a, b));
        ];
      |]

  let test p pp text =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All p text with
    | Result.Ok ast -> Format.printf "%a\n%!" pp ast
    | Error s -> Format.printf "Error: %s" s

  open Format

  let rec pretty ppf = function
    | Const n -> fprintf ppf "%d" n
    | Binop (Plus, l, r) -> fprintf ppf "%a+%a" pretty l pretty r
    | Binop (Dash, l, r) -> fprintf ppf "%a-%a" pretty l pretty r
    | Binop (Asterisk, l, r) -> fprintf ppf "%a*%a" pretty l pretty r
    | Binop (Power, l, r) -> fprintf ppf "(%a**%a)" pretty l pretty r
    | Binop (Slash, l, r) -> fprintf ppf "%a/%a" pretty l pretty r

  let%expect_test _ =
    test expr pretty "1+2*3";
    [%expect {| 1+2*3 |}]

  let%expect_test _ =
    test expr pretty "0+0*0";
    [%expect {| 0+0*0 |}]

  let%expect_test _ =
    test expr pretty "0*0*0";
    [%expect {| 0*0*0 |}]

  let%expect_test _ =
    let input =
      String.init 900001 (fun n -> if n mod 2 <> 0 then '*' else '1')
    in
    test expr (fun ppf _ -> Format.fprintf ppf "parsable") input;
    [%expect {| parsable |}]

  let expr2 =
    prio_assoc expr_small
      [|
        [
          (Left, ws *> char '+' <* ws, fun a b -> Binop (Plus, a, b));
          (Left, char '-', fun a b -> Binop (Dash, a, b));
        ];
        [ (Left, ws *> char '/' <* ws, fun a b -> Binop (Slash, a, b)) ];
        [
          ( Right,
            ws *> char '*' *> char '*' <* ws,
            fun a b -> Binop (Power, a, b) );
        ];
      |]

  let%expect_test _ =
    test expr2 pretty "1/2+1**2**3";
    [%expect {|
      1/2+(1**(2**3)) |}]
end

let test p pp text =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All p text with
  | Result.Ok ast -> Format.printf "%a\n%!" pp ast
  | Error s -> Format.printf "Error: %s" s

module Prop = struct
  type bin_op = Conj | Disj | Impl | Equiv
  [@@deriving show { with_path = false }]

  type expr = Var of char | Neg of expr | Binop of bin_op * expr * expr
  [@@deriving show { with_path = false }]

  let var c = Var c
  let binop op a b = Binop (op, a, b)
  let v = satisfy (function 'a' .. 'z' -> true | _ -> false)

  let expr =
    [|
      [
        (ws *> char '-' <* ws, fun a -> Neg a);
        (ws *> char '\' <* ws, fun a b -> Binop (Disj, a, a));
      ];
      [
        (ws *> char '*' <* ws, fun a  -> Binop (Conj, a, a));
        (ws *> char '/' <* ws, fun a  -> Binop (Impl, a, a));
      ];
    |]

  open Format

  let rec pretty ppf = function
    | Var n -> fprintf ppf "%c" n
    | Neg expr -> fprintf ppf "-%a" pretty expr
    | Binop (Conj, l, r) -> fprintf ppf "%a / %a" pretty l pretty r
    | Binop (Disj, l, r) -> fprintf ppf "%a \/ %a" pretty l pretty r
    | Binop (Impl, l, r) -> fprintf ppf "%a => %a)" pretty l pretty r
    | Binop (Equiv, l, r) -> fprintf ppf "%a <=> %a" pretty l pretty r

  let%expect_test _ =
    test expr pretty "a";
    [%expect {| Error: : not implemented |}]

  let%expect_test _ =
    test expr pretty {|a\/b|};
    [%expect {| Error: : not implemented |}]

  let%expect_test _ =
    test expr pretty {|a \/ b /\ c|};
    [%expect {| Error: : not implemented |}]

  let%expect_test _ =
    test expr pretty {|(a \/ b) /\ c|};
    [%expect {| Error: : not implemented |}]

  let%expect_test _ =
    test expr pretty {|!(a \/ b)|};
    [%expect {| Error: : not implemented |}]

  let%expect_test _ =
    test expr pretty {|(a => b => c)|};
    [%expect {| Error: : not implemented |}]
end

module User_cmd = struct
  type t =
    | Focus of int
    | Qed
    | Intros
    | Axiom
    | Destruct of int
    | Unfold_not
    | Apply of int
    | Left
    | Right
  [@@deriving show { with_path = false }]

  let number =
    let* xs = many1 digit in
    return (List.fold_left (fun acc x -> (acc * 10) + x) 0 xs)

  let parse =
    let open Angstrom in
    choice
      [
        (ws *> string "Axiom." <* ws) *> return Axiom;
        (ws *> string "Intros." <* ws) *> return Intros;
        (ws *> string "Qed." <* ws) *> return Qed;
        ( ws *> string "Focus " *> ws *> any_int8 <* char '.' >>| fun x ->
          Focus x );
      ]

  (*let%expect_test _ =
    test parse pp {|Focus 2.|};
    [%expect {| (Focus 50) |}]*)
end

let parse_formula =
  Angstrom.parse_string ~consume:Angstrom.Consume.All Prop.expr

let parse_cmd =
  Angstrom.parse_string ~consume:Angstrom.Consume.All User_cmd.parse*)

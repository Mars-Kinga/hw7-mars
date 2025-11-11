open Tokens
open Ast

exception ParseError of token list

(** [consume_token t toks] ensures that the head of [toks] is [t]. If it is, it
    returns the tail of [toks]. Otherwise, it raises a [ParseError].  *)
let consume_token : token -> token list -> token list =
  fun t toks ->
    begin match toks with
      | t' :: tail when t = t' ->
          tail

      | _ ->
          raise (ParseError toks)
    end

(** [call_or_prim f args toks] returns an expression corresponding to the
    application of [f] to [args]. If [f] is a primitive, the AST node for this
    will application will be [Prim0], [Prim1], or [Prim2]; otherwise, it will be
    [Call]. If the length of [args] does not match the arity of [f], this
    function will throw [ParseError toks]. *)
let call_or_prim : string -> expr list -> token list -> expr =
  fun f args toks ->
  begin match f with
    | "read_num" ->
        begin match args with
          | [] -> Prim0 ReadNum
          | _ -> raise (ParseError toks)
        end

    | "newline" ->
        begin match args with
          | [] -> Prim0 Newline
          | _ -> raise (ParseError toks)
        end

    | "add1" ->
        begin match args with
          | [arg] -> Prim1 (Add1, arg)
          | _ -> raise (ParseError toks)
        end

    | "sub1" ->
        begin match args with
          | [arg] -> Prim1 (Sub1, arg)
          | _ -> raise (ParseError toks)
        end

    | "is_zero" ->
        begin match args with
          | [arg] -> Prim1 (IsZero, arg)
          | _ -> raise (ParseError toks)
        end

    | "is_num" ->
        begin match args with
          | [arg] -> Prim1 (IsNum, arg)
          | _ -> raise (ParseError toks)
        end

    | "is_pair" ->
        begin match args with
          | [arg] -> Prim1 (IsPair, arg)
          | _ -> raise (ParseError toks)
        end

    | "is_empty" ->
        begin match args with
          | [arg] -> Prim1 (IsEmpty, arg)
          | _ -> raise (ParseError toks)
        end

    | "left" ->
        begin match args with
          | [arg] -> Prim1 (Left, arg)
          | _ -> raise (ParseError toks)
        end

    | "right" ->
        begin match args with
          | [arg] -> Prim1 (Right, arg)
          | _ -> raise (ParseError toks)
        end

    | "print" ->
        begin match args with
          | [arg] -> Prim1 (Print, arg)
          | _ -> raise (ParseError toks)
        end

    | "pair" ->
        begin match args with
          | [arg1; arg2] -> Prim2 (Pair, arg1, arg2)
          | _ -> raise (ParseError toks)
        end

    | _ ->
        Call (f, args)
  end

let peek : token list -> token option =
  function
  | [] -> None
  | t :: _ -> Some t

let expect_id : token list -> string * token list =
  function
  | ID x :: toks -> (x, toks)
  | toks -> raise (ParseError toks)

let rec parse_program : token list -> program =
  fun toks ->
    let defns, toks = parse_defns toks in
    let body, toks = parse_expr toks in
    if List.length toks <> 0 then
      raise (ParseError toks)
    else
      {defns; body}

and parse_defns : token list -> defn list * token list =
  fun toks ->
    ([], toks)

and parse_expr : token list -> expr * token list =
  parse_if_let

and parse_if_let : token list -> expr * token list =
  function
  | IF :: toks ->
      let e1, toks = parse_expr toks in
      let toks = consume_token THEN toks in
      let e2, toks = parse_expr toks in
      let toks = consume_token ELSE toks in
      let e3, toks = parse_expr toks in
      (If (e1, e2, e3), toks)
  | LET :: toks ->
      let x, toks = expect_id toks in
      let toks = consume_token EQ toks in
      let e1, toks = parse_expr toks in
      let toks = consume_token IN toks in
      let e2, toks = parse_expr toks in
      (Let (x, e1, e2), toks)
  | toks -> parse_cmp toks

and parse_cmp : token list -> expr * token list =
  fun toks ->
    let e1, toks = parse_sum toks in
    match peek toks with
    | Some EQ ->
        let toks = consume_token EQ toks in
        let e2, toks = parse_sum toks in
        (Prim2 (Eq, e1, e2), toks)
    | Some LT ->
        let toks = consume_token LT toks in
        let e2, toks = parse_sum toks in
        (Prim2 (Lt, e1, e2), toks)
    | _ -> (e1, toks)

and parse_sum : token list -> expr * token list =
  fun toks ->
    let e1, toks = parse_app toks in
    match peek toks with
    | Some PLUS ->
        let toks = consume_token PLUS toks in
        let e2, toks = parse_expr toks in (* right-associative by default *)
        (Prim2 (Plus, e1, e2), toks)
    | Some MINUS ->
        let toks = consume_token MINUS toks in
        let e2, toks = parse_expr toks in
        (Prim2 (Minus, e1, e2), toks)
    | _ -> (e1, toks)

and parse_app : token list -> expr * token list =
  function
  | NUM n :: toks -> (Num n, toks)
  | ID f :: LPAREN :: toks ->
      let rec args acc toks =
        match peek toks with
        | Some RPAREN -> (List.rev acc, consume_token RPAREN toks)
        | _ ->
            let e, toks = parse_expr toks in
            (match peek toks with
             | Some COMMA -> args (e :: acc) (consume_token COMMA toks)
             | Some RPAREN -> (List.rev (e :: acc), consume_token RPAREN toks)
             | _ -> raise (ParseError toks))
      in
      let args, toks = args [] toks in
      (call_or_prim f args toks, toks)
  | ID x :: toks ->
      let lower = String.lowercase_ascii x in
      (match lower with
       | "true" -> (True, toks)
       | "false" -> (False, toks)
       | "nil" -> (Nil, toks)
       | _ -> (Var x, toks))
  | LPAREN :: toks ->
      let e, toks = parse_expr toks in
      let toks = consume_token RPAREN toks in
      (e, toks)
  | toks -> raise (ParseError toks)

let parse : string -> program =
  fun s ->
    s
      |> tokenize
      |> parse_program

exception ParseError of string

let parse s =
  begin try Parser.parse s with
    | Parser.ParseError toks ->
        raise
          ( ParseError
              ( toks
                  |> List.map Tokens.string_of_token
                  |> String.concat ", "
              )
          )
  end

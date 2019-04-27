(* Convert program into a list of tokens *)
val lex : string -> Token.token list

(* Get string representation of a token *)
val token_to_string: Token.token -> string

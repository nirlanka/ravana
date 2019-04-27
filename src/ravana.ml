open Core_kernel

let compile progFileName =
    let sourceLines = In_channel.read_lines progFileName in
	let reducer line1 line2 = line1^" "^line2 in
    let ast = List.reduce_exn sourceLines ~f:reducer
    |> Lex.lex
    |> Parse.parse
    in
    Generate.generate progFileName ast

let fileName = Array.get Sys.argv 1

let _ = compile fileName

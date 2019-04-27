open Core_kernel
open Printf

let generate fileName programAst = 

  let fileNameOut = fileName^".js" in
  let chan = open_out fileNameOut in	(* Use to output line with:
  										   Printf.fprintf channel "%s\n" str
  										*)
  let fprint_line = Printf.fprintf chan "%s\n" in (* Use to output line *)

  (* let handle_error message = begin
      Printf.printf "ERROR: %s\n" message;
      close_out chan;
      Sys.remove fileNameOut;
      exit 1;
  end in *)

  let generate_fun global_ctx (f:Ast.fun_declaration) =
	begin match f.body with
	  | Some body -> fprint_line (match f.name with | ID str -> str);
	  | None -> ();
	end;
	global_ctx
  in

  let generate_global_var global_ctx (gv:Ast.declaration) =
	begin 
		let var_name = (match gv.var_name with | ID str -> str) in
		fprint_line var_name;
	end;
	global_ctx
  in

  let generate_tl global_ctx = function
    | Ast.Function f -> generate_fun global_ctx f
    | Ast.GlobalVar gv -> generate_global_var global_ctx gv
  in

  let rec generate_tls global_ctx = function
    | [] -> ();
    | tl::tls ->
       let global_ctx' = generate_tl global_ctx tl in
       generate_tls global_ctx' tls
  in

  match programAst with
    | Ast.Prog tl_list ->
      let global_ctx = Context.empty in
      let _ = generate_tls global_ctx tl_list in
      close_out chan
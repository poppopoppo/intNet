let pnt s =
  print_string (s^"\n");flush stdout
let pnt_line () =
  pnt "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~"
let gl_src:(string option ref) = ref None
let gl_dst:(string option ref) = ref None
let gl_gl_cod:(string option ref) = ref None
let string_to_command l = Parser.buffer Lexer.token l
let file_to_gl_cod l = Parser.text Lexer.token l
let load_gc (s:string) : string ModNet.gl_cod =
  let reg = Str.regexp ".+\\.gl" in
  if (Str.string_match reg s 0)
  then
    let f = open_in s in
    let c = file_to_gl_cod (Lexing.from_channel f) in
    let _ = close_in f in
    c
  else raise @@ Failure ("error:load: can't load "^s^". file prefix need to be gl")
let exit_calc st dst err =
  (match (st,dst) with
   | (None,None) -> ()
   | (Some _,None) -> ()
   | (None,Some _) -> ()
   | (Some st,Some dst) -> Env.save !st dst);
  match err with
  | None -> exit 0
  | Some e -> raise e

let _ =
  let _ = Arg.parse
      [("-s",Arg.String
          (fun s -> gl_src := (Some s)),"src file");
       ("-d",Arg.String (fun s -> gl_dst := (Some s)),"dst file");
       ("-g",Arg.String (fun s -> gl_gl_cod := Some s),"gl_cod file")
      ]
      (fun _ -> ()) "-s src_filename -d dst_filename" in

  let (gl_src,gl_dst) = (!gl_src,!gl_dst) in
  let gl_st = match gl_src with
    | None -> ref (Env.new_state ())
    | Some s -> ref (Env.load s) in

  let _ = Sys.signal Sys.sigint
      (Signal_handle (fun _ -> exit_calc (Some gl_st) gl_dst None)) in
  let gl_cod =
    match !gl_gl_cod with
    | None -> ModNet.End
    | Some s -> (load_gc s) in
  while true do
    try
      let _ = pnt_line ();Env.print_st !gl_st in
      let _ = print_string "command # ";flush stdout in
      let s = input_line stdin in
      let lexbuf = Lexing.from_string s in
      let result = string_to_command lexbuf in
      let _  = gl_st := Env.exec_command gl_cod result (!gl_st) in
      ()
    with
    | Parser.Error -> pnt "error: parsing error"
    | Env.Error s -> pnt s
    | Failure s -> pnt @@ "error:"^s
    | err -> exit_calc (Some gl_st) gl_dst (Some err)
  done

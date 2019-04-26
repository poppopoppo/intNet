type create_flg = Strict | Forgetable
let create_flg = Forgetable
let debug_flg = ref true
let pnt s = if !debug_flg=true then print_string s;flush stdout

module State = Map.Make(String)
type state = int State.t
exception Error of string
let (gl_st:state) = State.empty
let new_state () = State.empty

let rec calc l c =
  match c with
  | ModNet.Plus (o1,o2) -> (calc l o1) + (calc l o2)
  | ModNet.Mult (o1,o2) -> (calc l o1) * (calc l o2)
  | ModNet.Int z -> z
  | ModNet.Val i -> snd @@ List.nth l i

let consume k m =
  try
    let v = State.find k m in
    let m' = State.remove k m in
    (v,m')
  with Not_found -> raise @@ Error ("error: ¿"^k^" is not exist")
let create (k,v) m =
  match create_flg with
  | Strict ->
    (try
       let _ = State.find k m in
       raise @@ Error ("error: ¿"^k^" is allready exist")
     with Not_found -> State.add k v m)
  | Forgetable -> State.add k v m
let peek k st =
  try
    let v = State.find k st in
    v
  with Not_found -> raise @@ Error ("error:peek: ¿"^k^" is not exist")
let rec consumes src_l st =
  match src_l with
  | [] -> ([],st)
  | i::tl ->
    let (v,st') = consume i st in
    let (l',st'') = consumes tl st' in
    ((i,v)::l',st'')
let rec creates l m =
  match l with
  | [] -> m
  | (k,v)::tl ->
    let m' = creates tl m in
    let m'' = create (k,v) m' in
    m''
let rec find_l k l =
  match l with
  | [] -> raise @@ Error "error:find_l:not found"
  | (k0,v0)::tl -> if k0=k then v0 else (find_l k tl)
let exec_gl_cll (_ : string ModNet.gl_cod) (_:string) (_:string list) (_:string list) = []

let exec_command glc (c : string ModNet.cmd) st =
  try
    let (src,dst,op) = (c.src,c.dst,c.gl_cll) in
    let (l,st') = consumes src st in
    let result = match op with
      | Gl_nam s -> exec_gl_cll glc s src dst
      | Ply p ->
        let r = List.map (calc l) p in
        List.combine dst r in
    let st'' = creates result st' in
    st''
  with Invalid_argument s -> raise @@ Error s


let print_st st =
  let _ = print_string "state # ";flush stdout in
  let _ = State.iter
      (fun k v ->
         print_string ("$"^k^"="^(string_of_int v)^",");
         flush stdout )
      st in
  let _ = print_string "\n";flush stdout in
  ()

let load (s:string) : state =
  try
    let reg = Str.regexp ".+\\.st" in
    if (Str.string_match reg s 0)
    then
      let f = open_in s in
      let s0 = Marshal.from_channel f  in
      let _ = close_in f in
      let _ = Sys.remove s in
      s0
    else raise @@ Error ("error:load: can't load "^s^". file prefix need to be st")
  with | Error err -> raise @@ Error err
       | err -> pnt "error:load\n"; raise err
let save (st:state) s =
  let reg = Str.regexp ".+\\.st" in
  if (Str.string_match reg s 0)
  then let f = open_out s in
    let _ = Marshal.to_channel f st [] in
    ()
  else raise @@ Error "error:load: can't save to s. file prefix need to be st"

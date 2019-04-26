type 'k gl_cod =
  | End
  | Seq of ('k gl_cod) * ('k lc_etr)
and 'k lc_etr =
  | Clq of ('k lc_etr) list
  | Etr of {
      src : 'k list;
      dst : 'k list;
      nam : 'k;
      cod : 'k lc_cod
    }
and 'k lc_cod =
  | Lc_End
  | Agl of {
      cnd : 'k list;
      brc : (('k list) * ('k gl_cll) * ('k lc_cod)) list
    }
  | Whe of 'k cmd * 'k lc_cod * 'k lc_cod
and 'k cmd = {
  src : 'k list;
  dst : 'k list;
  gl_cll : 'k gl_cll
}
and 'k gl_cll =
  | Gl_nam of 'k
  | Ply of ('k ply) list
and 'k ply =
  | Plus of (('k ply) * ('k ply))
  | Mult of (('k ply) * ('k ply))
  | Int of int
  | Val of int
type plg =
  | Num of num
  | Pt of plg
  | Rcd of plg list
  | Ax of string
and num =
  | Int
  | FInt of int
  | Exp of (num * num)

%{

%}

%token SRC OPR CLN CRT WHR WHR_END
%token ORM ORH TEST CLQ LCE CMD
%token L_PRN R_PRN ORE
%token <string> NAM GL_NAM
%token <int> INT VAL
%token PLS MLT
%token EOF

%left PLS
%left MLT

%start text buffer
%type <(string ModNet.gl_cod)> text
%type <(string ModNet.cmd)> buffer
%%
buffer:
  | cmd EOF { $1 }
  ;
text:
  | gl_cod EOF  { $1 }
  ;
gl_cod:
(*  | { End } *)
  | lc_etr { ModNet.End }
  | gl_cod lc_etr { ModNet.Seq ($1,$2) }
(*  | gl_code def_flow *)
  ;
lc_etr:
  | LCE lc_man {  $2 }
  | LCE clq_lst { ModNet.Clq $2 }
  ;
lc_man:
  | NAM CLN nms SRC nms OPR OPR lc_cod { ModNet.Etr { src=$3;dst=$5;nam=$1;cod=$8} }
  ;
clq_lst:
  | CLQ lc_man { [$2] }
  | clq_lst CLQ lc_man { $1@[$3]}
  ;
nms:
  | { [] }
  | nms nam { $1@[$2] }
  ;
nam:
  | NAM { $1 }
  | CRT NAM { "+"^$2 }
  ;
lc_cod:
  | { ModNet.Lc_End }
  | cmd lc_cod { ModNet.Whe ($1,ModNet.Lc_End,$2) }
  | WHR cmd lc_cod WHR_END lc_cod { ModNet.Whe ($2,$3,$5) }
  | agl { $1 }
  ;
cmd:
  | CMD nms SRC nms OPR gl_cll { { src=$2;dst=$4;gl_cll=$6} }
  ;
agl:
  | ORH nms agl_lst { ModNet.Agl {cnd=$2;brc=$3} }
  ;
agl_lst:
  | agl_lst_m ORE nms OPR gl_cll lc_cod { $1@[($3,$5,$6)] }
  ;
agl_lst_m:
  | { [] }
  | agl_lst_m ORM nms OPR gl_cll lc_cod { $1@[($3,$5,$6)] }
  ;
gl_cll:
  | GL_NAM { ModNet.Gl_nam $1 }
  | ply_lst { ModNet.Ply $1 }
  ;
ply_lst:
  | { [] }
  | ply_lst ply { $1@[$2]}
  ;
ply:
  | VAL  { ModNet.Val 0 }
  | INT { ModNet.Int $1 }
  | ply PLS ply { ModNet.Plus ($1,$3) }
  | ply MLT ply { ModNet.Mult ($1,$3) }
  | L_PRN ply R_PRN { $2 }
  ;

{
module Sysgenparse where
import Sysgenlex
import Treedef
import Treepr
}

%name sysgenparse
%tokentype { Token }

-- %error { parseError }

%token
  '{'					{ TLB _ }
  '}'					{ TRB _ }
  ';'					{ TSemi _ }
  '.'					{ TDot _ }
  "->"					{ TRarrow _ }

  vcenter				{ TVcenter _ }
  esxhost				{ TEsxhost _ }
  segment				{ TSegment _ }
  switch				{ TSwitch _ }
  firewall				{ TFirewall _ }
  service				{ TService _}
  node					{ TNode _ }
  nic					{ TNic _ }
  interface				{ TInterface _ }
  zone					{ TZone _ }
  acl					{ TAcl _  }
  snat					{ TSnat _ }
  dnat					{ TDnat _ }
  policy				{ TPolicy _ }
  route					{ TRoute _ }
  init					{ TInit _ }
  platform				{ TPlatform _ }
  adxobj				{ TAdxobj _ }
  kemplb				{ TKemp _ }

  intlit				{ TIntLiteral _ $$ }
  ident		                        { TIdent _ $$ }
  strlit				{ TStringLiteral _ $$ }
%%

blocks : blocks block			{ BlockList $1 $2 }
       | block				{ BlockList BEmpty $1 }

block : vcenter ident '{' cnstrs '}'	{ VBlock $2 $4 }
      | esxhost ident '{' cnstrs '}'	{ EBlock $2 $4 }
      | segment ident '{' cnstrs '}'	{ SBlock $2 $4 }
      | switch ident '{' cnstrs ifaces iroute '}' {SWBlock $2 $4 $5 $6 }
      | firewall ident '{'
         cnstrs ifaces fblocks iroute '}' { FBlock $2 $4 $5 $6 $7 }
      | firewall ident '{'
         cnstrs ifaces iroute '}' { FBlock1 $2 $4 $5 $6 }
      | node ident '{' cnstrs inics '}'	   { NBlock $2 $4 $5 }
      | node ident '{' cnstrs inics insec '}'	{ NBlock1 $2 $4 $5 $6}
      | node ident '{' cnstrs inics pform '}'	{ NBlock2 $2 $4 $5 $6}
      | kemplb ident '{' cnstrs lblocks iroute '}' { KBlock $2 $4 $5 $6}

cnstrs : cnstrs cnstr			{ CnstrList $1 $2 }
       | cnstr				{ CnstrList Empty $1 }

cnstr : ident ident ';'			{ Cnstr1 $1 $2 }
      | ident intlit ';'		{ Cnstr2 $1 $2 }
      | ident strlit ';'                { Cnstr3 $1 $2 }

ifaces : ifaces iface 			{ IFaceList $1 $2 }
       | iface				{ IFaceList IFaceEmpty $1 }

iface : interface intlit '{' cnstrs '}' { IFace1 $2 $4 }

iroute : route '{' cnstrs '}'		{ IRoute1 $3 }

pform : platform '{' cnstrs '}'		{ IPlatform1 $3 }

fblocks : fblocks fblock		{ FBlockList $1 $2 }
        | fblock			{ FBlockList FBEmpty $1 }

fblock : zone ident '{' cnstrs '}'	{ IZone1 $2 $4 }
       | acl ident '{' cnstrs '}'	{ IAcl1 $2 $4 }
       | policy ident '{' cnstrs '}'	{ IPolicy1 $2 $4 }
       | dnat ident '{' cnstrs '}'	{ IDnat1 $2 $4 }
       | snat ident '{' cnstrs '}'	{ ISnat1 $2 $4 }
       | service ident '{' cnstrs '}'	{ IServ1 $2 $4 }
       | adxobj ident '{' cnstrs '}'	{ IAdxobj1 $2 $4 }

lblocks : lblocks lblock		{ LBlockList $1 $2 }
	| lblock			{ LBlockList LBEmpty $1 }

lblock : interface intlit '{' cnstrs '}' { IFace2 $2 $4 }
       | service ident  '{' cnstrs '}'	 { IServ2 $2 $4 }

inics : inics inic			{ INicList $1 $2 }
      | inic				{ INicList INicEmpty $1}

inic : nic intlit '{' cnstrs '}'	{ INic1 $2 $4 }

insec : init '{' cnstrs '}'		{ IInsec1 $3 }

{

happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
 where
  lcn = case tks of
         [] -> "eof"
         tk:_ -> "line " ++ show l ++ ", column " ++ show c
          where
           AlexPn _ l c = token_posn tk
}

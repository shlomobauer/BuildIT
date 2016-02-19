
{
module Sysgenlex (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$graphic    = $printable # $white
@string     = $graphic | " "

tokens :-
  $white+				;
  
  "#".*					;

  "{"		{ tok (\p s -> TLB p) }
  "}"		{ tok (\p s -> TRB p) }
  ";" 		{ tok (\p s -> TSemi p) }
  "." 		{ tok (\p s -> TDot p) }
  "->"		{ tok (\p s -> TRarrow p) }

  "vcenter"	{ tok (\p s -> TVcenter p) }
  "esxhost"	{ tok (\p s -> TEsxhost p) }
  "segment"	{ tok (\p s -> TSegment p) }
  "switch"	{ tok (\p s -> TSwitch p) }
  "firewall"	{ tok (\p s -> TFirewall p) }
  "service"	{ tok (\p s -> TService p) }
  "node"	{ tok (\p s -> TNode p) }
  "nic"		{ tok (\p s -> TNic p) }
  "interface"	{ tok (\p s -> TInterface p) }
  "zone"	{ tok (\p s -> TZone p) }
  "acl"		{ tok (\p s -> TAcl p) }
  "snat"	{ tok (\p s -> TSnat p) }
  "dnat"	{ tok (\p s -> TDnat p) }
  "policy"	{ tok (\p s -> TPolicy p) }
  "route"	{ tok (\p s -> TRoute p) }
  "init"	{ tok (\p s -> TInit p) }
  "platform"	{ tok (\p s -> TPlatform p) }
  "adxobj"	{ tok (\p s -> TAdxobj p) }
  "kemplb"	{ tok (\p s -> TKemp p) }

  $digit+			{ tok (\p s -> TIntLiteral p (read s)) }
  $alpha[$alpha $digit \_ \']*	{ tok (\p s -> TIdent p s) }
  
  \" @string* \"	{ tok (\p s -> TStringLiteral p (init (tail s))) }
{
-- Each action has type :: AlexPosn -> String -> Token

-- a "helper" to improve readability
tok f p s = f p s

-- The token type:
data Token =
  TLB AlexPosn             |
  TRB AlexPosn             |
  TDot AlexPosn            |
  TRarrow AlexPosn         |
  TVcenter AlexPosn        |
  TEsxhost AlexPosn        |
  TSwitch AlexPosn         |
  TSegment AlexPosn        |
  TFirewall AlexPosn       |
  TService AlexPosn        | 
  TNode AlexPosn           |
  TNic AlexPosn            |
  TInterface AlexPosn      |
  TZone AlexPosn           |
  TAcl AlexPosn            |
  TSnat AlexPosn           |
  TDnat AlexPosn           |
  TPolicy AlexPosn         |
  TRoute AlexPosn          |
  TInit AlexPosn           |
  TPlatform AlexPosn       |
  TAdxobj AlexPosn         |
  TKemp AlexPosn           |
  TSemi AlexPosn           |
  TIdent AlexPosn String   |
  TIntLiteral AlexPosn Int |
  TStringLiteral AlexPosn String
  deriving (Eq,Show)

token_posn (TLB p) = p
token_posn (TRB p) = p
token_posn (TDot p) = p
token_posn (TRarrow p) = p
token_posn (TVcenter p) = p
token_posn (TEsxhost p) = p
token_posn (TSwitch p) = p
token_posn (TSegment p) = p
token_posn (TFirewall p) = p
token_posn (TService p) = p
token_posn (TNode p) = p
token_posn (TNic p) = p
token_posn (TInterface p) = p
token_posn (TZone p) = p
token_posn (TAcl p) = p
token_posn (TSnat p) = p
token_posn (TDnat p) = p
token_posn (TPolicy p) = p
token_posn (TRoute p) = p
token_posn (TInit p) = p
token_posn (TPlatform p) = p
token_posn (TAdxobj p) = p
token_posn (TKemp p) = p
token_posn (TSemi p) = p
token_posn (TIdent p _) = p
token_posn (TIntLiteral p _) = p
token_posn (TStringLiteral p _) = p
}

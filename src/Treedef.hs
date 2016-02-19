module Treedef where

data BlockList
 = BlockList BlockList Block
 | BEmpty
 deriving (Show, Eq)

data Block
 = VBlock String CnstrList
 | EBlock String CnstrList
 | SBlock String CnstrList
 | SWBlock String CnstrList IFaceList IRoute
 | FBlock String CnstrList IFaceList FBlockList IRoute
 | FBlock1 String CnstrList IFaceList IRoute
 | NBlock String CnstrList INicList
 | NBlock1 String CnstrList INicList IInsec
 | NBlock2 String CnstrList INicList IPlatform
 | KBlock String CnstrList LBlockList IRoute
 deriving (Show, Eq) 

data FBlockList
 = FBlockList FBlockList FBlock
 | FBEmpty
 deriving (Show, Eq)

data FBlock
 = IZone1 String CnstrList
 | IAcl1 String CnstrList
 | IPolicy1 String CnstrList
 | IDnat1 String CnstrList
 | IServ1 String CnstrList
 | ISnat1 String CnstrList
 | IAdxobj1 String CnstrList
 deriving (Show, Eq)

data LBlockList
 = LBlockList LBlockList LBlock
 | LBEmpty
 deriving (Show, Eq)

data LBlock
 = IFace2 Int CnstrList
 | IServ2 String CnstrList
 deriving (Show, Eq)
 
data IRoute
 = IRoute1 CnstrList
 deriving (Show, Eq)

data IPlatform
 = IPlatform1 CnstrList
 deriving (Show, Eq)

data IFaceList
 = IFaceList IFaceList IFace
 | IFaceEmpty
 deriving (Show, Eq)

data IFace
 = IFace1 Int CnstrList
 deriving (Show, Eq)

data INicList
 = INicList INicList INic
 | INicEmpty
 deriving (Show, Eq)

data INic
 = INic1 Int CnstrList
 deriving (Show, Eq)

data IInsec
 = IInsec1 CnstrList
 deriving (Show, Eq)

data CnstrList
 = CnstrList CnstrList Cnstr
 | Empty
 deriving (Show, Eq)

data Cnstr
 = Cnstr1 String String
 | Cnstr2 String Int
 | Cnstr3 String String
 deriving (Show, Eq)

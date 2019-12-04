module Treepr where
import Treedef

-- pretty print block list
ptreebl (BlockList BEmpty b) = ptreeb b

ptreebl (BlockList a b) = do
 ptreebl a 
 ptreeb b

-- pretty print load balance block
ptreelbl i (LBlockList LBEmpty b) = ptreelb i b

ptreelbl i (LBlockList a b) = do
 ptreelbl i a
 ptreelb i b

-- pretty print firewall block
ptreefbl i (FBlockList FBEmpty b) = ptreefb i b

ptreefbl i (FBlockList a b) = do
 ptreefbl i a
 ptreefb i b

-- pretty print interface list
ptreefl i (IFaceList IFaceEmpty b) = ptreef i b

ptreefl i (IFaceList a b) = do
 ptreefl i a
 ptreef i b

-- pretty print nic list
ptreenbl i (INicList INicEmpty b) = ptreenb i b

ptreenbl i (INicList a b) = do
 ptreenbl i a
 ptreenb i b

-- pretty print vcenter block
ptreeb (VBlock a b) = let z = "vcenter " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b

-- pretty print esxhost block
ptreeb (EBlock a b) = let z = "esxhost " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b

-- pretty print segment block
ptreeb (SBlock a b) = let z = "segment " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b

-- pretty print switch block
ptreeb (SWBlock a b c d) = let z = "switch " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b
  ptreefl 1 c
  ptreer 1 d

-- FBlock String CnstrList IFaceList FBlockList IRoute
ptreeb (FBlock a b c d e) = let z = "firewall " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b
  ptreefl 1 c
  ptreefbl 1 d
  ptreer 1 e

-- FBlock1 String CnstrList IFaceList IRoute
ptreeb (FBlock1 a b c e) = let z = "firewall " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b
  ptreefl 1 c
  ptreer 1 e

-- NBlock String CnstrList INicList
ptreeb (NBlock a b c) = let z = "node " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b
  ptreenbl 1 c

-- NBlock1 String CnstrList INicList IInsec
ptreeb (NBlock1 a b c d) = let z = "node " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b
  ptreenbl 1 c
  ptreein 1 d

-- NBlock2 String CnstrList INicList IPlatform
ptreeb (NBlock2 a b c d) = let z = "node " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b
  ptreenbl 1 c
  ptreepform 1 d

-- KBlock String CnstrList LBlockList IRoute
ptreeb (KBlock a b c d) = let z = "kempLB " ++ a in
 do
  putStrLn ""
  putStrLn z
  ptreel 1 b
  ptreelbl 1 c
  ptreer 1 d

-- pretty print nic 
ptreenb i (INic1 a b) = let z = (spaces i) ++ "nic " ++ (show a) in
 do
  putStrLn z 
  ptreel (i+1) b

-- pretty print interface
ptreef i (IFace1 a b) = let z = (spaces i) ++ "interface " ++ (show a) in
 do
  putStrLn z 
  ptreel (i+1) b

-- pretty print route
ptreer i (IRoute1 a) = let z = (spaces i) ++ "route " in
 do
  putStrLn z
  ptreel (i+1) a

-- pretty print init
ptreein i (IInsec1 a) = let z = (spaces i) ++ "init " in
 do
  putStrLn z
  ptreel (i+1) a

-- pretty print platform
ptreepform i (IPlatform1 a) = let z = (spaces i) ++ "platform " in
 do
  putStrLn z
  ptreel (i+1) a

-- IZone1 String CnstrList
ptreefb i (IZone1 a b) = let z = (spaces i) ++ "zone " ++ a in
 do
  putStrLn z
  ptreel (i+1) b

-- IAcl1 String CnstrList
ptreefb i (IAcl1 a b) = let z = (spaces i) ++ "acl " ++ a in
 do
  putStrLn z
  ptreel (i+1) b

-- IPolicy1 String CnstrList
ptreefb i (IPolicy1 a b) = let z = (spaces i) ++ "policy " ++ a in
 do
  putStrLn z
  ptreel (i+1) b

-- IDnat1 String CnstrList
ptreefb i (IDnat1 a b) = let z = (spaces i) ++ "dnat " ++ a in
 do
  putStrLn z
  ptreel (i+1) b

-- IServ1 String CnstrList
ptreefb i (IServ1 a b) = let z = (spaces i) ++ "service " ++ a in
 do
  putStrLn z
  ptreel (i+1) b

-- ISnat1 String CnstrList
ptreefb i (ISnat1 a b) = let z = (spaces i) ++ "snat " ++ a in
 do
  putStrLn z
  ptreel (i+1) b

-- IAdxobj1 String CnstrList
ptreefb i (IAdxobj1 a b) = let z = (spaces i) ++ "adxobj " ++ a in
 do
  putStrLn z
  ptreel (i+1) b

-- IFace2 Int CnstrList
ptreelb i (IFace2 a b) = let z = (spaces i) ++ "interface " ++ (show a) in
 do
  putStrLn z
  ptreel (i+1) b

-- IServ2 String CnstrList
ptreelb i (IServ2 a b) = let z = (spaces i) ++ "service " ++ a in
 do
  putStrLn z
  ptreel (i+1) b

-- pretty print constraint list
ptreel i (CnstrList Empty b) = ptree i b
ptreel i (CnstrList a b) = do
 ptreel i a 
 ptree i b

-- pretty print constraints
ptree i (Cnstr1 b c) = putStrLn ((spaces i) ++ b ++ " : " ++ c)
ptree i (Cnstr2 b c) = putStrLn ((spaces i) ++ b ++ " : " ++ (show c ))
ptree i (Cnstr3 b c) = putStrLn ((spaces i) ++b ++ " : \"" ++ c ++ "\"")

-- spaces for formatting
spaces i = let space = " " ++ space in take i space

module Main where
import Sysgenparse
import Sysgenlex
import Treedef
import Treepr
import OutClean
import OutBuild
import OutFwXML

import System.Environment
import System.IO

main = do 
 args <- getArgs
 let fname = head args
 inh <- openFile fname ReadMode
 inStr <- hGetContents inh
 -- print (alexScanTokens inStr)
 -- putStrLn("\n\n")
 let parseTree =  sysgenparse (alexScanTokens inStr)
 -- print parseTree
 -- ptreebl parseTree
 buildit parseTree
 cleanit parseTree
 genfwxml parseTree
 hClose inh

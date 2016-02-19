module Util where
import Treedef

import System.IO
import System.Exit
import Data.Maybe

findCNSL key (CnstrList Empty b) = findCNS key b

findCNSL key (CnstrList a b) = 
 let z = findCNS key b in
   (if z == Nothing
     then findCNSL key a
   else z)

findCNS key (Cnstr1 b c)
 | key == b = Just c
 | otherwise =  Nothing

findCNS key (Cnstr2 b c)
 | key == b = Just (show c)
 | otherwise =  Nothing

findCNS key (Cnstr3 b c)
 | key == b = Just c
 | otherwise =  Nothing


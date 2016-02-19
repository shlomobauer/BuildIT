module OutClean where
import Treedef
import Util

import System.IO
import System.Exit
import Data.Maybe

------------------------------
-- cleanit
------------------------------

cleanit parseTree = do
 h <- openCleanOut
 prologue h
 cleanServers h parseTree
 cleanSegments h parseTree
 cleanConnect h
 cleanStopServers h parseTree
 cleanRemoveServers h parseTree
 cleanRemoveSegments h parseTree
 closeCleanOut h

------------------------------
-- functions called by cleanup
------------------------------

openCleanOut = openFile "cleanup.ps1" WriteMode

prologue h = do
 writeCleanOut h "# add power cli library"
 writeCleanOut h "Add-PSSnapin VMWare.VimAutomation.Core"

cleanServers h tree = do
 writeCleanOut h ""
 writeCleanOut h "#------------------------------"
 writeCleanOut h "# virtual servers to be removed"
 writeCleanOut h "#------------------------------"
 writeCleanOut h ""
 ocleanvbl h tree

cleanSegments h tree = do
 writeCleanOut h ""
 writeCleanOut h "#---------------------------------------"
 writeCleanOut h "# virtual network segments to be removed"
 writeCleanOut h "#---------------------------------------"
 writeCleanOut h ""
 ocleansbl h tree

cleanConnect h = do
 writeCleanOut h ""
 writeCleanOut h "#-------------------"
 writeCleanOut h "# connect to vcenter"
 writeCleanOut h "#-------------------"
 writeCleanOut h ""
 writeCleanOut h "Connect-VIServer -Server $vcenter -User $vcenteruser -Password $vcenterpass"

cleanStopServers h tree = do
 writeCleanOut h ""
 writeCleanOut h "#---------------------"
 writeCleanOut h "# stop virtual servers"
 writeCleanOut h "#---------------------"
 writeCleanOut h ""
 ocleansvbl h tree

cleanRemoveServers h tree = do
 writeCleanOut h ""
 writeCleanOut h "#-----------------------"
 writeCleanOut h "# remove virtual servers"
 writeCleanOut h "#-----------------------"
 writeCleanOut h ""
 ocleanrvbl h tree

cleanRemoveSegments h tree = do
 writeCleanOut h ""
 writeCleanOut h "#------------------------"
 writeCleanOut h "# remove virtual segments"
 writeCleanOut h "#------------------------"
 writeCleanOut h ""
 ocleanrsbl h tree

closeCleanOut h = hClose h

writeCleanOut h s = hPutStrLn h s

-----------------------
-- supporting functions
-----------------------

---------------------------------
-- network segments to be removed
---------------------------------

ocleansbl h (BlockList BEmpty b) = ocleansb h b

ocleansbl h (BlockList a b) = do
 ocleansbl h a
 ocleansb h b 

ocleansb h (SBlock a b) =
 writeCleanOut h ("$" ++ a ++ "_name = \"" ++ a ++ "\"")

ocleansb h _ = do
 return ()

-----------------------
-- servers to be removed
-----------------------

ocleanvbl h (BlockList BEmpty b) = ocleanvb h b

ocleanvbl h (BlockList a b) = do
 ocleanvbl h a
 ocleanvb h b 

ocleanvb h (SWBlock a b c d) = let z = findCNSL "name" b in
 if z == Nothing
 then
  do
   putStrLn ("Cleanup: Switch " ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeCleanOut h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

ocleanvb h (FBlock a b c d e) = let z = findCNSL "name" b in
 if z == Nothing
 then
  do
   putStrLn ("Cleanup: Firewall " ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeCleanOut h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

ocleanvb h (FBlock1 a b c d) = let z = findCNSL "name" b in
 if z == Nothing
 then
  do
   putStrLn ("Cleanup: Firewall " ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeCleanOut h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

ocleanvb h (NBlock a b c) = let z = findCNSL "name" b in
 if z == Nothing
 then
  do
   putStrLn ("Cleanup: Node " ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeCleanOut h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

ocleanvb h (NBlock1 a b c d) = let z = findCNSL "name" b in
 if z == Nothing
 then
  do
   putStrLn ("Cleanup: Node " ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeCleanOut h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

ocleanvb h (NBlock2 a b c d) = let z = findCNSL "name" b in
 if z == Nothing
 then
  do
   putStrLn ("Cleanup: Node " ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeCleanOut h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

ocleanvb h (KBlock a b c d) = let z = findCNSL "name" b in
 if z == Nothing
 then
  do
   putStrLn ("Cleanup: Kemp LB " ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeCleanOut h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

ocleanvb h _ = do
 return ()

---------------
-- stop servers
---------------

ocleansvbl h (BlockList BEmpty b) = ocleansvb h b

ocleansvbl h (BlockList a b) = do
 ocleansvbl h a
 ocleansvb h b 

ocleansvb h (SWBlock a b c d) =
 writeCleanOut h ("Stop-VM $" ++ a ++ "_name -Confirm:$false")

ocleansvb h (FBlock a b c d e) =
 writeCleanOut h ("Stop-VM $" ++ a ++ "_name -Confirm:$false")

ocleansvb h (FBlock1 a b c d) =
 writeCleanOut h ("Stop-VM $" ++ a ++ "_name -Confirm:$false")

ocleansvb h (NBlock a b c) =
 writeCleanOut h ("Stop-VM $" ++ a ++ "_name -Confirm:$false")

ocleansvb h (NBlock1 a b c d) =
 writeCleanOut h ("Stop-VM $" ++ a ++ "_name -Confirm:$false")

ocleansvb h (NBlock2 a b c d) =
 writeCleanOut h ("Stop-VM $" ++ a ++ "_name -Confirm:$false")

ocleansvb h (KBlock a b c d) =
 writeCleanOut h ("Stop-VM $" ++ a ++ "_name -Confirm:$false")

ocleansvb h _ = do
 return ()

-----------------
-- remove servers
-----------------

ocleanrvbl h (BlockList BEmpty b) = ocleanrvb h b

ocleanrvbl h (BlockList a b) = do
 ocleanrvbl h a
 ocleanrvb h b 

ocleanrvb h (SWBlock a b c d) =
 writeCleanOut h ("Remove-VM -VM $" ++ a ++ "_name -Confirm:$false -DeletePermanently:$true")

ocleanrvb h (FBlock a b c d e) =
 writeCleanOut h ("Remove-VM -VM $" ++ a ++ "_name -Confirm:$false -DeletePermanently:$true")

ocleanrvb h (FBlock1 a b c d) =
 writeCleanOut h ("Remove-VM -VM $" ++ a ++ "_name -Confirm:$false -DeletePermanently:$true")

ocleanrvb h (NBlock a b c) =
 writeCleanOut h ("Remove-VM -VM $" ++ a ++ "_name -Confirm:$false -DeletePermanently:$true")

ocleanrvb h (NBlock1 a b c d) =
 writeCleanOut h ("Remove-VM -VM $" ++ a ++ "_name -Confirm:$false -DeletePermanently:$true")

ocleanrvb h (NBlock2 a b c d) =
 writeCleanOut h ("Remove-VM -VM $" ++ a ++ "_name -Confirm:$false -DeletePermanently:$true")

ocleanrvb h (KBlock a b c d) =
 writeCleanOut h ("Remove-VM -VM $" ++ a ++ "_name -Confirm:$false -DeletePermanently:$true")

ocleanrvb h _ = do
 return ()

--------------------------
-- remove network segments
--------------------------

ocleanrsbl h (BlockList BEmpty b) = ocleanrsb h b

ocleanrsbl h (BlockList a b) = do
 ocleanrsbl h a
 ocleanrsb h b 

ocleanrsb h (SBlock a b) =
 writeCleanOut h ("Remove-VirtualSwitch -VirtualSwitch $" ++ a ++ "_name -Confirm:$false")

ocleanrsb h _ = do
 return ()


module OutBuild where
import Treedef
import Util

import System.IO
import System.Exit
import Data.Maybe
import Data.List.Split

import Control.Monad

------------------------------
-- build
------------------------------
buildit parseTree = do
 h <- openBuildIT
 prologue h parseTree
 buildVCenterCred h parseTree
 buildSegments h parseTree
 buildConnect h
 buildESXSwitches h parseTree
 buildSwitches h parseTree
 buildFirewalls h parseTree
 buildLoadBalancers h parseTree
 buildNodes h parseTree
 closeBuildIT h

------------------------------
-- functions called by buildit
------------------------------

openBuildIT = openFile "buildscript.ps1" WriteMode

closeBuildIT h = hClose h

prologue h tree = do
 if anyKBlockl tree == True
 then
  do
   writeBuildIT h "# module for Kemp Load Balancer"
   writeBuildIT h ("Import-Module -Name \"C:\\Windows" ++
                   "\\System32\\WindowsPowerShell\\v1.0" ++
                   "\\Modules\\KEMP\\Kemp.LoadBalancer.Powershell.psd1\"")
   writeBuildIT h ""
 else
  return ()

 writeBuildIT h "# vmware power cli"
 writeBuildIT h "Add-PSSnapin VMWare.VimAutomation.Core"

buildVCenterCred h tree = do
 writeBuildIT h ""
 writeBuildIT h "#-----------------------------"
 writeBuildIT h "# vcenter server & credentials"
 writeBuildIT h "#-----------------------------"
 writeBuildIT h ""
 obuildvcredbl h tree
 obuildecredbl h tree

buildSegments h tree = do
 writeBuildIT h ""
 writeBuildIT h "#------------------------------------------"
 writeBuildIT h "# virtual network segments to be configured"
 writeBuildIT h "#------------------------------------------"
 writeBuildIT h ""
 obuildsbl h tree

buildConnect h = do
 writeBuildIT h ""
 writeBuildIT h "#-------------------"
 writeBuildIT h "# connect to vcenter"
 writeBuildIT h "#-------------------"
 writeBuildIT h ""
 writeBuildIT h "Connect-VIServer -Server $vcenter -User $vcenteruser -Password $vcenterpass"

buildESXSwitches h tree = do
 buildVSwitches h tree
 buildPortGroups h tree
 buildPortSecurity h tree

buildVSwitches h tree = do
 writeBuildIT h ""
 writeBuildIT h "#-----------------------------------"
 writeBuildIT h "# make virtual switches for segments"
 writeBuildIT h "#-----------------------------------"
 writeBuildIT h ""
 obuildvsbl h tree

buildPortGroups h tree = do
 writeBuildIT h ""
 writeBuildIT h "#------------------------------"
 writeBuildIT h "# make port groups for segments"
 writeBuildIT h "#------------------------------"
 writeBuildIT h ""
 obuildpsbl h tree
 
buildPortSecurity h tree = do
 writeBuildIT h ""
 writeBuildIT h "#---------------------------------"
 writeBuildIT h "# make port groups security policy"
 writeBuildIT h "#---------------------------------"
 writeBuildIT h ""
 obuildssbl h tree

buildSwitches h tree = do
 writeBuildIT h ""
 writeBuildIT h "#---------------"
 writeBuildIT h "# Build Switches"
 writeBuildIT h "#---------------"
 obuildswitchbl h tree

buildFirewalls h tree = do
 writeBuildIT h ""
 writeBuildIT h "#----------------"
 writeBuildIT h "# Build Firewalls"
 writeBuildIT h "#----------------"
 obuildfirewallbl h tree

buildLoadBalancers h tree = do
 writeBuildIT h ""
 writeBuildIT h "#---------------------"
 writeBuildIT h "# Build Load Balancers"
 writeBuildIT h "#---------------------"
 obuildloadbalancebl h tree

buildNodes h tree = do
 writeBuildIT h ""
 writeBuildIT h "#------------"
 writeBuildIT h "# Build Nodes"
 writeBuildIT h "#------------"
 obuildnodebl h tree
 
-----------------------
-- supporting functions
-----------------------

---------------------------------
-- vcenter & esxhost credentials
---------------------------------

-- vcenter
obuildvcredbl h (BlockList BEmpty b) = obuildvcredb h b

obuildvcredbl h (BlockList a b) = do
 obuildvcredbl h a
 obuildvcredb h b 

obuildvcredb h (VBlock a b) = do
 let z = findCNSL "description" b
 if z == Nothing
 then
  do
   return ()
 else
  do
   let y = fromJust z
   writeBuildIT h ("# vcenter " ++ a ++ ":  \"" ++ y ++ "\"")
   writeBuildIT h ""

 let z = findCNSL "ip" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: VCenter " ++ a ++ " has no ip adx")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$vcenter = \"" ++ y ++ "\"")

 let z = findCNSL "user" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: VCenter " ++ a ++ " has no user")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$vcenteruser = \"" ++ y ++ "\"")

 let z = findCNSL "pswd" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: VCenter " ++ a ++ " has no pswd")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$vcenterpass = \"" ++ y ++ "\"")

 writeBuildIT h ""

obuildvcredb h _ = do
 return ()

-- esxhost
obuildecredbl h (BlockList BEmpty b) = obuildecredb h b

obuildecredbl h (BlockList a b) = do
 obuildecredbl h a
 obuildecredb h b 

obuildecredb h (EBlock a b) = do
 let z = findCNSL "description" b
 if z == Nothing
 then
  do
   return ()
 else
  do
   let y = fromJust z
   writeBuildIT h ("# esx host " ++ a ++ ":  \"" ++ y ++ "\"")
   writeBuildIT h ""

 let z = findCNSL "ip" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: ESX Host " ++ a ++ " has no ip adx")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$esxhost = \"" ++ y ++ "\"")

 let z = findCNSL "user" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: ESX Host " ++ a ++ " has no user")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$esxuser = \"" ++ y ++ "\"")

 let z = findCNSL "pswd" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: ESX Host " ++ a ++ " has no pswd")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$esxpass = \"" ++ y ++ "\"")

obuildecredb h _ = do
 return ()

-- virtual network segments
obuildsbl h (BlockList BEmpty b) = obuildsb h b

obuildsbl h (BlockList a b) = do
 obuildsbl h a
 obuildsb h b 

obuildsb h (SBlock a b) =
 writeBuildIT h ("$" ++ a ++ " = \"" ++ a ++ "\"")

obuildsb h _ = do
 return ()

-- make virtual switches
obuildvsbl h (BlockList BEmpty b) = obuildvsb h b

obuildvsbl h (BlockList a b) = do
 obuildvsbl h a
 obuildvsb h b 

obuildvsb h (SBlock a b) = do
 writeBuildIT h ("New-VirtualSwitch -VMHost $esxhost -Name $" ++ a)

obuildvsb h _ = do
 return ()

-- make port groups
obuildpsbl h (BlockList BEmpty b) = obuildpsb h b

obuildpsbl h (BlockList a b) = do
 obuildpsbl h a
 obuildpsb h b 

obuildpsb h (SBlock a b) = do
 writeBuildIT h ("New-VirtualPortGroup -Name $" ++ a ++ " -VirtualSwitch $" ++ a)

obuildpsb h _ = do
 return ()

-- make port security
obuildssbl h (BlockList BEmpty b) = obuildssb h b

obuildssbl h (BlockList a b) = do
 obuildssbl h a
 obuildssb h b 

obuildssb h (SBlock a b) = do
 writeBuildIT h ("Get-SecurityPolicy -VirtualPortGroup $" ++ a ++ " |")
 writeBuildIT h ("  Set-SecurityPolicy -AllowPromiscuous:$true")

obuildssb h _ = do
 return ()

-- build switches
obuildswitchbl h (BlockList BEmpty b) = obuildswitchb h b

obuildswitchbl h (BlockList a b) = do
 obuildswitchbl h a
 obuildswitchb h b

-- (SWBlock String CnstrList IFaceList IRoute)
obuildswitchb h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "#--------------------------------"
 writeBuildIT h ("# Build Switch " ++ a)
 writeBuildIT h "#--------------------------------"
 switchCred h (SWBlock a b c d)
 passConv h a
 vmMake h a
 switchConfig h (SWBlock a b c d)
 switchPower h (SWBlock a b c d)
 switchArpCache h (SWBlock a b c d)
 switchSetVlans h (SWBlock a b c d)
 switchConfigIntfs h (SWBlock a b c d)
 switchConfigVlans h (SWBlock a b c d)
 switchConfigRoutes h (SWBlock a b c d)
 switchConfigMgmt h (SWBlock a b c d)
 switchSetMgmt h (SWBlock a b c d)

obuildswitchb h _ = do
 return ()

----------------------------------

switchCred h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "# variables used to build switch"
 writeBuildIT h ""
 let z = findCNSL "template" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Switch " ++ a ++ " has no template")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_template = \"" ++ y ++ "\"")

 let z = findCNSL "url" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Switch " ++ a ++ " has no url")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_url = \"" ++ y ++ "\"")

 let z = findCNSL "user" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Switch " ++ a ++ " has no user")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_user = \"" ++ y ++ "\"")

 let z = findCNSL "pswd" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Switch " ++ a ++ " has no pswd")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_pass = \"" ++ y ++ "\"")

 let z = findCNSL "name" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Switch " ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

----------------------------------

firewallCred h a b c = do
 writeBuildIT h ""
 writeBuildIT h "# variables used to build firewall"
 writeBuildIT h ""
 let z = findCNSL "template" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: " ++ c ++ a ++ " has no template")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_template = \"" ++ y ++ "\"")

 writeBuildIT h ""

 let z = findCNSL "url" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: " ++ c ++ a ++ " has no url")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_url = \"" ++ y ++ "\"")

 let z = findCNSL "user" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: " ++ c ++ a ++ " has no user")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_user = \"" ++ y ++ "\"")

 let z = findCNSL "pswd" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: " ++ c ++ a ++ " has no pswd")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_pswd = \"" ++ y ++ "\"")

 let z = findCNSL "name" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: " ++ c ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

 let z = findCNSL "auth" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: " ++ c ++ a ++ " has no auth code")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_code = \"" ++ y ++ "\"")

 let z = findCNSL "file" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: " ++ c ++ a ++ " has no file name")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_file = \"" ++ y ++ "\"")

 let z = findCNSL "path" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: " ++ c ++ a ++ " has no path")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_path = \"" ++ y ++ "\"")

---------------------------

passConv h a = do
 writeBuildIT h ""
 writeBuildIT h "# user/password to secure strings"
 writeBuildIT h ""
 writeBuildIT h ("$" ++ a ++ "_converted_pass = ConvertTo-SecureString \"$" ++ a ++ "_pass\" -AsPlainText -Force")

 writeBuildIT h ("$" ++ a ++ "_converted_cred = New-Object System.Management.Automation.PSCredential(\"$" ++ a ++ "_user\",$" ++ a ++ "_converted_pass)")

-------------------------------

vmMake h a = do
 writeBuildIT h ""
 writeBuildIT h "# create VM from template"
 writeBuildIT h ""
 writeBuildIT h ("New-VM -Template $" ++ a ++ "_Template -Name $" ++ a ++ "_name -VMHost $esxhost")

-------------------------------

switchConfig h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "# set up adapters for switch"
 writeBuildIT h ""
 switchInfbl h a c

-------------------------------

switchInfbl h a (IFaceList IFaceEmpty c) = switchInfb h a c

switchInfbl h a (IFaceList b c) = do
 switchInfbl h a b
 switchInfb h a c

-- IFace1 Int CnstrList
switchInfb h a (IFace1 b c) = do
 if b == 0
 then
  do
   return ()
 else
  do
   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Switch " ++ a ++ " Interface " ++ (show b) ++
      " network segment missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("New-NetworkAdapter -VM $" ++ a ++ 
      "_name -Type E1000 -NetworkName $" ++ y ++ " -StartConnected:$true" ++
      " -Confirm:$false")

-----------------

switchPower h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "# start switch and wait for it to be ready"
 writeBuildIT h ""
 writeBuildIT h ("Start-VM -VM $" ++ a ++ "_name")
 writeBuildIT h "Start-Sleep -s 60"

-----------------

switchArpCache h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "# disable persistent arp cache"
 writeBuildIT h ""
 writeBuildIT h "$arpJson = @\""
 writeBuildIT h "{"
 writeBuildIT h " \"jsonrpc\":\"2.0\","
 writeBuildIT h " \"method\":\"runCmds\","
 writeBuildIT h " \"params\": {"
 writeBuildIT h "     \"version\": 1,"
 writeBuildIT h "     \"cmds\": ["
 writeBuildIT h "         \"enable\","
 writeBuildIT h "         \"configure terminal\","
 writeBuildIT h "           \"no arp cache persistent\","
 writeBuildIT h "         \"write memory\""
 writeBuildIT h "       ],"
 writeBuildIT h "     \"format\":\"text\","
 writeBuildIT h "     \"timestamps\": \"false\""
 writeBuildIT h "   },"
 writeBuildIT h " \"id\": \"BuildIT\""
 writeBuildIT h "}"
 writeBuildIT h "\"@"
 writeBuildIT h ""

 writeBuildIT h ("$arp_response = Invoke-WebRequest -Method Post -Uri $" ++
                  a ++ "_url -Body $arpJson -Credential $" ++ a ++
                  "_converted_cred")

-------------------------

switchSetVlans h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "# setup switch vlans"
 writeBuildIT h ""
 writeBuildIT h "$VLANJson = @\""
 writeBuildIT h "{"
 writeBuildIT h "    \"jsonrpc\":\"2.0\","
 writeBuildIT h "    \"method\":\"runCmds\","
 writeBuildIT h "    \"params\": {"
 writeBuildIT h "        \"version\": 1,"
 writeBuildIT h "        \"cmds\": ["
 writeBuildIT h "            \"enable\","
 writeBuildIT h "            \"configure terminal\","

 switchInfCVlanbl h a c

 writeBuildIT h "            \"write memory\""
 writeBuildIT h "        ],"
 writeBuildIT h "    \"format\":\"text\","
 writeBuildIT h "    \"timestamps\": \"false\""
 writeBuildIT h "    },"
 writeBuildIT h "    \"id\": \"BuildIT\""
 writeBuildIT h "}"
 writeBuildIT h "\"@"
 writeBuildIT h ""

 writeBuildIT h ("$vlan_response = Invoke-WebRequest -Method Post -Uri $" ++
                  a ++ "_url -Body $VLANJson -Credential $" ++ a ++
                  "_converted_cred")

----------------

switchInfCVlanbl h a (IFaceList IFaceEmpty c) = switchInfCVlanb h a c

switchInfCVlanbl h a (IFaceList b c) = do
 switchInfCVlanbl h a b
 switchInfCVlanb h a c

switchInfCVlanb h a (IFace1 b c) = do
 if b == 0
 then
  do
   return ()
 else
  do
   let z = findCNSL "vlan" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Switch " ++ a ++ " Interface " ++ (show b) ++
      " vlan missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("              \"vlan " ++ y ++ "\",") 

---------------------

switchConfigIntfs h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "# configure switch interfaces"
 writeBuildIT h ""
 writeBuildIT h "$InterfaceJson = @\""
 writeBuildIT h "{"
 writeBuildIT h "    \"jsonrpc\":\"2.0\","
 writeBuildIT h "    \"method\":\"runCmds\","
 writeBuildIT h "    \"params\": {"
 writeBuildIT h "        \"version\": 1,"
 writeBuildIT h "        \"cmds\": ["
 writeBuildIT h "            \"enable\","
 writeBuildIT h "            \"configure terminal\","

 switchInfDVlanbl h a c

 writeBuildIT h "            \"write memory\""
 writeBuildIT h "        ],"
 writeBuildIT h "    \"format\":\"text\","
 writeBuildIT h "    \"timestamps\": \"false\""
 writeBuildIT h "    },"
 writeBuildIT h "    \"id\": \"BuildIT\""
 writeBuildIT h "}"
 writeBuildIT h "\"@"
 writeBuildIT h ""

 writeBuildIT h ("$interface_response = Invoke-WebRequest -Method Post -Uri $"
                 ++ a ++ "_url -Body $InterfaceJson -Credential $" ++ a ++
                  "_converted_cred")

------------------------------

switchInfDVlanbl h a (IFaceList IFaceEmpty c) = switchInfDVlanb h a c

switchInfDVlanbl h a (IFaceList b c) = do
 switchInfDVlanbl h a b 
 switchInfDVlanb h a c 

switchInfDVlanb h a (IFace1 b c) = do
 if b == 0
 then
  do
   return ()
 else
  do
   writeBuildIT h ("              \"interface Ethernet" ++ (show b) ++ "\",")
   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Switch " ++ a ++ " Interface " ++ (show b) ++
      " network missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("                \"description " ++ y ++ "\",") 

   let z = findCNSL "vlan" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Switch " ++ a ++ " Interface " ++ (show b) ++
      " vlan missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("                \"switchport access vlan " ++ y ++ "\",") 

---------------------

switchConfigVlans h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "# configure switch vlans, ip adx's etc."
 writeBuildIT h ""
 writeBuildIT h "$VLANCfgJson = @\""
 writeBuildIT h "{"
 writeBuildIT h "    \"jsonrpc\":\"2.0\","
 writeBuildIT h "    \"method\":\"runCmds\","
 writeBuildIT h "    \"params\": {"
 writeBuildIT h "        \"version\": 1,"
 writeBuildIT h "        \"cmds\": ["
 writeBuildIT h "            \"enable\","
 writeBuildIT h "            \"configure terminal\","

 switchInfEVlanbl h a c

 writeBuildIT h "            \"write memory\""
 writeBuildIT h "        ],"
 writeBuildIT h "    \"format\":\"text\","
 writeBuildIT h "    \"timestamps\": \"false\""
 writeBuildIT h "    },"
 writeBuildIT h "    \"id\": \"BuildIT\""
 writeBuildIT h "}"
 writeBuildIT h "\"@"
 writeBuildIT h ""

 writeBuildIT h ("$VLANCfg_response = Invoke-WebRequest -Method Post -Uri $"
                 ++ a ++ "_url -Body $VLANCfgJson -Credential $" ++ a ++
                  "_converted_cred")

---------------------

switchInfEVlanbl h a (IFaceList IFaceEmpty c) = switchInfEVlanb h a c

switchInfEVlanbl h a (IFaceList b c) = do
 switchInfEVlanbl h a b 
 switchInfEVlanb h a c 

switchInfEVlanb h a (IFace1 b c) = do
 if b == 0
 then
  do
   return ()
 else
  do
   let z = findCNSL "vlan" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Switch " ++ a ++ " Interface " ++ (show b) ++
      " vlan missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("              \"interface VLAN " ++ y ++ "\",") 

   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Switch " ++ a ++ " Interface " ++ (show b) ++
      " network  missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("                \"description " ++ y ++ "\",") 

   let z = findCNSL "ip" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Switch " ++ a ++ " Interface " ++ (show b) ++
      " ip  missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("                \"ip address " ++ y ++ "\",") 

--------------

switchConfigRoutes h (SWBlock a b c (IRoute1 d)) = do
 writeBuildIT h ""
 writeBuildIT h "# switch layer 3 routing"
 writeBuildIT h ""
 writeBuildIT h "$IPRoutingJson = @\""
 writeBuildIT h "{"
 writeBuildIT h "    \"jsonrpc\":\"2.0\","
 writeBuildIT h "    \"method\":\"runCmds\","
 writeBuildIT h "    \"params\": {"
 writeBuildIT h "        \"version\": 1,"
 writeBuildIT h "        \"cmds\": ["
 writeBuildIT h "            \"enable\","
 writeBuildIT h "            \"configure terminal\","
 writeBuildIT h "              \"ip routing\","

 switchIProutebl h a d

 writeBuildIT h "            \"write memory\""
 writeBuildIT h "        ],"
 writeBuildIT h "    \"format\":\"text\","
 writeBuildIT h "    \"timestamps\": \"false\""
 writeBuildIT h "    },"
 writeBuildIT h "    \"id\": \"BuildIT\""
 writeBuildIT h "}"
 writeBuildIT h "\"@"
 writeBuildIT h ""

 writeBuildIT h ("$IPRouting_response = Invoke-WebRequest -Method Post -Uri $"
                 ++ a ++ "_url -Body $IPRoutingJson -Credential $" ++ a ++
                  "_converted_cred")

--------------

switchIProutebl h a (CnstrList Empty c)  = switchIProuteb h a c

switchIProutebl h a (CnstrList b c) = do
 switchIProutebl h a b
 switchIProuteb h a c

switchIProuteb h a (Cnstr1 _ c) = do
 writeBuildIT h ("                \"" ++ c ++ "\",")

switchIProuteb h a (Cnstr2 _ c) = do
 writeBuildIT h ("                \"" ++ (show c) ++ "\",")

switchIProuteb h a (Cnstr3 _ c) = do
 writeBuildIT h ("                \"" ++ c ++ "\",")

--------------

switchConfigMgmt h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "# Configure switch managment interface"
 writeBuildIT h ""
 writeBuildIT h "$MGTIntIPJson = @\""
 writeBuildIT h "{"
 writeBuildIT h "    \"jsonrpc\":\"2.0\","
 writeBuildIT h "    \"method\":\"runCmds\","
 writeBuildIT h "    \"params\": {"
 writeBuildIT h "        \"version\": 1,"
 writeBuildIT h "        \"cmds\": ["
 writeBuildIT h "            \"enable\","
 writeBuildIT h "            \"configure terminal\","
 writeBuildIT h "              \"interface Management1\","

 switchInfFVlanbl h a c

 writeBuildIT h "            \"write memory\""
 writeBuildIT h "        ],"
 writeBuildIT h "    \"format\":\"text\","
 writeBuildIT h "    \"timestamps\": \"false\""
 writeBuildIT h "    },"
 writeBuildIT h "    \"id\": \"BuildIT\""
 writeBuildIT h "}"
 writeBuildIT h "\"@"
 writeBuildIT h ""

 writeBuildIT h ("$MGTIntIP_response = Invoke-WebRequest -Method Post -Uri $"
                 ++ a ++ "_url -Body $MGTIntIPJson -Credential $" ++ a ++
                  "_converted_cred")

--------------

switchInfFVlanbl h a (IFaceList IFaceEmpty c) = switchInfFVlanb h a c

switchInfFVlanbl h a (IFaceList b c) = do
 switchInfFVlanbl h a b 
 switchInfFVlanb h a c 

switchInfFVlanb h a (IFace1 b c) = do
 if b /= 0
 then
  do
   return ()
 else
  do
   let z = findCNSL "ip" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Switch " ++ a ++ " Interface " ++ (show b) ++
      " ip missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("                \"ip address " ++ y ++ "\",") 

--------------

switchSetMgmt h (SWBlock a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "# Enable switch management interface within VM"
 writeBuildIT h ""
 writeBuildIT h ("Get-VM -name $" ++ a ++ "_name |")
 writeBuildIT h (" Get-NetworkAdapter -Name \"Network adapter 1\" |")
 switchInfGVlanbl h a c

--------------
switchInfGVlanbl h a (IFaceList IFaceEmpty c) = switchInfGVlanb h a c

switchInfGVlanbl h a (IFaceList b c) = do
 switchInfGVlanbl h a b 
 switchInfGVlanb h a c 

switchInfGVlanb h a (IFace1 b c) = do
 if b /= 0
 then
  do
   return ()
 else
  do
   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Switch " ++ a ++ " Interface " ++ (show b) ++
      " network missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("   Set-NetworkAdapter -NetworkName $" ++ y
        ++ " -Confirm:$false")

---------------------

obuildnodebl h (BlockList BEmpty b) = obuildnodeb h b

obuildnodebl h (BlockList a b) = do
 obuildnodebl h a
 obuildnodeb h b

-- NBlock2 String CnstrList INicList IPlatform
obuildnodeb h (NBlock2 a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "#--------------------------"
 writeBuildIT h ("# Build Node " ++ a)
 writeBuildIT h "#--------------------------"
 nodeCred h a b
 nodeNetworks h a c
 nodeScript1 h a d
 writeBuildIT h ""
 writeBuildIT h ("Get-OSCustomizationSpec ScriptedDeployment |")
 writeBuildIT h (" Get-OSCustomizationNicMapping |")
 writeBuildIT h ("  Set-OSCustomizationNicMapping -IpMode:UseStaticIP " ++
                    "-IpAddress $" ++ a ++ "_network_ip -SubnetMask " ++
                    "$" ++ a ++ "_network_mask -DefaultGateway " ++
                    "$" ++ a ++ "_network_gateway")

 writeBuildIT h ""
 writeBuildIT h ("New-VM -Template $" ++ a ++ "_Template -Name $" ++ a ++
                  "_name -VMHost $esxhost -OSCustomizationSpec" ++
                  " ScriptedDeployment")

 writeBuildIT h ""
 writeBuildIT h ("Start-VM -VM $" ++ a ++ "_name")

 writeBuildIT h ""
 writeBuildIT h ("Get-VM -name $" ++ a ++ "_name |")
 writeBuildIT h (" Get-NetworkAdapter -Name \"Network adapter 1\" |")
 writeBuildIT h ("  Set-NetworkAdapter -NetworkName $" ++ a ++
                   "_network_name -StartConnected:$true -Connected" ++
                   ":$true -Confirm:$false")

 writeBuildIT h ""
 writeBuildIT h ("Start-Sleep -s 90")

 writeBuildIT h ""
 writeBuildIT h ("Invoke-VMScript -VM $" ++ a ++ "_name" ++
                 " -GuestUser $" ++ a ++ "_plat_lcluser -GuestPassword " ++
                 "$" ++ a ++ "_plat_lclpswd -ScriptText \"$" ++ a ++
                 "_plat_artscrpt\" -RunAsync:$true")

 writeBuildIT h ""
 writeBuildIT h ("Invoke-VMScript -VM $" ++ a ++ "_name" ++
                 " -GuestUser $" ++ a ++
                 "_plat_lcluser -GuestPassword " ++
                 "$" ++ a ++
                 "_plat_lclpswd -ScriptText \"bash /tmp/install/$" ++
                 a ++
                 "_plat_strtscrpt\" -Arguments $" ++ a ++
                 "_plat_strtparms -RunAsync:$true")

-- NBlock1 String CnstrList INicList IInsec
obuildnodeb h (NBlock1 a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "#--------------------------"
 writeBuildIT h ("# Build Node " ++ a)
 writeBuildIT h "#--------------------------"
 nodeCred h a b
 nodeNetworks h a c
 nodeScript h a d
 writeBuildIT h ""
 writeBuildIT h ("Get-OSCustomizationSpec ScriptedDeployment |")
 writeBuildIT h (" Get-OSCustomizationNicMapping |")
 writeBuildIT h ("  Set-OSCustomizationNicMapping -IpMode:UseStaticIP" ++
                    "-IpAddress $" ++ a ++ "_network_ip -SubnetMask " ++
                    "$" ++ a ++ "_network_mask -DefaultGateway " ++
                    "$" ++ a ++ "_network_gateway")

 writeBuildIT h ""
 writeBuildIT h ("New-VM -Template $" ++ a ++ "_Template -Name $" ++ a ++
                  "_name -VMHost $esxhost -OSCustomizationSpec" ++
                  " ScriptedDeployment")

 writeBuildIT h ""
 writeBuildIT h ("Start-VM -VM $" ++ a ++ "_name")

 writeBuildIT h ""
 writeBuildIT h ("Get-VM -name $" ++ a ++ "_name |")
 writeBuildIT h (" Get-NetworkAdapter -Name \"Network adapter 1\" |")
 writeBuildIT h ("  Set-NetworkAdapter -NetworkName $" ++ a ++
                   "_network_name -StartConnected:$true -Connected" ++
                   ":$true -Confirm:$false")

 writeBuildIT h ""
 writeBuildIT h ("Start-Sleep -s 90")

 writeBuildIT h ""
 writeBuildIT h ("Invoke-VMScript -VM $" ++ a ++ "_name" ++
                 " -GuestUser $" ++ a ++ "_init_user -GuestPassword " ++
                 "$" ++ a ++ "_init_pswd -ScriptText \"$" ++ a ++
                 "_init_script\" -RunAsync:$true")
 
obuildnodeb h (NBlock a b c) = do
 writeBuildIT h ""
 writeBuildIT h "#--------------------------"
 writeBuildIT h ("#Build Node " ++ a)
 writeBuildIT h "#--------------------------"
 nodeCred h a b
 nodeNetworks h a c
 writeBuildIT h ""
 writeBuildIT h ("Get-OSCustomizationSpec ScriptedDeployment |")
 writeBuildIT h (" Get-OSCustomizationNicMapping |")
 writeBuildIT h ("  Set-OSCustomizationNicMapping -IpMode:UseStaticIP " ++
                    "-IpAddress $" ++ a ++ "_network_ip -SubnetMask " ++
                    "$" ++ a ++ "_network_mask -DefaultGateway " ++
                    "$" ++ a ++ "_network_gateway")

 writeBuildIT h ""
 writeBuildIT h ("New-VM -Template $" ++ a ++ "_Template -Name $" ++ a ++
                  "_name -VMHost $esxhost -OSCustomizationSpec" ++
                  " ScriptedDeployment")

 writeBuildIT h ""
 writeBuildIT h ("Start-VM -VM $" ++ a ++ "_name")

 writeBuildIT h ""
 writeBuildIT h ("Get-VM -name $" ++ a ++ "_name |")
 writeBuildIT h (" Get-NetworkAdapter -Name \"Network adapter 1\" |")
 writeBuildIT h ("  Set-NetworkAdapter -NetworkName $" ++ a ++
                   "_network_name -StartConnected:$true -Connected" ++
                   ":$true -Confirm:$false")

 writeBuildIT h ""
 writeBuildIT h ("Start-Sleep -s 90")

obuildnodeb h _ = do
 return ()

-----------------

nodeCred h a b = do
 writeBuildIT h ""
 writeBuildIT h "# set up variables used to build node"
 writeBuildIT h ""
 let z = findCNSL "template" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " template missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_Template = \"" ++ y ++ "\"")

 let z = findCNSL "name" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " name missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

-----------------

nodeNetworks h a (INicList INicEmpty c)  = nodeNetwork h a c

nodeNetworks h a (INicList b c) = do
 nodeNetworks h a b
 nodeNetwork h a c

-- h String (INic1 Int CnstrList)
nodeNetwork h a (INic1 b c) = do
 if b == 0
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " nic 0 not allowed")
   exitFailure
 else
  do
   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Node " ++ a ++ " nic " ++
                (show b) ++ " network missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("$" ++ a ++ "_network_name = \"" ++ y ++ "\"")

   let z = findCNSL "ip" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Node " ++ a ++ " nic " ++
                (show b) ++ " ip missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("$" ++ a ++ "_network_ip = \"" ++ y ++ "\"")

   let z = findCNSL "mask" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Node " ++ a ++ " nic " ++
                (show b) ++ " mask missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("$" ++ a ++ "_network_mask = \"" ++ y ++ "\"")

   let z = findCNSL "gateway" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Node " ++ a ++ " nic " ++
                (show b) ++ " gateway missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("$" ++ a ++ "_network_gateway = \"" ++ y ++ "\"")

-----------------

nodeScript1 h a (IPlatform1 b)  = do
 let z = findCNSL "artuser" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ "platform artuser missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_plat_artuser = \"" ++ y ++ "\"")

 let z = findCNSL "artpswd" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " platform artpswd missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_plat_artpswd = \"" ++ y ++ "\"")

 let z = findCNSL "artsrc" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " platform artsrc missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_plat_artsrc = \"" ++ y ++ "\"")

 writeBuildIT h ("$" ++ a ++ "_plat_artscrpt = " ++
                "\"mkdir -p /tmp/mnt; " ++
                "mkdir -p /tmp/install; " ++
                "mount -t cifs -o " ++
                "username=$" ++ a ++ "_plat_artuser," ++
                "password=$" ++ a ++ "_plat_artpswd " ++
                "$" ++ a ++ "_plat_artsrc /tmp/mnt; " ++
                "cp /tmp/mnt/* /tmp/install; " ++
                "unzip /tmp/install/*; " ++
                "umount /tmp/mnt; " ++
                "rm -rf /tmp/mnt;\"")

 let z = findCNSL "lcluser" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " platform lcluser missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_plat_lcluser = \"" ++ y ++ "\"")

 let z = findCNSL "lclpswd" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " platform lclpswd missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_plat_lclpswd = \"" ++ y ++ "\"")

 let z = findCNSL "strtscrpt" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " platform strtscrpt missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_plat_strtscrpt = \"" ++ y ++ "\"")

 let z = findCNSL "strtparms" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " platform strtparms missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_plat_strtparms = \"" ++ y ++ "\"")

-----------------

nodeScript h a (IInsec1 b)  = do
 let z = findCNSL "user" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ "init missing user  missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_init_user = \"" ++ y ++ "\"")

 let z = findCNSL "pswd" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " init pswd missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_init_pswd = \"" ++ y ++ "\"")

 let z = findCNSL "script" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Node " ++ a ++ " init script missing")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_init_script = \"" ++ y ++ "\"")

-----------------

obuildfirewallbl h (BlockList BEmpty b) = obuildfirewallb h b

obuildfirewallbl h (BlockList a b) = do
 obuildfirewallbl h a
 obuildfirewallb h b

-- FBlock1 String CnstrList IFaceList IRoute
obuildfirewallb h (FBlock1 a b c d) = do
 writeBuildIT h ""
 writeBuildIT h "#--------------------------"
 writeBuildIT h ("# Build Firewall " ++ a)
 writeBuildIT h "#--------------------------"
 firewallCred h a b "Firewall"
 vmMake h a
 firewallIntfs h a c
 firewallPower h a
 firewallApiKey h a b
 firewallLicense h a
 firewallImportConfig h a
 firewallLoadConfig h a
 firewallCommitConfig h a
 firewallSetMgmt h a c

-- FBlock String CnstrList IFaceList FBlockList IRoute
obuildfirewallb h (FBlock a b c d e) = do
 writeBuildIT h ""
 writeBuildIT h "#--------------------------"
 writeBuildIT h ("# Build Firewall " ++ a)
 writeBuildIT h "#--------------------------"
 firewallCred h a b "Firewall"
 vmMake h a
 firewallIntfs h a c
 firewallPower h a
 firewallApiKey h a b
 firewallLicense h a
 firewallImportConfig h a
 firewallLoadConfig h a
 firewallCommitConfig h a
 firewallSetMgmt h a c

obuildfirewallb h _ = do
 return ()

-------------------------------

firewallIntfs h a b = do
 writeBuildIT h ""
 writeBuildIT h "# set up firewall network adapters"
 writeBuildIT h ""
 firewallIntfs' h a b

firewallIntfs' h a (IFaceList IFaceEmpty c) = firewallIntf h a c

firewallIntfs' h a (IFaceList b c) = do
 firewallIntfs' h a b
 firewallIntf h a c

-- IFace1 Int CnstrList
firewallIntf h a (IFace1 b c) = do
 if b == 0
 then
  do
   return ()
 else
  do
   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Firewall " ++ a ++ " Interface " ++ (show b) ++
      " network segment missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("New-NetworkAdapter -VM $" ++ a ++ 
      "_name -Type VMXNET3 -NetworkName $" ++ y ++ " -StartConnected:$true" ++
      " -Confirm:$false")

-----------------

firewallPower h a = do
 writeBuildIT h ""
 writeBuildIT h "# start firewall and wait for it to be ready"
 writeBuildIT h ""
 writeBuildIT h ("Start-VM -VM $" ++ a ++ "_name")
 writeBuildIT h "Start-Sleep -s 60"

------------------

-- h String CnstrList
firewallApiKey h a b = do
 writeBuildIT h ""
 writeBuildIT h "# Set up keys for firewall license server"
 writeBuildIT h ""
 let z = findCNSL "url" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++ " url missing")
   exitFailure
 else
  do
   let y = fromJust z
   let z = findCNSL "user" b
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Firewall " ++ a ++ " user missing")
     exitFailure
   else
    do
     let y' = fromJust z
     let z = findCNSL "user" b
     if z == Nothing
     then
      do
       putStrLn ("BuildIT: Firewall " ++ a ++ " pswd missing")
       exitFailure
     else
      do
       let y'' = fromJust z
       let z = findCNSL "auth" b
       if z == Nothing
       then
        do
         putStrLn ("BuildIT: Firewall " ++ a ++ " auth missing")
         exitFailure
       else
        do
         let y''' = fromJust z
         writeBuildIT h ("$apikey_raw = curl.exe -v -k https://$" ++ a ++
                         "_url/api?type=keygen\"&\"user=$" ++ a ++
                         "_user\"&\"password=$" ++ a ++ "_pswd")
         writeBuildIT h ("$apikey = $apikey_raw -replace" ++
                         "\"<response status = 'success'><result><key>\"," ++
                         " \"\" -replace \"=</key></result></response>\"," ++
                         " \"\"")

-----------------
firewallLicense h a = do
 writeBuildIT h ""
 writeBuildIT h "# request license"
 writeBuildIT h ""
 writeBuildIT h ("curl.exe -v -k https://$" ++ a ++ "_url/api/?type=op\"&\"" ++
                 "cmd=%3Crequest%3E%3Clicense%3E%3" ++
                 "Cfetch%3E%3Cauth-code%3E" ++
                 "$" ++ a ++ "_code" ++
                 "%3C%2" ++
                 "Fauth-code%3E%3C%2Ffetch%3E%3C%2Flicense%3E%3C%2" ++
                 "Frequest%3E\"&\"key=$apikey")

----------------

firewallImportConfig h a = do
 writeBuildIT h ""
 writeBuildIT h "# import firewall xml configuration"
 writeBuildIT h ""
 writeBuildIT h ("curl.exe -v -k --form file=@$" ++ a ++ "_path$" ++ a ++
                 "_file " ++ "https://$" ++ a ++ "_url/api/?type=import" ++
                 "\"&\"category=configuration\"&\"key=$apikey")

----------------

firewallLoadConfig h a = do
 writeBuildIT h ""
 writeBuildIT h "# load firewall xml configuration"
 writeBuildIT h ""
 writeBuildIT h ("curl.exe -v -k https://$" ++ a ++ "_url/api/?type=op" ++
                 "\"&\"cmd=%3Cload%3E%3Cconfig%3E%3Cfrom%3E$" ++ a ++
                 "_file%3C%2Ffrom%3E%3C%2Fconfig%3E%3C%2Fload%3E" ++
                 "\"&\"key=$apikey")

----------------

firewallCommitConfig h a = do
 writeBuildIT h ""
 writeBuildIT h "# commit the configuration (so it loads on reboot)"
 writeBuildIT h ""
 writeBuildIT h ("curl.exe -v -k https://$" ++ a ++ "_url/api?type=commit" ++
                 "\"&\"cmd=%3Ccommit%3E%3C%2Fcommit%3E\"&\"key=$apikey")

----------------

firewallSetMgmt h a c = do
 writeBuildIT h ""
 writeBuildIT h "# setup firewall management interface"
 writeBuildIT h ""
 writeBuildIT h ("Get-VM -name $" ++ a ++ "_name |")
 writeBuildIT h (" Get-NetworkAdapter -Name \"Network adapter 1\" |")
 firewallInfGVlanbl h a c

--------------
firewallInfGVlanbl h a (IFaceList IFaceEmpty c) = firewallInfGVlanb h a c

firewallInfGVlanbl h a (IFaceList b c) = do
 firewallInfGVlanbl h a b 
 firewallInfGVlanb h a c 

firewallInfGVlanb h a (IFace1 b c) = do
 if b /= 0
 then
  do
   return ()
 else
  do
   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Firewall " ++ a ++ " Interface " ++ (show b) ++
      " network missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("   Set-NetworkAdapter -NetworkName $" ++ y
        ++ " -Confirm:$false")

---------------------

obuildloadbalancebl h (BlockList BEmpty b) = obuildloadbalanceb h b

obuildloadbalancebl h (BlockList a b) = do
 obuildloadbalancebl h a
 obuildloadbalanceb h b

-- KBlock String CnstrList LBlockList IRoute
obuildloadbalanceb h (KBlock a b c (IRoute1 d)) = do
 writeBuildIT h ""
 writeBuildIT h "#--------------------------"
 writeBuildIT h ("# Build Kemp Load Balancer " ++ a)
 writeBuildIT h "#--------------------------"
 loadbalanceCred h a b c
 vmMake h a
 loadbalanceIntfs h a c
 loadbalancePower h a
 loadbalanceLicense h a b c
 loadbalanceApplyConfig h a c
 loadbalanceRoutes h a d
 loadbalanceVirtService h a c
 loadbalanceSetMgmt h a c
-- @@

obuildloadbalanceb h _ = do
 return ()

---------------------
loadbalanceCred h a b c = do
 writeBuildIT h ""
 writeBuildIT h ("# variables used to build " ++ a)
 writeBuildIT h ""

 let z = findCNSL "template" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " has no template")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_Template = \"" ++ y ++ "\"")

 writeBuildIT h ""

 let z = findCNSL "name" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " has no name")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_name = \"" ++ y ++ "\"")

 writeBuildIT h ""
 
 let z = getIntf0IPadxl c
 if z == Nothing
 then
  do
   putStrLn ("Kemp Load Balancer " ++ a ++ " missing interface 0 OR " ++
                   "no ip in interface 0")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_lbadx = \"" ++ y ++ "\"")

 writeBuildIT h ""

 let z = findCNSL "init_usr" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " needs init_usr")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_init_usr = \"" ++ y ++ "\"")

 let z = findCNSL "init_psw" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " needs init_psw")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_init_psw = \"" ++ y ++ "\"")

 writeBuildIT h ""

 let z = findCNSL "lic_usr" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " needs lic_usr")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_lic_usr = \"" ++ y ++ "\"")

 let z = findCNSL "lic_psw" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " needs lic_psw")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_lic_psw = \"" ++ y ++ "\"")

 writeBuildIT h ""

 let z = findCNSL "real_usr" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " needs real_usr")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_real_usr = \"" ++ y ++ "\"")

 let z = findCNSL "real_psw" b
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " needs real_psw")
   exitFailure
 else
  do
   let y = fromJust z
   writeBuildIT h ("$" ++ a ++ "_real_psw = \"" ++ y ++ "\"")

 writeBuildIT h ""

 writeBuildIT h ("$" ++ a ++ "_cnv_init_psw = ConvertTo-SecureString " ++
                 "\"$" ++ a ++ "_init_psw\" -AsPlainText -Force")

 writeBuildIT h ("$" ++ a ++ "_cnv_init_crd = New-Object System" ++
                 ".Management.Automation.PSCredential(\"$" ++ a ++
                 "_init_usr\", $" ++ a ++ "_cnv_init_psw)")

 writeBuildIT h ""

 writeBuildIT h ("$" ++ a ++ "_cnv_real_psw = ConvertTo-SecureString " ++
                 "\"$" ++ a ++ "_real_psw\" -AsPlainText -Force")

 writeBuildIT h ("$" ++ a ++ "_cnv_real_crd = New-Object System" ++
                 ".Management.Automation.PSCredential(\"$" ++ a ++
                 "_real_usr\", $" ++ a ++ "_cnv_real_psw)")
 

-------------------------------

-- loadbalanceIntfs h String LBlockList
loadbalanceIntfs h a b = do
 if anyIfsLBl b == True
 then
  return ()
 else
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " has no interfaces")
   exitFailure

 writeBuildIT h ""
 writeBuildIT h "# set up load balance network adapters"
 writeBuildIT h ""
 loadbalanceIntfs' h a b

loadbalanceIntfs' h a (LBlockList LBEmpty c) = loadbalanceIntf h a c

loadbalanceIntfs' h a (LBlockList b c) = do
 loadbalanceIntfs' h a b
 loadbalanceIntf h a c

-- IFace2 Int CnstrList
loadbalanceIntf h a (IFace2 b c) = do
 if b == 0
 then
  do
   return ()
 else
  do
   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Kemp LB " ++ a ++ " Interface " ++ (show b) ++
      " network segment missing")
     exitFailure
   else
    do
     let y = fromJust z
     writeBuildIT h ("New-NetworkAdapter -VM $" ++ a ++ 
      "_name -Type VMXNET3 -NetworkName $" ++ y ++ " -StartConnected:$true" ++
      " -Confirm:$false")

loadbalanceIntf h a _ = do
 return ()

-----------------

loadbalancePower h a = do
 writeBuildIT h ""
 writeBuildIT h "# start kemp load balancer and wait for it to be ready"
 writeBuildIT h ""
 writeBuildIT h ("Start-VM -VM $" ++ a ++ "_name")
 writeBuildIT h "Start-Sleep -s 60"

---------------------

loadbalanceLicense h a b c = do
 writeBuildIT h ""
 writeBuildIT h ("# License Kemp Load Balancer " ++ a)
 writeBuildIT h ""
 
 writeBuildIT h ""

 writeBuildIT h ("[XML]$eula1_" ++ a ++ " = ReadEula -LoadBalancer " ++
                 "$" ++ a ++ "_lbadx" ++
                 " -Credential $" ++ a ++"_cnv_init_crd")

 writeBuildIT h ""

 writeBuildIT h ("$magic1_" ++ a ++ " = $eula1_" ++ a ++
                 ".Response.Success.Data |")
 writeBuildIT h " Select -ExpandProperty Magic"

 writeBuildIT h ""

 writeBuildIT h ("[XML]$eula2_" ++ a ++ "  = AcceptEula -LoadBalancer $" ++
                 a ++ "_lbadx -Credential $" ++ a ++ "_cnv_init_crd " ++
                 "-Magic $magic1_" ++ a ++ " -Type Free -Verbose")

 writeBuildIT h ""

 writeBuildIT h ("$magic2_" ++ a ++ " = $eula2_" ++ a ++
                 ".Response.Success.Data |")
 writeBuildIT h " Select -ExpandProperty Magic"

 writeBuildIT h ""

 writeBuildIT h ("AcceptEula2 -LoadBalancer $" ++ a ++ "_lbadx " ++
                 "-Credential $" ++ a ++ "_cnv_init_crd -Magic $magic2_" ++
                 a ++ " -Accept yes -Verbose")
 
 writeBuildIT h ""

 writeBuildIT h ("Alsilicense -LoadBalancer $" ++ a ++ "_lbadx " ++
                 "-Credential $" ++ a ++ "_cnv_init_crd -KempId " ++
                 "$" ++ a ++ "_lic_usr -Password $" ++ a ++ "_lic_psw " ++
                 "-Verbose")
 
 writeBuildIT h ""

 writeBuildIT h ("SetinitialPasswd -LoadBalancer $" ++ a ++ "_lbadx " ++
                 "-Credential $" ++ a ++ "_cnv_init_crd -Passwd " ++
                 "$" ++ a ++ "_real_psw -Verbose")

---------------------

-- loadbalanceApplyConfig IO lbname::String CnstrList LBlockList
loadbalanceApplyConfig h a c = do
 writeBuildIT h ""
 writeBuildIT h ("# Configure Kemp Load Balancer " ++ a)
 writeBuildIT h ""
 
 writeBuildIT h ("Initialize-LoadBalancer -Address $" ++ a ++  "_lbadx " ++
                 "-LBPort 443 -Credential $" ++ a ++ "_cnv_real_crd -Verbose")
 writeBuildIT h "Start-Sleep 5"
 writeBuildIT h ""

 writeBuildIT h ("Set-DNSConfiguration -Hostname $" ++ a ++ "_name")
 writeBuildIT h "Start-Sleep 5"
 writeBuildIT h ""

 writeBuildIT h "Set-NetworkOptions -NonLocalRS:$true -MultiGW:$true"
 writeBuildIT h "Start-Sleep 5"

 loadbalanceConfigIntfs h a c

---------------------

loadbalanceConfigIntfs h a (LBlockList LBEmpty c) = loadbalanceConfigIntf h a c

loadbalanceConfigIntfs h a (LBlockList b c) = do
 loadbalanceConfigIntfs h a b
 loadbalanceConfigIntf h a c

loadbalanceConfigIntf h a (IFace2 b c) = do
 writeBuildIT h ""
 if b == 0 -- interface 0 is managment only
 then
  do
   writeBuildIT h ("Set-Interface -InterfaceID 0 " ++
                   "-GWIface:$false -GeoTraffic:$false")
 else
  do
   let z = findCNSL "ip" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ 
               "interface " ++ (show b) ++ " has no ip adx")
     exitFailure
   else
    do
     let y = fromJust z
     if b == 1 -- interface 1 is management only too
     then
      do
       writeBuildIT h ("Set-Interface -InterfaceID 1 " ++
                       "-IPAddress " ++ y ++
                       " -GWIface:$false -GeoTraffic:$false")
     else -- interface is for load balancing
      do
       writeBuildIT h ("Set-Interface -InterfaceID " ++ (show b) ++ 
                       " -IPAddress " ++ y ++
                       " -GWIface:$true -GeoTraffic:$true")

 writeBuildIT h "Start-Sleep 5"

loadbalanceConfigIntf h a _ = do
 return ()

---------------------

loadbalanceRoutes h a b = do
 writeBuildIT h ""
 writeBuildIT h "# Configure Load Balancer Routes"
 loadbalanceRoutes' h a b

loadbalanceRoutes' h a (CnstrList Empty c) = loadbalanceRoute h a c

loadbalanceRoutes' h a (CnstrList b c) = do
 loadbalanceRoutes' h a b
 loadbalanceRoute h a c

loadbalanceRoute h a (Cnstr1 b c) = do
 putStrLn ("BuiltIT: Kemp Load balancer " ++ a ++ "Route " ++ b ++
           " must be of form \"destIPadx gatewayIPadx\"")
 exitFailure

loadbalanceRoute h a (Cnstr2 b c) = do
 putStrLn ("BuiltIT: Kemp Load balancer " ++ a ++ "Route " ++ b ++
           " must be of form \"destIPadx gatewayIPadx\"")
 exitFailure

loadbalanceRoute h a (Cnstr3 b c) = do
 writeBuildIT h ""
 let z = splitOn " " c
 if length z /= 2
 then
  do
   putStrLn ("BuiltIT: Kemp Load balancer " ++ a ++ "Route " ++ b ++
           " must be of form \"destIPadx gatewayIPadx\"")
   exitFailure
 else
  do
   writeBuildIT h ("New-Route -CIDR 24 -Destination " ++ (z!!0) ++
                   " -Gateway " ++ (z!!1))
   writeBuildIT h "Start-Sleep 5"

---------------------

loadbalanceVirtService h a d = do
 writeBuildIT h ""
 writeBuildIT h "# Configure Load Balancer Virtual Services"
 writeBuildIT h ""
 loadbalancevservs h a d

---------------------
loadbalancevservs h a (LBlockList LBEmpty c) = loadbalancevserv h a c
loadbalancevservs h a (LBlockList b c) = do
 loadbalancevservs h a b
 loadbalancevserv h a c

-- IServ2 String CnstrList
loadbalancevserv h a (IServ2 b c) = do
 let g = findCNSL "gateway" c
 if g == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " service " ++
             b ++ " missing gateway")
   exitFailure 
  else
   do            
    let p = findCNSL "port" c
    if p == Nothing
    then
     do
      putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " service " ++
             b ++ " missing port")
      exitFailure 
    else
     do
      let v = findCNSL "vip" c
      if v == Nothing
      then
       do
        putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " service " ++
             b ++ " missing port")
        exitFailure 
      else
       do
        writeBuildIT h ("New-VirtualService -DefaultGW " ++
                       (fromJust g) ++ " -Nickname " ++ a ++
                      " -Port " ++ (fromJust p) ++ " -virtualService " ++
                      (fromJust v) ++ " -Protocol tcp -CheckPort 443" ++
                      " -Transparent:$false -Persist src" ++
                      " -PersistTimeout 30 -Schedule rr")
        writeBuildIT h "Start-Sleep 5"

        let z = findCNSL "host" c
        if z == Nothing
        then
         do 
          putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " service " ++
                      b ++ " missing host")
          exitFailure 
        else
         do
          let y = fromJust z
          let y' = splitOn "," y
          forM_ y' $ \s -> do
           writeBuildIT h ""
           writeBuildIT h ("New-RealServer -VirtualService " ++ (fromJust v) ++
             " -Port " ++ (fromJust p) ++ " -Protocol tcp -RealServer " ++
             s ++ " -RealServerPort " ++ (fromJust p) ++ " -Enable:$true" ++
             " -Non_Local:$true")
           writeBuildIT h "Start-Sleep 5"

loadbalancevserv h a _ = do
 return ()

---------------------

loadbalanceSetMgmt h a c = do
 writeBuildIT h ""
 writeBuildIT h "# Configure Management Network on Interface 1"
 writeBuildIT h ""
 let z = getIntf1Gtwy c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Kemp Load Balancer " ++ a ++ " interface 1 missing " ++
             "gateway")
   exitFailure 
 else
  do
   writeBuildIT h ("Set-AdminAccess -WuiIface 1 -WuiPort 443 " ++
                   "-WuiDefaultGateway " ++ (fromJust z))

   writeBuildIT h "Start-Sleep 5"
   writeBuildIT h ""

   writeBuildIT h ("Get-VM -name $" ++ a ++ "_name |")
   writeBuildIT h (" Get-NetworkAdapter -Name \"Network Adapter 1\" |")
   writeBuildIT h ("  Set-NetworkAdapter -Connected:$false " ++
                   "-StartConnected:$false -Confirm:$false")
   writeBuildIT h "Start-Sleep 5"
 
---------------------
-- any load balancers?

anyKBlockl (BlockList BEmpty b) = anyKBlock b
anyKBlockl (BlockList a b) = if anyKBlock b == True then True else anyKBlockl a

anyKBlock (KBlock _ _ _ _) = True
anyKBlock _ = False

-- any interfaces in LBlockList

anyIfsLBl (LBlockList LBEmpty b) = anyIfsLB b
anyIfsLBl (LBlockList a b) = if anyIfsLB b == True then True else anyIfsLBl a

anyIfsLB (IFace2 _ _) = True
anyIfsLB _ = False

---------------------
-- interface 0 ip address

getIntf0IPadxl (LBlockList LBEmpty b) = getIntf0IPadx b

getIntf0IPadxl (LBlockList a b) =
 let z = getIntf0IPadx b in
  if z /= Nothing then z else getIntf0IPadxl a

-- IFace2 Int CnstrList
getIntf0IPadx (IFace2 a b) =
 if a == 0
 then
  findCNSL "ip" b 
 else
  Nothing

getIntf0IPadx _ = Nothing

---------------------

-- interface 1 gateway

getIntf1Gtwy (LBlockList LBEmpty b) = getIntf1Gtwy' b

getIntf1Gtwy (LBlockList a b) =
 let z = getIntf1Gtwy' b in
  if z /= Nothing then z else getIntf1Gtwy a

-- IFace2 Int CnstrList
getIntf1Gtwy' (IFace2 a b) =
 if a == 1
 then
  findCNSL "gateway" b 
 else
  Nothing

getIntf1Gtwy' _ = Nothing
---------------------

writeBuildIT h s = hPutStrLn h s

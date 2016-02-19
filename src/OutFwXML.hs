module OutFwXML where
import Treedef
import Util

import System.IO
import System.Exit
import Data.Maybe
import Data.List.Split

import Control.Monad

genfwxml tree = do
 obuildfwxmlbl tree

-------------------

obuildfwxmlbl (BlockList BEmpty b) = obuildfwxmlb b

obuildfwxmlbl (BlockList a b) = do
 obuildfwxmlbl a
 obuildfwxmlb b

-- FBlock1 String CnstrList IFaceList IRoute
obuildfwxmlb (FBlock1 a b c d) = do
 putStrLn ("BuildIT: Firewall " ++ a ++ " unsupported FBlock1")
 exitFailure

-- FBlock String CnstrList IFaceList FBlockList IRoute
obuildfwxmlb (FBlock a b c d e) = do
 h <- openXmlOut a

 writeXmlOut h "<?xml version=\"1.0\"?>"
 writeXmlOut h "<config version=\"7.0.0\" urldb=\"paloaltonetworks\">"

 writeXmlOut h " <mgt-config>"
 writeXmlOut h "  <users>"
 writeXmlOut h "   <entry name=\"admin\">"
 writeXmlOut h "   <phash>$1$eohmufte$eQM4h/DWy718Uyii2d54/0</phash>"
 writeXmlOut h "    <permissions>"
 writeXmlOut h "     <role-based>"
 writeXmlOut h "      <superuser>yes</superuser>"
 writeXmlOut h "     </role-based>"
 writeXmlOut h "    </permissions>"
 writeXmlOut h "   </entry>"
 writeXmlOut h "  </users>"
 writeXmlOut h " </mgt-config>"

 writeXmlOut h " <shared>"
 writeXmlOut h "  <application/>"
 writeXmlOut h "  <application-group/>"
 writeXmlOut h "  <service/>"
 writeXmlOut h "  <service-group/>"

 writeXmlOut h "  <botnet>"
 writeXmlOut h "   <configuration>"
 writeXmlOut h "    <http>"
 writeXmlOut h "     <dynamic-dns>"
 writeXmlOut h "      <enabled>yes</enabled>"
 writeXmlOut h "      <threshold>5</threshold>"
 writeXmlOut h "     </dynamic-dns>"
 writeXmlOut h "     <malware-sites>"
 writeXmlOut h "      <enabled>yes</enabled>"
 writeXmlOut h "      <threshold>5</threshold>"
 writeXmlOut h "     </malware-sites>"
 writeXmlOut h "     <recent-domains>"
 writeXmlOut h "      <enabled>yes</enabled>"
 writeXmlOut h "      <threshold>5</threshold>"
 writeXmlOut h "     </recent-domains>"
 writeXmlOut h "     <ip-domains>"
 writeXmlOut h "      <enabled>yes</enabled>"
 writeXmlOut h "      <threshold>10</threshold>"
 writeXmlOut h "     </ip-domains>"
 writeXmlOut h "     <executables-from-unknown-sites>"
 writeXmlOut h "      <enabled>yes</enabled>"
 writeXmlOut h "      <threshold>5</threshold>"
 writeXmlOut h "     </executables-from-unknown-sites>"
 writeXmlOut h "    </http>"
 writeXmlOut h "    <other-applications>"
 writeXmlOut h "     <irc>yes</irc>"
 writeXmlOut h "    </other-applications>"
 writeXmlOut h "    <unknown-applications>"
 writeXmlOut h "     <unknown-tcp>"
 writeXmlOut h "      <destinations-per-hour>10</destinations-per-hour>"
 writeXmlOut h "      <sessions-per-hour>10</sessions-per-hour>"
 writeXmlOut h "      <session-length>"
 writeXmlOut h "       <maximum-bytes>100</maximum-bytes>"
 writeXmlOut h "       <minimum-bytes>50</minimum-bytes>"
 writeXmlOut h "      </session-length>"
 writeXmlOut h "     </unknown-tcp>"
 writeXmlOut h "     <unknown-udp>"
 writeXmlOut h "      <destinations-per-hour>10</destinations-per-hour>"
 writeXmlOut h "      <sessions-per-hour>10</sessions-per-hour>"
 writeXmlOut h "      <session-length>"
 writeXmlOut h "       <maximum-bytes>100</maximum-bytes>"
 writeXmlOut h "       <minimum-bytes>50</minimum-bytes>"
 writeXmlOut h "      </session-length>"
 writeXmlOut h "     </unknown-udp>"
 writeXmlOut h "    </unknown-applications>"
 writeXmlOut h "   </configuration>"
 writeXmlOut h "   <report>"
 writeXmlOut h "    <topn>100</topn>"
 writeXmlOut h "    <scheduled>yes</scheduled>"
 writeXmlOut h "   </report>"
 writeXmlOut h "  </botnet>"
 writeXmlOut h "  <certificate>"
 writeXmlOut h "   <entry name=\"default\">"
 writeXmlOut h "   <subject-hash>b6451bdf</subject-hash>"
 writeXmlOut h "   <issuer-hash>b6451bdf</issuer-hash>"
 writeXmlOut h "   <not-valid-before>Oct 28 20:31:33 2015 GMT</not-valid-before>"
 writeXmlOut h "   <issuer>/CN=192.168.1.31</issuer>"
 writeXmlOut h "   <not-valid-after>Oct 27 20:31:33 2016 GMT</not-valid-after>"
 writeXmlOut h "   <common-name>192.168.1.31</common-name>"
 writeXmlOut h "   <expiry-epoch>1477600293</expiry-epoch>"
 writeXmlOut h "   <ca>yes</ca>"
 writeXmlOut h "   <subject>/CN=192.168.1.31</subject>"
 writeXmlOut h "   <public-key>-----BEGIN CERTIFICATE-----"
 writeXmlOut h "MIICzjCCAbagAwIBAgIJANzK2+lpafikMA0GCSqGSIb3DQEBCwUAMBcxFTATBgNV"
 writeXmlOut h "BAMTDDE5Mi4xNjguMS4zMTAeFw0xNTEwMjgyMDMxMzNaFw0xNjEwMjcyMDMxMzNa"
 writeXmlOut h "MBcxFTATBgNVBAMTDDE5Mi4xNjguMS4zMTCCASIwDQYJKoZIhvcNAQEBBQADggEP"
 writeXmlOut h "ADCCAQoCggEBALrzxBUtnrEfZkOQ3BHBmoJ5BqDv4r83dU3OQ3XJhsb6zmvX7+29"
 writeXmlOut h "aaQ7pdPnV6VkwxgqcNRybznkW4mMnuyiUfM11prrSozRV97jQe9O0rrla8AciVdT"
 writeXmlOut h "EmkHAriML4q4bC78TcQe1JpD+GG0YoLX3qaGc4Pyn8Yy1vJpxKHGWdiIvw+nlt16"
 writeXmlOut h "FZX3h/EpFfAE7sEbJfahY0ACRi1PzBfANeVYQIMQM1J7wr74Rbj1vh+S6ZztCfdj"
 writeXmlOut h "dTT28TwjmlEsqMk24Om1PnpPIGI9tq1nFBXJDRaHiSOS3S2lcKij8gLGeI6sGQN/"
 writeXmlOut h "ndoMlHs8vo1/5GfE3Zq7YF1DcAsrwUMcW9MCAwEAAaMdMBswDAYDVR0TBAUwAwEB"
 writeXmlOut h "/zALBgNVHQ8EBAMCAgQwDQYJKoZIhvcNAQELBQADggEBAIvml5jo1vS0occLLwZF"
 writeXmlOut h "knSQqchzw4XHvpbxtLGg/yjqtf0m0a8GR3YxwBNMi+n2MB0khRmb/JZC9cZXlRbC"
 writeXmlOut h "yAMAW8caHLtmrY2YJEXp6QVRC8n2WzGbQd1TpHxZ+xoVgqA8Vg1fkgMsm17On3B6"
 writeXmlOut h "5bpvtEtYu1VBdLJFGpY7GDBaPbGeu8cSzPc9NyykGROd1nl/1BDp5DJPy1cHHw7z"
 writeXmlOut h "CVdxUksHrHsyjPiMwKBzhjrHc1xAkoAWFqsnsUhAMCRYSuBWrUsLSCozWMBa3lQE"
 writeXmlOut h "mx9XqE5H9lwnrijaI5ZEv7urWZLu44iNWcJW2W+5QW9A6BzxT2njin+ea03zETsi"
 writeXmlOut h "vXk="
 writeXmlOut h "-----END CERTIFICATE-----"
 writeXmlOut h "</public-key>"
 writeXmlOut h " <algorithm>RSA</algorithm>"
 writeXmlOut h "<private-key>-AQ==16o+3W06hNIlRVrPDZBhEkPvKHY=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</private-key>"

 writeXmlOut h "   </entry>"
 writeXmlOut h "  </certificate>"
 writeXmlOut h "  <ssl-tls-service-profile>"
 writeXmlOut h "   <entry name=\"default (Provisioning)\">"
 writeXmlOut h "    <protocol-settings>"
 writeXmlOut h "     <min-version>tls1-0</min-version>"
 writeXmlOut h "     <max-version>max</max-version>"
 writeXmlOut h "    </protocol-settings>"
 writeXmlOut h "    <certificate>default</certificate>"
 writeXmlOut h "   </entry>"
 writeXmlOut h "  </ssl-tls-service-profile>"
--
-- added (no correlation to cfg file)

 writeXmlOut h "  <log-settings>"
 writeXmlOut h "   <profiles>"
 writeXmlOut h "    <entry name=\"default\">"
 writeXmlOut h "     <traffic>"
 writeXmlOut h "      <any>"
 writeXmlOut h "       <send-to-panorama>yes</send-to-panorama>"
 writeXmlOut h "      </any>"
 writeXmlOut h "     </traffic>"
 writeXmlOut h "    </entry>"
 writeXmlOut h "   </profiles>"
 writeXmlOut h "  </log-settings>"
 writeXmlOut h " </shared>"

 writeXmlOut h " <devices>"
 writeXmlOut h "  <entry name=\"localhost.localdomain\">"
 writeXmlOut h "  <network>"
 writeXmlOut h "   <interface>"
--
-- output the ethernet blocksfor each interface (except 0)
-- baseline has (no interfaces?): writeXmlOut h "    <ethernet/>"

 writeXmlOut h "    <ethernet>"
 obuildfwifbl h a c
 writeXmlOut h "    </ethernet>"

--
 writeXmlOut h "   </interface>"
 writeXmlOut h "   <profiles>"
 writeXmlOut h "    <monitor-profile>"
 writeXmlOut h "     <entry name=\"default\">"
 writeXmlOut h "      <interval>3</interval>"
 writeXmlOut h "      <threshold>5</threshold>"
 writeXmlOut h "      <action>wait-recover</action>"
 writeXmlOut h "     </entry>"
 writeXmlOut h "    </monitor-profile>"
 writeXmlOut h "   </profiles>"
 writeXmlOut h "   <ike>"
 writeXmlOut h "    <crypto-profiles>"
 writeXmlOut h "     <ike-crypto-profiles>"
 writeXmlOut h "      <entry name=\"default\">"
 writeXmlOut h "       <encryption>"
 writeXmlOut h "        <member>aes-128-cbc</member>"
 writeXmlOut h "        <member>3des</member>"
 writeXmlOut h "       </encryption>"
 writeXmlOut h "       <hash>"
 writeXmlOut h "        <member>sha1</member>"
 writeXmlOut h "       </hash>"
 writeXmlOut h "       <dh-group>"
 writeXmlOut h "        <member>group2</member>"
 writeXmlOut h "       </dh-group>"
 writeXmlOut h "       <lifetime>"
 writeXmlOut h "        <hours>8</hours>"
 writeXmlOut h "       </lifetime>"
 writeXmlOut h "      </entry>"
 writeXmlOut h "      <entry name=\"Suite-B-GCM-128\">"
 writeXmlOut h "       <encryption>"
 writeXmlOut h "        <member>aes-128-cbc</member>"
 writeXmlOut h "       </encryption>"
 writeXmlOut h "       <hash>"
 writeXmlOut h "        <member>sha256</member>"
 writeXmlOut h "       </hash>"
 writeXmlOut h "       <dh-group>"
 writeXmlOut h "        <member>group19</member>"
 writeXmlOut h "       </dh-group>"
 writeXmlOut h "       <lifetime>"
 writeXmlOut h "        <hours>8</hours>"
 writeXmlOut h "        </lifetime>"
 writeXmlOut h "      </entry>"
 writeXmlOut h "      <entry name=\"Suite-B-GCM-256\">"
 writeXmlOut h "       <encryption>"
 writeXmlOut h "        <member>aes-256-cbc</member>"
 writeXmlOut h "       </encryption>"
 writeXmlOut h "       <hash>"
 writeXmlOut h "        <member>sha384</member>"
 writeXmlOut h "       </hash>"
 writeXmlOut h "       <dh-group>"
 writeXmlOut h "        <member>group20</member>"
 writeXmlOut h "       </dh-group>"
 writeXmlOut h "       <lifetime>"
 writeXmlOut h "        <hours>8</hours>"
 writeXmlOut h "       </lifetime>"
 writeXmlOut h "      </entry>"
 writeXmlOut h "     </ike-crypto-profiles>"
 writeXmlOut h "     <ipsec-crypto-profiles>"
 writeXmlOut h "      <entry name=\"default\">"
 writeXmlOut h "       <esp>"
 writeXmlOut h "        <encryption>"
 writeXmlOut h "         <member>aes-128-cbc</member>"
 writeXmlOut h "         <member>3des</member>"
 writeXmlOut h "        </encryption>"
 writeXmlOut h "        <authentication>"
 writeXmlOut h "         <member>sha1</member>"
 writeXmlOut h "        </authentication>"
 writeXmlOut h "       </esp>"
 writeXmlOut h "       <dh-group>group2</dh-group>"
 writeXmlOut h "       <lifetime>"
 writeXmlOut h "        <hours>1</hours>"
 writeXmlOut h "       </lifetime>"
 writeXmlOut h "      </entry>"
 writeXmlOut h "      <entry name=\"Suite-B-GCM-128\">"
 writeXmlOut h "       <esp>"
 writeXmlOut h "        <encryption>"
 writeXmlOut h "         <member>aes-128-gcm</member>"
 writeXmlOut h "        </encryption>"
 writeXmlOut h "        <authentication>"
 writeXmlOut h "         <member>none</member>"
 writeXmlOut h "        </authentication>"
 writeXmlOut h "       </esp>"
 writeXmlOut h "       <dh-group>group19</dh-group>"
 writeXmlOut h "       <lifetime>"
 writeXmlOut h "        <hours>1</hours>"
 writeXmlOut h "       </lifetime>"
 writeXmlOut h "      </entry>"
 writeXmlOut h "      <entry name=\"Suite-B-GCM-256\">"
 writeXmlOut h "       <esp>"
 writeXmlOut h "        <encryption>"
 writeXmlOut h "         <member>aes-256-gcm</member>"
 writeXmlOut h "        </encryption>"
 writeXmlOut h "        <authentication>"
 writeXmlOut h "         <member>none</member>"
 writeXmlOut h "        </authentication>"
 writeXmlOut h "       </esp>"
 writeXmlOut h "       <dh-group>group20</dh-group>"
 writeXmlOut h "       <lifetime>"
 writeXmlOut h "        <hours>1</hours>"
 writeXmlOut h "       </lifetime>"
 writeXmlOut h "      </entry>"
 writeXmlOut h "     </ipsec-crypto-profiles>"
 writeXmlOut h "     <global-protect-app-crypto-profiles>"
 writeXmlOut h "      <entry name=\"default\">"
 writeXmlOut h "       <encryption>"
 writeXmlOut h "        <member>aes-128-cbc</member>"
 writeXmlOut h "       </encryption>"
 writeXmlOut h "       <authentication>"
 writeXmlOut h "        <member>sha1</member>"
 writeXmlOut h "       </authentication>"
 writeXmlOut h "      </entry>"
 writeXmlOut h "     </global-protect-app-crypto-profiles>"
 writeXmlOut h "    </crypto-profiles>"
 writeXmlOut h "   </ike>"
 writeXmlOut h "   <qos>"
 writeXmlOut h "    <profile>"
 writeXmlOut h "     <entry name=\"default\">"
 writeXmlOut h "      <class>"
 writeXmlOut h "       <entry name=\"class1\">"
 writeXmlOut h "        <priority>real-time</priority>"
 writeXmlOut h "       </entry>"
 writeXmlOut h "       <entry name=\"class2\">"
 writeXmlOut h "        <priority>high</priority>"
 writeXmlOut h "       </entry>"
 writeXmlOut h "       <entry name=\"class3\">"
 writeXmlOut h "        <priority>high</priority>"
 writeXmlOut h "       </entry>"
 writeXmlOut h "       <entry name=\"class4\">"
 writeXmlOut h "        <priority>medium</priority>"
 writeXmlOut h "       </entry>"
 writeXmlOut h "       <entry name=\"class5\">"
 writeXmlOut h "        <priority>medium</priority>"
 writeXmlOut h "       </entry>"
 writeXmlOut h "       <entry name=\"class6\">"
 writeXmlOut h "        <priority>low</priority>"
 writeXmlOut h "       </entry>"
 writeXmlOut h "       <entry name=\"class7\">"
 writeXmlOut h "        <priority>low</priority>"
 writeXmlOut h "       </entry>"
 writeXmlOut h "       <entry name=\"class8\">"
 writeXmlOut h "        <priority>low</priority>"
 writeXmlOut h "       </entry>"
 writeXmlOut h "      </class>"
 writeXmlOut h "     </entry>"
 writeXmlOut h "    </profile>"
 writeXmlOut h "   </qos>"
 writeXmlOut h "  <virtual-router>"
 writeXmlOut h "   <entry name=\"default\">"
 writeXmlOut h "    <protocol>"
 writeXmlOut h "     <bgp>"
 writeXmlOut h "      <enable>no</enable>"
 writeXmlOut h "      <dampening-profile>"
 writeXmlOut h "       <entry name=\"default\">"
 writeXmlOut h "        <cutoff>1.25</cutoff>"
 writeXmlOut h "        <reuse>0.5</reuse>"
 writeXmlOut h "        <max-hold-time>900</max-hold-time>"
 writeXmlOut h "        <decay-half-life-reachable>300</decay-half-life-reachable>"
 writeXmlOut h "        <decay-half-life-unreachable>900</decay-half-life-unreachable>"
 writeXmlOut h "        <enable>yes</enable>"
 writeXmlOut h "       </entry>"
 writeXmlOut h "      </dampening-profile>"
 writeXmlOut h "     </bgp>"
 writeXmlOut h "    </protocol>"
 writeXmlOut h "   </entry>"
 
 writeXmlOut h ("   <entry name=\"" ++ a ++ "\">")

 writeXmlOut h "    <ecmp>"
 writeXmlOut h "     <algorithm>"
 writeXmlOut h "      <ip-modulo/>"
 writeXmlOut h "     </algorithm>"
 writeXmlOut h "    </ecmp>"
 writeXmlOut h "    <protocol>"
 writeXmlOut h "     <bgp>"
 writeXmlOut h "      <routing-options>"
 writeXmlOut h "       <graceful-restart>"
 writeXmlOut h "        <enable>yes</enable>"
 writeXmlOut h "       </graceful-restart>"
 writeXmlOut h "       <as-format>2-byte</as-format>"
 writeXmlOut h "      </routing-options>"
 writeXmlOut h "      <enable>no</enable>"
 writeXmlOut h "     </bgp>"
 writeXmlOut h "    </protocol>"
--

 writeXmlOut h "    <interface>"
 obuildfwints  h a c
 writeXmlOut h "    </interface>"

--
 writeXmlOut h "    <routing-table>"
 writeXmlOut h "     <ip>"
--

 writeXmlOut h "      <static-route>"
 obuildrts  h a e
 writeXmlOut h "      </static-route>"

--
 writeXmlOut h "     </ip>"
 writeXmlOut h "    </routing-table>"
 writeXmlOut h "   </entry>"

 writeXmlOut h "  </virtual-router>"
 writeXmlOut h " </network>"
 writeXmlOut h " <deviceconfig>"
 writeXmlOut h "  <system>"
 writeXmlOut h "   <ip-address>192.168.1.31</ip-address>"
 writeXmlOut h "   <netmask>255.255.255.0</netmask>"
 writeXmlOut h "   <update-server>updates.paloaltonetworks.com</update-server>"
 writeXmlOut h "   <update-schedule>"
 writeXmlOut h "    <threats>"
 writeXmlOut h "     <recurring>"
 writeXmlOut h "      <weekly>"
 writeXmlOut h "       <day-of-week>wednesday</day-of-week>"
 writeXmlOut h "       <at>01:02</at>"
 writeXmlOut h "       <action>download-only</action>"
 writeXmlOut h "      </weekly>"
 writeXmlOut h "     </recurring>"
 writeXmlOut h "    </threats>"
 writeXmlOut h "   </update-schedule>"
 writeXmlOut h "   <timezone>US/Central</timezone>"
 writeXmlOut h "   <service>"
 writeXmlOut h "    <disable-telnet>yes</disable-telnet>"
 writeXmlOut h "    <disable-http>yes</disable-http>"
 writeXmlOut h "   </service>"
 writeXmlOut h "   <hostname>PA-VM</hostname>"
 writeXmlOut h "   <default-gateway>192.168.1.1</default-gateway>"
 writeXmlOut h "   <dns-setting>"
 writeXmlOut h "    <servers>"
 writeXmlOut h "     <primary>192.168.1.1</primary>"
 writeXmlOut h "    </servers>"
 writeXmlOut h "   </dns-setting>"
 writeXmlOut h "   <ntp-servers>"
 writeXmlOut h "    <primary-ntp-server>"
 writeXmlOut h "     <ntp-server-address>pool.ntp.org</ntp-server-address>"
 writeXmlOut h "     <authentication-type>"
 writeXmlOut h "      <none/>"
 writeXmlOut h "     </authentication-type>"
 writeXmlOut h "    </primary-ntp-server>"
 writeXmlOut h "   </ntp-servers>"
 writeXmlOut h "   <ssl-tls-service-profile>default (Provisioning)</ssl-tls-service-profile>"
 writeXmlOut h "  </system>"
 writeXmlOut h "  <setting>"
 writeXmlOut h "   <config>"
 writeXmlOut h "    <rematch>yes</rematch>"
 writeXmlOut h "   </config>"
 writeXmlOut h "   <management>"
 writeXmlOut h "    <hostname-type-in-syslog>FQDN</hostname-type-in-syslog>"
 writeXmlOut h "   </management>"
 writeXmlOut h "   <auto-mac-detect>yes</auto-mac-detect>"
 writeXmlOut h "  </setting>"
 writeXmlOut h " </deviceconfig>"
 writeXmlOut h " <vsys>"
 writeXmlOut h "  <entry name=\"vsys1\">"
 writeXmlOut h "   <application/>"
 writeXmlOut h "   <application-group/>"
--

 -- writeXmlOut h "   <zone/>" # this was "baseline"
 writeXmlOut h "   <zone>"
 obuildfwzonel h a d
 writeXmlOut h "   </zone>"
--
 -- writeXmlOut h "   <service/>" # this was "baseline"
 writeXmlOut h "   <service>"
 obuildfwservl h a d
 writeXmlOut h "   </service>"

--
 writeXmlOut h "   <service-group/>"
 writeXmlOut h "   <schedule/>"
--

 -- writeXmlOut h "   <rulebase/>" # this was "baseline"

 writeXmlOut h "   <rulebase>"

 if nattedl d == True
 then
  do
   writeXmlOut h "    <nat>"
   writeXmlOut h "     <rules>"
   obuildfwnats h a d
   writeXmlOut h "     </rules>"
   writeXmlOut h "    </nat>"
 else
  return ()

 writeXmlOut h "    <security>"
 writeXmlOut h "     <rules>"

 obuildfwpolicys h a d

 writeXmlOut h "     </rules>"
 writeXmlOut h "    </security>"

 writeXmlOut h "   </rulebase>"

 -- this section is for address objects

 writeXmlOut h "   <address>"
 obuildfwadxobjs h a d
 obuildfwadxints h a c
 writeXmlOut h "   </address>"

 writeXmlOut h "   <import>"
 writeXmlOut h "    <network>"
 writeXmlOut h "     <interface>"
 obuildfwints1 h a c
 writeXmlOut h "     </interface>"
 writeXmlOut h "    </network>"
 writeXmlOut h "   </import>"

--
 writeXmlOut h "  </entry>"
 writeXmlOut h " </vsys>"
 writeXmlOut h "</entry>"
 writeXmlOut h "</devices>"
 writeXmlOut h "</config>"

 closeXmlOut h

obuildfwxmlb _ = do
 return ()

-----------------------------

obuildfwifbl h a (IFaceList IFaceEmpty c) = obuildfwifb h a c

obuildfwifbl h a (IFaceList b c) = do
 obuildfwifbl h a b
 obuildfwifb h a c

-- IFace1 Int CnstrList
obuildfwifb h a (IFace1 b c) = do
 if b == 0
 then
  return ()
 else
  do
   writeXmlOut h ("     <entry name=\"ethernet1/" ++ (show b) ++ "\">")
   writeXmlOut h "      <layer3>"
   writeXmlOut h "       <ipv6>"
   writeXmlOut h "        <neighbor-discovery>"
   writeXmlOut h "         <router-advertisement>"
   writeXmlOut h "          <enable>no</enable>"
   writeXmlOut h "         </router-advertisement>"
   writeXmlOut h "        </neighbor-discovery>"
   writeXmlOut h "       </ipv6>"
   writeXmlOut h "       <ndp-proxy>"
   writeXmlOut h "        <enabled>no</enabled>"
   writeXmlOut h "       </ndp-proxy>"
   writeXmlOut h "       <lldp>"
   writeXmlOut h "        <enable>no</enable>"
   writeXmlOut h "       </lldp>"
   writeXmlOut h "       <ip>"

   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Firewall " ++ a ++
               " interface " ++ (show b) ++ " has no network")
     exitFailure
   else
    do
     let y = fromJust z
     writeXmlOut h ("        <entry name=\"" ++ y ++ "\"/>")

   writeXmlOut h "       </ip>"
   writeXmlOut h "      </layer3>"

   let z = findCNSL "description" c
   if z == Nothing
   then
    do
     return ()
   else
    do
     let y = fromJust z
     writeXmlOut h("      <comment>" ++ y ++ "</comment>")

   writeXmlOut h "     </entry>"

------------------------------

obuildfwints h a (IFaceList IFaceEmpty c) = obuildfwint h a c

obuildfwints h a (IFaceList b c) = do
 obuildfwints h a b
 obuildfwint h a c

-- IFace1 Int CnstrList
obuildfwint h a (IFace1 b c) = do
 if b == 0
 then
  return ()
 else
  do
   writeXmlOut h ("     <member>ethernet1/" ++ (show b) ++ "</member>")

------------------------------

obuildfwints1 h a (IFaceList IFaceEmpty c) = obuildfwint1 h a c

obuildfwints1 h a (IFaceList b c) = do
 obuildfwints1 h a b
 obuildfwint1 h a c

-- IFace1 Int CnstrList
obuildfwint1 h a (IFace1 b c) = do
 if b == 0
 then
  return ()
 else
  do
   writeXmlOut h ("      <member>ethernet1/" ++ (show b) ++ "</member>")

------------------------------

-- IRoute CnstrList
obuildrts  h a (IRoute1 c) = obuildrtl h a c

obuildrtl h a (CnstrList Empty c) = obuildrt h a c

obuildrtl h a (CnstrList b c) = do
 obuildrtl h a b
 obuildrt h a c

obuildrt h a (Cnstr3 b c) = do
 writeXmlOut h ("       <entry name=\"" ++ b ++ "\">")
 writeXmlOut h  "        <nexthop>"
 let z = splitOn " " c
 if length z /= 2
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++ " route " ++ b ++
             ": \"" ++ c ++  "\" doesn't have next/destination")
   exitFailure
 else
  do
   writeXmlOut h  ("         <ip-address>"  ++ (z!!1) ++ "</ip-address>")
   writeXmlOut h  "        </nexthop>"
   writeXmlOut h  "        <metric>10</metric>"
   writeXmlOut h  ("        <destination>" ++ (z!!0) ++ "</destination>")
   writeXmlOut h  "       </entry>"

obuildrt h a _ = do
 return ()

------------------------------

obuildfwzonel h a (FBlockList FBEmpty c) = obuildfwzone h a c

obuildfwzonel h a (FBlockList b c) = do
 obuildfwzonel  h a b
 obuildfwzone  h a c

obuildfwzone h a (IZone1 b c) = do
 writeXmlOut h ("    <entry name=\"" ++ b ++ "\">")
 writeXmlOut h  "     <network>"
 let z = findCNSL "type" c
 if z == Nothing
 then
  do
   writeXmlOut h ("      <layer3>")
 else
  do
   let y = fromJust z
   writeXmlOut h ("      <" ++ y ++ ">")

 let z' = findCNSL "member" c
 if z' == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
               " zone " ++ b ++ " has no \"member\" constraint")
   exitFailure
 else
  do
   let y' = fromJust z'
   writeXmlOut h ("       <member>ethernet1/" ++ y' ++ "</member>")

 if z == Nothing
 then
  do
   writeXmlOut h ("      </layer3>")
 else
  do
   let y = fromJust z
   writeXmlOut h ("      </" ++ y ++ ">")

 let z = findCNSL "logsetting" c
 if z == Nothing
 then
  do
   writeXmlOut h "      <log-setting>default</log-setting>"
 else
  do
   let y = fromJust z
   writeXmlOut h ("      <log-setting>" ++ y ++ "</log-setting>")
   
 writeXmlOut h  "     </network>"
 writeXmlOut h  "    </entry>"

obuildfwzone h a _ = do
 return ()

------------------------------

obuildfwservl h a (FBlockList FBEmpty c) = obuildfwserv h a c

obuildfwservl h a (FBlockList b c) = do
 obuildfwservl  h a b
 obuildfwserv  h a c

obuildfwserv h a (IServ1 b c) = do
 writeXmlOut h ("    <entry name=\"" ++ b ++ "\">")
 writeXmlOut h ("     <protocol>")
 let z = findCNSL "protocol" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
               " service " ++ b ++ " has no protocol constraint")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h ("      <" ++ y ++ ">")
   let z = findCNSL "port" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Firewall " ++ a ++
                 " service " ++ b ++ " has no port constraint")
     exitFailure
   else
    do
     let y = fromJust z
     writeXmlOut h ("       <port>" ++ y ++ "</port>")

   writeXmlOut h ("      </" ++ y ++ ">")

 writeXmlOut h ("     </protocol>")
 writeXmlOut h ("    </entry>")

obuildfwserv h a _ = do
 return ()

------------------------------

-- is there any snat or dnat??
nattedl (FBlockList FBEmpty b) = natted b
nattedl (FBlockList a b) = if natted b == True then True else nattedl a

natted (ISnat1 _ _) = True
natted (IDnat1 _ _) = True
natted _ = False

------------------------------

obuildfwadxobjs h a (FBlockList FBEmpty c) = obuildfwadxobj h a c

obuildfwadxobjs h a (FBlockList b c) = do
 obuildfwadxobjs h a b 
 obuildfwadxobj h a c

obuildfwadxobj h a (IAdxobj1 b c) = do
 writeXmlOut h ("    <entry name=\"" ++ b ++ "\">")
 let z = findCNSL "type" c
 if z == Nothing
 then
  do
   let ty = "ip-netmask"
   let z = findCNSL "ip" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Firewall " ++ a ++
                 " adxobj " ++ b ++ " has no ip constraint")
     exitFailure
    else
    do
     let y = fromJust z
     writeXmlOut h ("     <" ++ ty ++ ">" ++ y ++ "</" ++ ty ++ ">")
 else
  do
   let ty = fromJust z
   let z = findCNSL "ip" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Firewall " ++ a ++
                 " adxobj " ++ b ++ " has no ip constraint")
     exitFailure
    else
    do
     let y = fromJust z
     writeXmlOut h ("     <" ++ ty ++ ">" ++ y ++ "</" ++ ty ++ ">")

 writeXmlOut h "    </entry>"

obuildfwadxobj h a _ = do
 return ()

------------------------------

obuildfwadxints h a (IFaceList IFaceEmpty c) = obuildfwint h a c

obuildfwadxints h a (IFaceList b c) = do
 obuildfwadxints h a b 
 obuildfwadxint h a c
   
-- IFace1 Int CnstrList
obuildfwadxint h a (IFace1 b c) = do
 if b == 0
 then
  return ()
 else
  do
   let z = findCNSL "network" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Firewall " ++ a ++
               " interface " ++ (show b) ++ " has no network")
     exitFailure
   else
    do
     let y = fromJust z
     writeXmlOut h ("    <entry name=\"" ++ y ++ "\">")

   let z = findCNSL "ip" c
   if z == Nothing
   then
    do
     putStrLn ("BuildIT: Firewall " ++ a ++
               " interface " ++ (show b) ++ " has no ip")
     exitFailure
   else
    do
     let y = fromJust z
     writeXmlOut h ("     <ip-netmask>" ++ y ++ "</ip-netmask>")

   writeXmlOut h "    </entry>"
   
------------------------------
obuildfwpolicys h a (FBlockList FBEmpty c) = obuildfwpolicy h a c

obuildfwpolicys h a (FBlockList b c) =  do
 obuildfwpolicys h a b
 obuildfwpolicy h a c

obuildfwpolicy h a (IPolicy1 b c) = do
 writeXmlOut h ("      <entry name=\"" ++ b ++ "\">")
 let z = findCNSL "to_zone" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++ " policy " ++ b ++ " no to_zone")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "       <to>"
   writeXmlOut h ("        <member>" ++ y ++ "</member>")
   writeXmlOut h  "       </to>"

 let z = findCNSL "from_zone" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++ " policy " ++ b ++ " no from_zone")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "       <from>"
   writeXmlOut h ("        <member>" ++ y ++ "</member>")
   writeXmlOut h  "       </from>"

 let z = findCNSL "source" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++ " policy " ++ b ++ " no source")
   exitFailure
 else
  do
   let y = fromJust z
   let y' = splitOn "," y
   writeXmlOut h  "       <source>"
   forM_ y' $ \s -> do
    writeXmlOut h ("        <member>" ++ s ++ "</member>")
   writeXmlOut h  "       </source>"

 let z = findCNSL "destination" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++ " policy " ++ b ++ " no destination")
   exitFailure
 else
  do
   let y = fromJust z
   let y' = splitOn "," y
   writeXmlOut h  "       <destination>"
   forM_ y' $ \s -> do
    writeXmlOut h ("        <member>" ++ s ++ "</member>")
   writeXmlOut h  "       </destination>"

 let z = findCNSL "source-user" c
 if z == Nothing
 then
  do
   writeXmlOut h  "       <source-user>"
   writeXmlOut h ("        <member>any</member>")
   writeXmlOut h  "       </source-user>"
 else
  do
   let y = fromJust z
   writeXmlOut h  "       <source-user>"
   writeXmlOut h ("        <member>" ++ y ++ "</member>")
   writeXmlOut h  "       </source-user>"

 let z = findCNSL "category" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++ " policy " ++ b ++ " no category")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "       <category>"
   writeXmlOut h ("        <member>" ++ y ++ "</member>")
   writeXmlOut h  "       </category>"

 let z = findCNSL "application" c
 if z == Nothing
 then
  do
   writeXmlOut h  "       <application>"
   writeXmlOut h ("        <member>any</member>")
   writeXmlOut h  "       </application>"
 else
  do -- application can have multiple elements separated by ','
   let y = fromJust z
   let y' = splitOn "," y
   writeXmlOut h  "       <application>"

   forM_ y' $ \s -> do
    writeXmlOut h ("        <member>" ++ s ++ "</member>")

   writeXmlOut h  "       </application>"

 let z = findCNSL "appservice" c
 if z == Nothing
 then
  do
   writeXmlOut h  "       <service>"
   writeXmlOut h ("        <member>any</member>")
   writeXmlOut h  "       </service>"
 else
  do
   let y = fromJust z
   let y' = splitOn "," y
   writeXmlOut h  "       <service>"

   forM_ y' $ \s -> do
    writeXmlOut h ("        <member>" ++ s ++ "</member>")

   writeXmlOut h  "       </service>"

 let z = findCNSL "hip-profiles" c
 if z == Nothing
 then
  do
   writeXmlOut h  "       <hip-profiles>"
   writeXmlOut h ("        <member>any</member>")
   writeXmlOut h  "       </hip-profiles>"
 else
  do
   let y = fromJust z
   writeXmlOut h  "       <hip-profiles>"
   writeXmlOut h ("        <member>" ++ y ++ "</member>")
   writeXmlOut h  "       </hip-profiles>"

 let z = findCNSL "action" c
 if z == Nothing
 then
  do
   writeXmlOut h ("       <action>allow</action>")
 else
  do
   let y = fromJust z
   writeXmlOut h ("       <action>" ++ y ++ "</action>")

 let z = findCNSL "log-start" c
 if z == Nothing
 then
  do
   writeXmlOut h  "       <log-start>yes</log-start>"
 else
  do
   let y = fromJust z
   writeXmlOut h ("       <log-start>" ++ y ++ "</log-start>")

 let z = findCNSL "log-setting" c
 if z == Nothing
 then
  do
   writeXmlOut h ("       <log-setting>default</log-setting>")
 else
  do
   let y = fromJust z
   writeXmlOut h ("       <log-setting>" ++ y ++ "</log-setting>")

 writeXmlOut h  "      </entry>"
 
obuildfwpolicy h a _ = do
 return ()

------------------------------

obuildfwnats h a (FBlockList FBEmpty c) = obuildfwnat h a c

obuildfwnats h a (FBlockList b c) = do
 obuildfwnats h a b
 obuildfwnat h a c

obuildfwnat h a (IDnat1 b c) = do
 writeXmlOut h ("      <entry name=\"" ++ b ++ "\">")
 writeXmlOut h  "       <source-translation>"
 writeXmlOut h  "        <static-ip>"
 writeXmlOut h  "         <bi-directional>yes</bi-directional>"

 let z = findCNSL "translated_address" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " dnat " ++  b ++ " has no translated_address")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h ("         <translated-address>" ++ y ++
                    "</translated-address>")

 writeXmlOut h  "        </static-ip>"
 writeXmlOut h  "       </source-translation>"

 let z = findCNSL "to_zone" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " dnat " ++  b ++ " has no to_zone")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "        <to>"
   writeXmlOut h ("         <member>" ++ y ++ "</member>")
   writeXmlOut h  "        </to>"

 let z = findCNSL "from_zone" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " dnat " ++  b ++ " has no from_zone")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "        <from>"
   writeXmlOut h ("         <member>" ++ y ++ "</member>")
   writeXmlOut h  "        </from>"

 let z = findCNSL "source" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " dnat " ++  b ++ " has no source")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "        <source>"
   writeXmlOut h ("         <member>" ++ y ++ "</member>")
   writeXmlOut h  "        </source>"

 let z = findCNSL "destination" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " dnat " ++  b ++ " has no destination")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "        <destination>"
   writeXmlOut h ("         <member>" ++ y ++ "</member>")
   writeXmlOut h  "        </destination>"

 let z = findCNSL "appservice" c
 if z == Nothing
 then
  do
   writeXmlOut h  "       <service>any</service>"
--   writeXmlOut h  "       <service>"
--   writeXmlOut h ("        <member>any</member>")
--   writeXmlOut h  "       </service>"
 else
  do
   let y = fromJust z
--   let y' = splitOn "," y
   writeXmlOut h  ("       <service>" ++ y ++ "</service>")

--   forM_ y' $ \s -> do
--    writeXmlOut h ("        <member>" ++ s ++ "</member>")

--   writeXmlOut h  "       </service>"

 let z = findCNSL "description" c
 if z == Nothing
 then
  do
   return ()
 else
  do
   let y = fromJust z
   writeXmlOut h ("        <description>" ++ y ++ "</description>")

 writeXmlOut h "      </entry>"

obuildfwnat h a (ISnat1 b c) = do
 writeXmlOut h ("      <entry name=\"" ++ b ++ "\">")
 writeXmlOut h  "       <source-translation>"
 writeXmlOut h  "        <dynamic-ip-and-port>"

 let z = findCNSL "translated_address" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " snat " ++  b ++ " has no translated_address")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h ("         <translated-address>")
   writeXmlOut h ("          <member>" ++ y ++ "</member>")
   writeXmlOut h ("         </translated-address>")

 writeXmlOut h  "        </dynamic-ip-and-port>"
 writeXmlOut h  "       </source-translation>"

 let z = findCNSL "to_zone" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " snat " ++  b ++ " has no to_zone")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "        <to>"
   writeXmlOut h ("         <member>" ++ y ++ "</member>")
   writeXmlOut h  "        </to>"

 let z = findCNSL "from_zone" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " snat " ++  b ++ " has no from_zone")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "        <from>"
   writeXmlOut h ("         <member>" ++ y ++ "</member>")
   writeXmlOut h  "        </from>"

 let z = findCNSL "source" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " snat " ++  b ++ " has no source")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "        <source>"
   writeXmlOut h ("         <member>" ++ y ++ "</member>")
   writeXmlOut h  "        </source>"

 let z = findCNSL "destination" c
 if z == Nothing
 then
  do
   putStrLn ("BuildIT: Firewall " ++ a ++
             " snat " ++  b ++ " has no destination")
   exitFailure
 else
  do
   let y = fromJust z
   writeXmlOut h  "        <destination>"
   writeXmlOut h ("         <member>" ++ y ++ "</member>")
   writeXmlOut h  "        </destination>"

 let z = findCNSL "appservice" c
 if z == Nothing
 then
  do
   writeXmlOut h  "       <service>any</service>"
   -- writeXmlOut h  "       <service>"
   -- writeXmlOut h ("        <member>any</member>")
   -- writeXmlOut h  "       </service>"
 else
  do
   let y = fromJust z
   let y' = splitOn "," y
   if length y' == 1
   then
    do
     writeXmlOut h  ("       <service>" ++ y'!!0 ++ "</service>")
   else
    do
     writeXmlOut h  "       <service>"
     forM_ y' $ \s -> do
      writeXmlOut h ("        <member>" ++ s ++ "</member>")
     writeXmlOut h  "       </service>"

 let z = findCNSL "description" c
 if z == Nothing
 then
  do
   return ()
 else
  do
   let y = fromJust z
   writeXmlOut h ("        <description>" ++ y ++ "</description>")

 writeXmlOut h "      </entry>"

obuildfwnat h a _ = do
 return()

------------------------------

openXmlOut f = openFile (f ++ ".xml") WriteMode
closeXmlOut h = hClose h
writeXmlOut h s = hPutStrLn h s

-------------------------------


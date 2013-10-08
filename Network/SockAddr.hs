-- | Converting an address in 'SockAddr'.

module Network.SockAddr (
    showSockAddr
  , showSockAddrBS
  ) where

import Data.Bits (shift, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (pack)
import Network.Socket (SockAddr(..), HostAddress, HostAddress6)
import System.ByteOrder
import Text.Printf

isReversed :: Bool
isReversed = byteOrder == LittleEndian

----------------------------------------------------------------

-- | Convert 'SockAddr' to 'String'. If the address is
--   an IPv4-embedded IPv6 address, the IPv4 is extracted.
--
-- >>> import Network.Socket
-- >>> as <- getAddrInfo (Just defaultHints) (Just "example.org") (Just "http")
-- >>> map (showSockAddr.addrAddress) as
-- ["93.184.216.119","93.184.216.119","2606:2800:220:6d:26bf:1447:1097:aa7","2606:2800:220:6d:26bf:1447:1097:aa7"]
showSockAddr :: SockAddr -> String
showSockAddr (SockAddrInet _ addr4)                       = showIPv4 addr4 isReversed
showSockAddr (SockAddrInet6 _ _ (0,0,0x0000ffff,addr4) _) = showIPv4 addr4 False
showSockAddr (SockAddrInet6 _ _ (0,0,0,1) _)              = "::1"
showSockAddr (SockAddrInet6 _ _ addr6 _)                  = showIPv6 addr6
showSockAddr _                                            = "unknownSocket"

----------------------------------------------------------------

-- HostAddress is network byte order.
showIPv4 :: HostAddress -> Bool-> String
showIPv4 w32 reversed
  | reversed  = show b1 ++ "." ++ show b2 ++ "." ++ show b3 ++ "." ++ show b4
  | otherwise = show b4 ++ "." ++ show b3 ++ "." ++ show b2 ++ "." ++ show b1
  where
    t1 = w32
    t2 = shift t1 (-8)
    t3 = shift t2 (-8)
    t4 = shift t3 (-8)
    b1 = t1 .&. 0x000000ff
    b2 = t2 .&. 0x000000ff
    b3 = t3 .&. 0x000000ff
    b4 = t4 .&. 0x000000ff

-- HostAddress6 is host byte order.
showIPv6 :: HostAddress6 -> String
showIPv6 (w1,w2,w3,w4) =
    printf "%x:%x:%x:%x:%x:%x:%x:%x" s1 s2 s3 s4 s5 s6 s7 s8
  where
    (s1,s2) = split16 w1
    (s3,s4) = split16 w2
    (s5,s6) = split16 w3
    (s7,s8) = split16 w4
    split16 w = (h1,h2)
      where
        h1 = shift w (-16) .&. 0x0000ffff
        h2 = w .&. 0x0000ffff

----------------------------------------------------------------

-- | Convert 'SockAddr' to 'ByteString'. If the address is
--   an IPv4-embedded IPv6 address, the IPv4 is extracted.
--
-- >>> import Network.Socket
-- >>> as <- getAddrInfo (Just defaultHints) (Just "localhost") (Just "http")
-- >>> map (showSockAddrBS.addrAddress) as
-- ["127.0.0.1","::1","fe80:0:0:0:0:0:0:1"]

showSockAddrBS :: SockAddr -> ByteString
showSockAddrBS = BS.pack . showSockAddr

-- | Converting an address in 'SockAddr'.

module Network.SockAddr (
    showSockAddr
  , showSockAddrBS
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.List (intersperse)
import Data.Word (Word32)
import Network.Socket (SockAddr(..), HostAddress, HostAddress6)
import System.ByteOrder

isReversed :: Bool
isReversed = byteOrder == LittleEndian

----------------------------------------------------------------

-- | Convert 'SockAddr' to 'String'. If the address is
--   an IPv4-embedded IPv6 address, the IPv4 is extracted.
--
-- >>> import Network.Socket
-- >>> showSockAddr $ SockAddrInet 80 (tupleToHostAddress (192,0,2,1))
-- "192.0.2.1"
-- >>> showSockAddr $ SockAddrInet6 80 0 (tupleToHostAddress6 (0x2001,0xdb8,1,2,3,4,5,6)) 0
-- "2001:db8:1:2:3:4:5:6"
showSockAddr :: SockAddr -> String
showSockAddr = C8.unpack . showSockAddrBS

-- | Convert 'SockAddr' to 'ByteString'. If the address is
--   an IPv4-embedded IPv6 address, the IPv4 is extracted.
--
-- >>> import Network.Socket
-- >>> showSockAddrBS $ SockAddrInet6 80 0 (tupleToHostAddress6 (0,0,0,0,0,0,0,1)) 0
-- "::1"
showSockAddrBS :: SockAddr -> ByteString
showSockAddrBS = BL.toStrict . B.toLazyByteString . sockAddrBuilder

----------------------------------------------------------------

sockAddrBuilder :: SockAddr -> B.Builder
sockAddrBuilder (SockAddrInet _ addr4)                       = buildIPv4 addr4 isReversed
sockAddrBuilder (SockAddrInet6 _ _ (0,0,0x0000ffff,addr4) _) = buildIPv4 addr4 False
sockAddrBuilder (SockAddrInet6 _ _ (0,0,0,1) _)              = B.string7 "::1"
sockAddrBuilder (SockAddrInet6 _ _ addr6 _)                  = buildIPv6 addr6
sockAddrBuilder _                                            = B.string7 "unknownSocket"

-- HostAddress is network byte order.
buildIPv4 :: HostAddress -> Bool -> B.Builder
buildIPv4 w32 reversed = mconcat $ intersperse (B.char7 '.') $ map B.word32Dec bs
  where
    bs0 = [w32 .&. 0xff, (w32 `shiftR` 8) .&. 0xff, (w32 `shiftR` 16) .&. 0xff, w32 `shiftR` 24]
    bs | reversed  = bs0
       | otherwise = reverse bs0

-- HostAddress6 is host byte order.
buildIPv6 :: HostAddress6 -> B.Builder
buildIPv6 (w1,w2,w3,w4) =
    mconcat $ intersperse (B.char7 ':') $ map B.word32Hex $ concatMap split16 [w1,w2,w3,w4]
  where
    split16 :: Word32 -> [Word32]
    split16 w = [w `shiftR` 16, w .&. 0xffff]

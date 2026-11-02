-- | Converting the address part of 'SockAddr' (without port).

module Network.SockAddr (
    showSockAddr
  , showSockAddrBS
  ) where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.List (intersperse)
import Data.Word (Word8)
import Network.Socket (
    HostAddress6,
    SockAddr (..),
    hostAddress6ToTuple,
    hostAddressToTuple,
 )

----------------------------------------------------------------

-- | Convert the address part of 'SockAddr' to 'String'.
--   The port number is not included. If the address is
--   an IPv4-mapped IPv6 address (@::ffff:0:0/96@),
--   it is shown as IPv4.
--
-- >>> import Network.Socket
-- >>> showSockAddr $ SockAddrInet 80 (tupleToHostAddress (192,0,2,1))
-- "192.0.2.1"
-- >>> showSockAddr $ SockAddrInet6 80 0 (tupleToHostAddress6 (0x2001,0xdb8,1,2,3,4,5,6)) 0
-- "2001:db8:1:2:3:4:5:6"
showSockAddr :: SockAddr -> String
showSockAddr = C8.unpack . showSockAddrBS

-- | Convert the address part of 'SockAddr' to 'ByteString'.
--   The port number is not included. If the address is
--   an IPv4-mapped IPv6 address (@::ffff:0:0/96@),
--   it is shown as IPv4.
--
-- >>> import Network.Socket
-- >>> showSockAddrBS $ SockAddrInet6 80 0 (tupleToHostAddress6 (0,0,0,0,0,0,0,1)) 0
-- "::1"
showSockAddrBS :: SockAddr -> ByteString
showSockAddrBS = BL.toStrict . B.toLazyByteString . sockAddrBuilder

----------------------------------------------------------------

sockAddrBuilder :: SockAddr -> B.Builder
sockAddrBuilder (SockAddrInet _ addr4) = buildIPv4 $ hostAddressToTuple addr4
sockAddrBuilder (SockAddrInet6 _ _ addr6 _) = case hostAddress6ToTuple addr6 of
    (0, 0, 0, 0, 0, 0xffff, h, l) -> buildIPv4 (hi h, lo h, hi l, lo l)
    (0, 0, 0, 0, 0, 0, 0, 1) -> B.string7 "::1"
    _ -> buildIPv6 addr6
  where
    hi w = fromIntegral (w `shiftR` 8)
    lo w = fromIntegral w
sockAddrBuilder _ = B.string7 "unknownSocket"

buildIPv4 :: (Word8, Word8, Word8, Word8) -> B.Builder
buildIPv4 (a, b, c, d) = mconcat $ intersperse (B.char7 '.') $ map B.word8Dec [a, b, c, d]

buildIPv6 :: HostAddress6 -> B.Builder
buildIPv6 addr6 = mconcat $ intersperse (B.char7 ':') $ map B.word16Hex [a, b, c, d, e, f, g, h]
  where
    (a, b, c, d, e, f, g, h) = hostAddress6ToTuple addr6

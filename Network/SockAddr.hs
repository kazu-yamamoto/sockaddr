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
import Data.Word (Word16, Word8)
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
--   it is shown as IPv4. A non-zero scope ID of IPv6 is appended
--   as a number, e.g. @fe80::1%4@.
--   For 'SockAddrUnix', its path is returned.
--
-- >>> import Network.Socket
-- >>> showSockAddr $ SockAddrInet 80 (tupleToHostAddress (192,0,2,1))
-- "192.0.2.1"
-- >>> showSockAddr $ SockAddrInet6 80 0 (tupleToHostAddress6 (0x2001,0xdb8,1,2,3,4,5,6)) 0
-- "2001:db8:1:2:3:4:5:6"
-- >>> showSockAddr $ SockAddrInet6 80 0 (tupleToHostAddress6 (0x2001,0xdb8,0,0,1,0,0,1)) 0
-- "2001:db8::1:0:0:1"
showSockAddr :: SockAddr -> String
showSockAddr (SockAddrUnix path) = path
showSockAddr sa = C8.unpack $ showSockAddrBS sa

-- | Convert the address part of 'SockAddr' to 'ByteString'.
--   The port number is not included. If the address is
--   an IPv4-mapped IPv6 address (@::ffff:0:0/96@),
--   it is shown as IPv4. A non-zero scope ID of IPv6 is appended
--   as a number, e.g. @fe80::1%4@.
--   For 'SockAddrUnix', its path is encoded in UTF-8.
--
-- >>> import Network.Socket
-- >>> showSockAddrBS $ SockAddrInet6 80 0 (tupleToHostAddress6 (0,0,0,0,0,0,0,1)) 0
-- "::1"
showSockAddrBS :: SockAddr -> ByteString
showSockAddrBS = BL.toStrict . B.toLazyByteString . sockAddrBuilder

----------------------------------------------------------------

sockAddrBuilder :: SockAddr -> B.Builder
sockAddrBuilder (SockAddrInet _ addr4) = buildIPv4 $ hostAddressToTuple addr4
sockAddrBuilder (SockAddrInet6 _ _ addr6 scope) = case hostAddress6ToTuple addr6 of
    (0, 0, 0, 0, 0, 0xffff, h, l) -> buildIPv4 (hi h, lo h, hi l, lo l)
    _ -> buildIPv6 addr6 <> zone
  where
    zone
        | scope == 0 = mempty
        | otherwise = B.char7 '%' <> B.word32Dec scope
    hi w = fromIntegral (w `shiftR` 8)
    lo w = fromIntegral w
sockAddrBuilder (SockAddrUnix path) = B.stringUtf8 path

buildIPv4 :: (Word8, Word8, Word8, Word8) -> B.Builder
buildIPv4 (a, b, c, d) = mconcat $ intersperse (B.char7 '.') $ map B.word8Dec [a, b, c, d]

-- RFC 5952: the longest (first if tied) run of two or more
-- zero fields is compressed to "::".
buildIPv6 :: HostAddress6 -> B.Builder
buildIPv6 addr6 = case longestZeros ws of
    Just (i, n) -> hexes (take i ws) <> B.string7 "::" <> hexes (drop (i + n) ws)
    Nothing -> hexes ws
  where
    (a, b, c, d, e, f, g, h) = hostAddress6ToTuple addr6
    ws = [a, b, c, d, e, f, g, h]
    hexes = mconcat . intersperse (B.char7 ':') . map B.word16Hex

-- Returns (start index, length) of the zero run to be compressed.
longestZeros :: [Word16] -> Maybe (Int, Int)
longestZeros ws = case runs of
    [] -> Nothing
    _ -> Just $ foldr1 (\x y -> if snd y > snd x then y else x) runs
  where
    runs = [(i, n) | (i, n) <- go 0 ws, n >= 2]
    go _ [] = []
    go i xs@(0 : _) = let n = length (takeWhile (== 0) xs) in (i, n) : go (i + n) (drop n xs)
    go i (_ : xs) = go (i + 1) xs

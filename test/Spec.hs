module Main (main) where

import Data.Word (Word16, Word8)
import Network.SockAddr
import Network.Socket
import Test.Hspec

v4 :: (Word8, Word8, Word8, Word8) -> SockAddr
v4 t = SockAddrInet 80 (tupleToHostAddress t)

v6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> SockAddr
v6 t = SockAddrInet6 80 0 (tupleToHostAddress6 t) 0

main :: IO ()
main = hspec $ do
    describe "showSockAddr" $ do
        it "shows IPv4" $ do
            showSockAddr (v4 (127, 0, 0, 1)) `shouldBe` "127.0.0.1"
            showSockAddr (v4 (192, 0, 2, 255)) `shouldBe` "192.0.2.255"
        it "shows IPv4-mapped IPv6 as IPv4" $
            showSockAddr (v6 (0, 0, 0, 0, 0, 0xffff, 0xc000, 0x0201))
                `shouldBe` "192.0.2.1"
        it "shows IPv6 loopback" $
            showSockAddr (v6 (0, 0, 0, 0, 0, 0, 0, 1)) `shouldBe` "::1"
        it "shows IPv6" $
            showSockAddr (v6 (0x2001, 0xdb8, 1, 2, 3, 4, 5, 6))
                `shouldBe` "2001:db8:1:2:3:4:5:6"
        it "appends non-zero scope ID" $
            showSockAddr (SockAddrInet6 80 0 (tupleToHostAddress6 (0xfe80, 0, 0, 0, 0, 0, 0, 0x1234)) 4)
                `shouldBe` "fe80:0:0:0:0:0:0:1234%4"
        it "shows Unix domain socket path" $
            showSockAddr (SockAddrUnix "/tmp/\12477") `shouldBe` "/tmp/\12477"
    describe "showSockAddrBS" $ do
        it "agrees with showSockAddr" $
            showSockAddrBS (v4 (192, 0, 2, 1)) `shouldBe` "192.0.2.1"
        it "encodes Unix domain socket path in UTF-8" $
            showSockAddrBS (SockAddrUnix "/tmp/\12477") `shouldBe` "/tmp/\227\130\189"

{-# LANGUAGE OverloadedStrings #-}
import Network.Socks5
import Network.Socket hiding (recv, sClose)
import Network.Socket.ByteString
import Network.BSD
import Network
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as BC

import System.IO (hClose, hFlush)
import System.Environment (getArgs)

main = do
    args <- getArgs
    let serverName = "localhost"
    let serverPort = 1080
    let destinationName = case args of
            []    -> "www.google.com"
            (x:_) -> x
    -- socks server is expected to be running on localhost port 1080
    he     <- getHostByName serverName
    let socksServerAddr = SockAddrInet serverPort (head $ hostAddresses he)

    example1 socksServerAddr destinationName
    example2 socksServerAddr destinationName

    example3 serverName serverPort destinationName 80

  where
        -- connect to @destName on port 80 through the socks server
        -- www.google.com get resolve on the client here and then the sockaddr is
        -- passed to socksConnectAddr
        example1 socksServerAddr destName = do
            socket <- socket AF_INET Stream defaultProtocol
            socksConnectWithSocket socket (defaultSocksConfFromSockAddr socksServerAddr)
                        (SocksAddress (SocksAddrDomainName $ BC.pack destName) 80)

            sendAll socket "GET / HTTP/1.0\r\n\r\n"
            recv socket 4096 >>= putStrLn . show
            sClose socket
        -- connect to @destName on port 80 through the socks server
        -- the server is doing the resolution itself
        example2 socksServerAddr destName = do
            socket <- socket AF_INET Stream defaultProtocol
            socksConnectName socket socksServerAddr destName 80
            sendAll socket "GET / HTTP/1.0\r\n\r\n"
            recv socket 4096 >>= putStrLn . show
            sClose socket

        example3 sname sport dname dport = do    
            handle <- socksConnectTo sname (PortNumber sport) dname (PortNumber dport)
            BC.hPut handle "GET / HTTP/1.0\r\n\r\n"
            hFlush handle
            BC.hGet handle 1024 >>= putStrLn . show
            hClose handle

module Dahdit.Network
  ( Encoder (..)
  , runEncoder
  , HostPort (..)
  , udpClient
  , withUdpClient
  )
where

import Control.Exception (bracket, onException)
import Dahdit (Binary (..), Put, putTarget)
import Data.ByteString qualified as BS
import Network.Socket qualified as NS
import System.IO (Handle, IOMode (..), hFlush)

-- newtype Decoder = Decoder {unDecoder :: forall a. Get a -> IO a}

-- runDecoder :: Binary a => Decoder -> IO a
-- runDecoder dec = unDecoder dec get

newtype Encoder = Encoder {unEncoder :: Put -> IO ()}

runEncoder :: Binary a => Encoder -> a -> IO ()
runEncoder enc = unEncoder enc . put

-- -- Need to take something more than handle here. Buffer unused data etc
-- decodeHandle :: Binary a => Handle -> IO a
-- decodeHandle = error "TODO"

encodeHandle :: Handle -> Put -> IO ()
encodeHandle h p = do
  let bs = putTarget p
  BS.hPut h bs
  hFlush h

data HostPort = HostPort
  { hpHost :: !(Maybe String)
  , hpPort :: !Int
  }
  deriving stock (Eq, Ord, Show)

newtype TcpOpts = TcpOpts {tcoFinTimeout :: Int}
  deriving newtype (Show)
  deriving stock (Eq, Ord)

newtype ServerOpts = ServerOpts {soMaxClients :: Int}
  deriving newtype (Show)
  deriving stock (Eq, Ord)

data SockTy = SockTyTcp | SockTyUdp
  deriving stock (Eq, Ord, Show, Enum, Bounded)

sockTyReal :: SockTy -> NS.SocketType
sockTyReal = \case
  SockTyTcp -> NS.Stream
  SockTyUdp -> NS.Datagram

data Target = Target
  { targetHp :: !HostPort
  , targetSockTy :: !SockTy
  }
  deriving stock (Eq, Ord, Show)

targetResolve :: Target -> IO NS.AddrInfo
targetResolve t@(Target (HostPort host port) sockTy) = do
  let hints = NS.defaultHints {NS.addrSocketType = sockTyReal sockTy}
  infos <- NS.getAddrInfo (Just hints) host (Just (show port))
  case infos of
    [] -> fail ("Could not resolve address: " ++ show t)
    info : _ -> pure info

targetConnect :: Target -> IO NS.Socket
targetConnect t = do
  info <- targetResolve t
  sock <- NS.openSocket info
  NS.connect sock (NS.addrAddress info)
  pure sock

socketEncoder :: NS.Socket -> IO Encoder
socketEncoder sock = do
  handle <- NS.socketToHandle sock WriteMode
  pure (Encoder (encodeHandle handle))

-- socketDecoder :: NS.Socket -> IO Decoder
-- socketDecoder sock = do
--   handle <- NS.socketToHandle sock ReadMode
--   pure (Decoder (decodeHandle handle))

-- socketBidi :: NS.Socket -> IO (Encoder, Decoder)
-- socketBidi sock = do
--   handle <- NS.socketToHandle sock ReadWriteMode
--   pure (Encoder (encodeHandle handle), Decoder (decodeHandle handle))

-- -- Should be called with async exceptions masked
-- tcpClient :: HostPort -> TcpOpts -> IO (Encoder, Decoder, IO ())
-- tcpClient hp (TcpOpts finTo) = do
--   sock <- targetConnect (Target hp SockTyTcp)
--   let dis = if finTo > 0 then NS.gracefulClose sock finTo else NS.close sock
--   (enc, dec) <- onException (socketBidi sock) dis
--   pure (enc, dec, dis)

-- withTcpClient :: HostPort -> TcpOpts -> (Encoder -> Decoder -> IO a) -> IO a
-- withTcpClient hp to f = bracket acq rel use where
--   acq = tcpClient hp to
--   rel (_, _, dis) = dis
--   use (enc, dec, _) = f enc dec

-- Should be called with async exceptions masked
udpClient :: HostPort -> IO (Encoder, IO ())
udpClient hp = do
  sock <- targetConnect (Target hp SockTyUdp)
  let dis = NS.close sock
  enc <- onException (socketEncoder sock) dis
  pure (enc, dis)

withUdpClient :: HostPort -> (Encoder -> IO a) -> IO a
withUdpClient hp f = bracket acq rel use
 where
  acq = udpClient hp
  rel (_, dis) = dis
  use (enc, _) = f enc

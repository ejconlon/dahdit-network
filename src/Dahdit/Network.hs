module Dahdit.Network
  ( Decoder (..)
  , runDecoder
  , handleDecoder
  , Encoder (..)
  , runEncoder
  , handleEncoder
  , HostPort (..)
  , tcpClient
  , withTcpClient
  , udpClient
  , withUdpClient
  , udpServer
  , withUdpServer
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Dahdit (Binary (..), Get, Put, putTarget)
import Data.Acquire (Acquire, mkAcquire, withAcquire)
import Data.ByteString qualified as BS
import Network.Socket qualified as NS
import System.IO (Handle, IOMode (..), hFlush)

newtype Decoder = Decoder {unDecoder :: forall a. Get a -> IO a}

runDecoder :: Binary a => Decoder -> IO a
runDecoder dec = unDecoder dec get

handleDecoder :: Handle -> IO Decoder
handleDecoder = error "TODO"

newtype Encoder = Encoder {unEncoder :: Put -> IO ()}

runEncoder :: Binary a => Encoder -> a -> IO ()
runEncoder enc = unEncoder enc . put

handleEncoder :: Handle -> Encoder
handleEncoder h = Encoder $ \p -> do
  bs <- putTarget p
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

targetBind :: Target -> IO NS.Socket
targetBind t = do
  info <- targetResolve t
  sock <- NS.openSocket info
  NS.bind sock (NS.addrAddress info)
  pure sock

socketEncoder :: NS.Socket -> IO Encoder
socketEncoder sock = do
  handle <- NS.socketToHandle sock WriteMode
  pure (handleEncoder handle)

socketDecoder :: NS.Socket -> IO Decoder
socketDecoder sock = do
  handle <- NS.socketToHandle sock ReadMode
  handleDecoder handle

socketBidi :: NS.Socket -> IO (Encoder, Decoder)
socketBidi sock = do
  handle <- NS.socketToHandle sock ReadWriteMode
  decoder <- handleDecoder handle
  pure (handleEncoder handle, decoder)

tcpClient :: HostPort -> TcpOpts -> Acquire (Encoder, Decoder)
tcpClient hp (TcpOpts finTo) = fmap (\(enc, dec, _) -> (enc, dec)) (mkAcquire acq rel)
 where
  acq = do
    sock <- targetConnect (Target hp SockTyTcp)
    (enc, dec) <- socketBidi sock
    pure (enc, dec, sock)
  rel (_, _, sock) =
    if finTo > 0 then NS.gracefulClose sock finTo else NS.close sock

withTcpClient :: MonadUnliftIO m => HostPort -> TcpOpts -> (Encoder -> Decoder -> m a) -> m a
withTcpClient hp to f = withAcquire (tcpClient hp to) (uncurry f)

udpClient :: HostPort -> Acquire Encoder
udpClient hp = fmap fst (mkAcquire acq rel)
 where
  acq = do
    sock <- targetConnect (Target hp SockTyUdp)
    enc <- socketEncoder sock
    pure (enc, sock)
  rel = NS.close . snd

withUdpClient :: HostPort -> (Encoder -> IO a) -> IO a
withUdpClient = withAcquire . udpClient

udpServer :: HostPort -> Acquire Decoder
udpServer hp = fmap fst (mkAcquire acq rel)
 where
  acq = do
    sock <- targetBind (Target hp SockTyUdp)
    dec <- socketDecoder sock
    pure (dec, sock)
  rel = NS.close . snd

withUdpServer :: HostPort -> (Decoder -> IO a) -> IO a
withUdpServer = withAcquire . udpServer

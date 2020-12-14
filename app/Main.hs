{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment ( getArgs )
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import System.Process ( readProcess )
import GHC.Generics ( Generic )
import Data.Aeson
    ( decodeFileStrict, encodeFile, FromJSON, ToJSON )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import qualified Data.Function as F (on)
import Data.List ( groupBy, sort )
import Data.Ord ( comparing )
import Data.Time.LocalTime
    ( LocalTime(localDay),
      ZonedTime(zonedTimeToLocalTime),
      getZonedTime )
import Data.ByteString.Internal (packChars)
import qualified Data.ByteString as B
import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
import Graphics.UI.Gtk
    ( set,
      containerBorderWidth,
      containerChild,
      widgetDestroy,
      widgetShowAll,
      entryGetText,
      entryNew,
      onEntryActivate,
      screenGetHeight,
      screenGetWidth,
      initGUI,
      mainGUI,
      postGUIAsync,
      windowDecorated,
      windowDefaultHeight,
      windowDefaultWidth,
      windowGetScreen,
      windowMove,
      windowNew,
      windowSetKeepAbove,
      windowWindowPosition,
      AttrOp((:=)),
      WindowPosition(WinPosCenter) )
import Control.Exception ( bracket, catch )
import Control.Monad ( forever )
import Control.Concurrent (forkIO)
import System.Socket
    ( accept,
      bind,
      close,
      listen,
      receive,
      send,
      socket,
      msgNoSignal,
      SocketException,
      Socket,
      ReuseAddress(ReuseAddress),
      SocketOption(setSocketOption) )
import System.Socket.Family.Inet6
    ( SocketAddress(SocketAddressInet6),
      inet6Any,
      Inet6,
      V6Only(V6Only) )
import System.Socket.Type.Stream ( Stream )
import System.Socket.Protocol.TCP ( TCP )

obtenerEvento :: CargarEvento -> IO ()
obtenerEvento cargarEvento = do
  window <- windowNew
  pantalla <- windowGetScreen window
  (width, height) <- (,) <$> screenGetWidth pantalla <*> screenGetHeight pantalla
  motivoHorasEntrada <- entryNew
  set window [windowDefaultWidth := 500,
              windowDefaultHeight := 100,
              windowWindowPosition := WinPosCenter,
              windowDecorated := False,
              containerChild := motivoHorasEntrada, containerBorderWidth := 10]
  windowSetKeepAbove window True
  windowMove window width height
  widgetShowAll window 
  onEntryActivate motivoHorasEntrada $ do
    evento <- Evento <$> entryGetText motivoHorasEntrada
    cargarEvento [] evento
    widgetDestroy window
  pure ()

type Registros = [Registro]
type Tiempo = LocalTime
newtype Evento = Evento String deriving (Eq, Generic)

instance FromJSON Evento
instance ToJSON Evento

data MetaData = MetaData { tipo :: String, info :: String } deriving (Eq, Generic)

instance Show MetaData where
  show (MetaData tipo info) = "(" <> tipo <> ": " <> info <> ")"

instance FromJSON MetaData
instance ToJSON MetaData

data Registro = Registro {
  evento :: Evento,
  tiempo :: Tiempo,
  metadata :: [MetaData]
} deriving (Eq, Show, Generic)

instance FromJSON Registro
instance ToJSON Registro

instance Show Evento where
    show (Evento evento) = evento

instance Ord Registro where
    compare = comparing tiempo

cargar :: Registro -> Registros -> Registros
cargar registro registros = registro : registros

type CargarEvento = [MetaData] -> Evento -> IO ()

readShell :: String -> String -> IO String
readShell cmd = readProcess "bash" ["-c", cmd]

obtenerBranchDe :: FilePath -> IO String
obtenerBranchDe path =
  init <$> readShell ("cd " <> path <> " && git branch --show-current") ""

obtenerBranchDeRailsTeespring :: IO MetaData
obtenerBranchDeRailsTeespring =
  MetaData "rails-teespring current branch" <$> obtenerBranchDe "~/development/proyectos/teespring/rails-teespring"

obtenerBranchDeRailsCampaignQA :: IO MetaData
obtenerBranchDeRailsCampaignQA =
  MetaData "campaign-qa current branch" <$> obtenerBranchDe "~/development/proyectos/teespring/campaign-qa"

obtenerMetadata :: IO [MetaData]
obtenerMetadata = sequence [
    obtenerBranchDeRailsTeespring,
    obtenerBranchDeRailsCampaignQA
  ]

cargarEvento :: IORef Registros -> CargarEvento
cargarEvento registros metadataDelEvento nuevoEvento = do
  tiempoActual <- zonedTimeToLocalTime <$> getZonedTime
  metadataDefault <- obtenerMetadata
  modifyIORef registros (cargar $ Registro nuevoEvento tiempoActual (metadataDelEvento <> metadataDefault))

mostrarDia :: Registro -> String
mostrarDia registro = dia <> "\n" <> separador
  where dia = show . localDay . tiempo $ registro
        separador = "-------"

mostrarRegistro :: Registro -> String
mostrarRegistro registro = hora <> " - " <> eventoDelRegistro <> " - " <> metadataDelRegistro
  where hora = formatTime defaultTimeLocale "%T" (tiempo registro)
        eventoDelRegistro = show (evento registro)
        metadataDelRegistro = show (metadata registro)

mostrarRegistrosDelDia :: [Registro] -> String
mostrarRegistrosDelDia registrosDelDia =
  unlines $ mostrarDia (head registrosDelDia) : map mostrarRegistro registrosDelDia

mostrarRegistros :: IORef Registros -> IO String
mostrarRegistros registrosIO = do
  registros <- readIORef registrosIO
  let registrosPorDia = groupBy ((==) `F.on` (localDay . tiempo)) $ sort registros
  pure . unlines . map mostrarRegistrosDelDia $ registrosPorDia

leerRegistros :: FilePath -> IO Registros
leerRegistros rutaARegistros = do
  resultado <- decodeFileStrict rutaARegistros
  case resultado of
    Just registros -> pure registros
    Nothing -> error "Error al leer el JSON de registros"

guardarRegistros :: FilePath -> IORef Registros -> IO ()
guardarRegistros rutaARegistros registrosIO = readIORef registrosIO >>= encodeFile rutaARegistros

main :: IO ()
main = do
  args <- getArgs
  let (rutaARegistros, puertoDelSocket) = case args of
        (ruta:puerto:_) -> (ruta, read puerto)
        (ruta:_) -> (ruta, 8080)
  registrosGuardados <- leerRegistros rutaARegistros
  registros <- newIORef registrosGuardados
  forkIO $ do
    openSocket puertoDelSocket (mostrarRegistros registros) $ \metadata evento -> do
      cargarEvento registros metadata evento
      guardarRegistros rutaARegistros registros
  putStrLn "empezamo"
  initGUI
  mainGUI

openSocket :: Int -> IO String -> CargarEvento -> IO ()
openSocket puerto mostrarRegistros cargarEvento = bracket
  ( socket :: IO (System.Socket.Socket Inet6 Stream TCP) )
  ( \s-> do
    close s
    putStrLn "Listening socket closed."
  )
  ( \s-> do
    setSocketOption s (ReuseAddress True)
    setSocketOption s (V6Only False)
    bind s (SocketAddressInet6 inet6Any (fromIntegral puerto) 0 0)
    listen s 5
    putStrLn $ "Escuchando en " <> show puerto
    forever $ acceptAndHandle mostrarRegistros cargarEvento s `catch` \e-> print (e :: SocketException)
  )

acceptAndHandle :: IO String -> CargarEvento -> System.Socket.Socket Inet6 Stream TCP -> IO ()
acceptAndHandle mostrarRegistros cargarEvento socket = bracket
  (accept socket)
  (\(socket, _address) -> close socket)
  (\(socket, _address) -> do
    message <- receive socket 9999 msgNoSignal --kcyo debe haber otro receive mas copado
    case P.parse parserMensaje "" message of
      Right mensaje -> case mensaje of
        CargaDeEvento -> postGUIAsync $ obtenerEvento cargarEvento
        Mostrar -> do
          registros <- packChars <$> mostrarRegistros
          send socket registros msgNoSignal
          pure ()
        Commit repo branch mensaje ->
          cargarEvento [MetaData "repoDelCommit" repo,
                        MetaData "branchDelCommit" branch,
                        MetaData "mensajeDelCommit" mensaje] (Evento "Commit")
        RegistrarEvento evento metadatas -> cargarEvento metadatas evento
      Left error -> print error
  )

data Mensaje = CargaDeEvento
              | Mostrar
              | Commit String String String
              | RegistrarEvento Evento [MetaData] deriving (Show, Eq)

type Parsersito a = P.ParsecT B.ByteString () Identity a

parserMensaje :: Parsersito Mensaje
parserMensaje = P.try parserCargar <|> P.try parserMostrar <|> P.try parserCommit <|> parserRegistrarEvento

parserCargar :: Parsersito Mensaje
parserCargar = P.string "cargar" $> CargaDeEvento

parserMostrar :: Parsersito Mensaje
parserMostrar = P.string "mostrar" $> Mostrar

parserCommit :: Parsersito Mensaje
parserCommit = do
  let separador = P.string "|"
  _ <- P.string "commit" <> separador
  repo <- P.manyTill P.anyChar (P.try separador)
  branch <- P.manyTill P.anyChar (P.try separador)
  mensaje <- P.manyTill P.anyChar P.eof
  pure $ Commit repo branch mensaje

parserRegistrarEvento :: Parsersito Mensaje
parserRegistrarEvento = do
  _ <- P.string "registrar"
  _ <- separador
  nombreEvento <- P.manyTill P.anyChar (P.try separador <|> (P.newline >> pure ()) <> P.eof)
  metadatas <- P.sepBy metadataParser separador
  pure $ RegistrarEvento (Evento nombreEvento) metadatas
    where separador = P.string "|" >> pure ()
          metadataParser = do
            tipo <- P.many1 P.alphaNum
            _ <- P.string ":"
            valor <- P.manyTill P.anyChar (P.lookAhead . P.try $ separador <|> P.eof <|> (P.newline >> pure ()))
            pure $ MetaData tipo valor

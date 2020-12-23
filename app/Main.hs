{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Lib
    ( convertirEnRegistro,
      guardarRegistros,
      mostrarTodosLosRegistros,
      rutaARegistros,
      Registro,
      cargarNuevoRegistro,
      RegistroRecibido )
import qualified Network.WebSockets as WS
import VentanaDeCarga ( mostrarVentanaDeCarga )
import System.Environment (getArgs)
import Data.IORef (newIORef)
import Data.ByteString (concat)
import Control.Concurrent (forkIO)
import Graphics.UI.Gtk (mainGUI, initGUI, postGUIAsync)
import Data.ByteString.Internal (ByteString, packChars)
import Control.Exception ( bracket, catch )
import Data.ByteString.Search (replace, split)
import Control.Monad ( forever )
import GHC.Generics (Generic)
import Data.Aeson
    ( decodeStrict, (.:), FromJSON(parseJSON), Value(Object) )
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (toChunks)
import Tiempo (momentoActual)

main :: IO ()
main = do
  args <- getArgs
  let (directorioDeRegistros, puertoDelSocket) = case args of
        (ruta:puerto:_) -> (ruta, read puerto)
        (ruta:_) -> (ruta, 8080)
  rutaARegistrosDeHoy <- rutaARegistros directorioDeRegistros
  registros <- newIORef []
  forkIO $ do
    openSocket puertoDelSocket (mostrarTodosLosRegistros directorioDeRegistros) $ \registro -> do
      cargarNuevoRegistro registros registro
      guardarRegistros rutaARegistrosDeHoy registros
  putStrLn "empezamo"
  initGUI
  mainGUI

openSocket :: Int -> IO String -> (Registro -> IO ()) -> IO ()
openSocket puerto mostrarRegistros cargarRegistro = WS.runServer "127.0.0.1" puerto app
  where app pending = do
          conexion <- WS.acceptRequest pending
          WS.withPingThread conexion 30 (return ()) $ do
            message <- WS.receiveData conexion
            case decodeStrict message of
              Just mensaje -> case mensaje of
                AbrirMenuDeCarga -> postGUIAsync $
                  mostrarVentanaDeCarga ["teespring", "gestion interna", "descanso"] (Just "teespring") cargarRegistro
                MostrarRegistros -> do
                  registros <- split "\n" . packChars <$> mostrarRegistros
                  mapM_ (WS.sendTextData conexion) registros
                CargarRegistro registroRecibido -> convertirEnRegistro registroRecibido >>= cargarRegistro
              Nothing -> WS.sendTextData conexion ("Ocurrió un problema al recibir el mensaje" :: ByteString)

data Mensaje = AbrirMenuDeCarga
              | MostrarRegistros
              | CargarRegistro RegistroRecibido deriving (Show, Eq, Generic)

instance FromJSON Mensaje where
  parseJSON (Object o) = do
    tipoMensaje <- (o .: "accion" :: Parser String)
    case tipoMensaje of
      "cargar" -> pure AbrirMenuDeCarga
      "mostrar" -> pure MostrarRegistros
      "registrar" -> CargarRegistro <$> o .: "registro"


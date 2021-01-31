{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Lib
    (mostrarRegistros,  convertirEnRegistro,
      guardarRegistros,
      rutaARegistros,
      Registro,
      cargarNuevoRegistro,
      leerRegistros,
      RegistroRecibido )
import System.Directory (listDirectory)
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
    ( decodeStrict, (.:), encode, FromJSON(parseJSON), Value(Object) )
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (toChunks)
import Tiempo (momentoActual)
import BloqueDeHoras (mostrarHorasPorDia, horasPorDia)
import qualified Data.ByteString.UTF8 as B.UTF8 (fromString)
import Main.Utf8 (withUtf8)

mostrarTodosLosRegistros :: FilePath -> IO String
mostrarTodosLosRegistros directorio = mostrarRegistros <$> todosLosRegistros directorio

mostrarRegistrosConResumenPorDia :: FilePath -> IO String
mostrarRegistrosConResumenPorDia directorio = mostrarHorasPorDia <$> todosLosRegistros directorio

horasPorDiaJSON :: FilePath -> IO ByteString
horasPorDiaJSON directorio = do
  registros <- todosLosRegistros directorio
  pure . WS.fromLazyByteString . encode $ horasPorDia registros

todosLosRegistros :: FilePath -> IO [Registro]
todosLosRegistros directorio = do
  archivosDeRegistros <- fmap ((directorio <> "/") <>) <$> listDirectory directorio
  foldMap leerRegistros archivosDeRegistros

main :: IO ()
main = withUtf8 $ do
  args <- getArgs
  let (directorioDeRegistros, puertoDelSocket) = case args of
        (ruta:puerto:_) -> (ruta, read puerto)
        (ruta:_) -> (ruta, 8080)
  rutaARegistrosDeHoy <- rutaARegistros directorioDeRegistros
  registros <- newIORef []
  forkIO $ do
    openSocket puertoDelSocket (mostrarTodosLosRegistros directorioDeRegistros)
                               (mostrarRegistrosConResumenPorDia directorioDeRegistros)
                               (horasPorDiaJSON directorioDeRegistros) $ \registro -> do
      cargarNuevoRegistro registros registro
      guardarRegistros rutaARegistrosDeHoy registros
  putStrLn "empezamo"
  initGUI
  mainGUI

openSocket :: Int -> IO String -> IO String -> IO ByteString -> (Registro -> IO ()) -> IO ()
openSocket puerto mostrarRegistros mostrarRegistrosConResumen horasComoJSON cargarRegistro = WS.runServer "127.0.0.1" puerto app
  where app pending = do
          conexion <- WS.acceptRequest pending
          WS.withPingThread conexion 30 (return ()) $ do
            message <- WS.receiveData conexion
            case decodeStrict message of
              Just mensaje -> case mensaje of
                AbrirMenuDeCarga -> postGUIAsync $
                  mostrarVentanaDeCarga ["teespring", "gestion interna", "descanso"] (Just "teespring") cargarRegistro
                MostrarRegistros -> mostrarRegistros >>= enviarTexto conexion
                MostrarConResumenPorDia -> mostrarRegistrosConResumen >>= enviarTexto conexion
                ResumenesPorDia -> horasComoJSON >>= WS.sendTextData conexion
                CargarRegistro registroRecibido -> do
                  convertirEnRegistro registroRecibido >>= cargarRegistro
                  WS.sendClose conexion ("Registro cargado!" :: ByteString)
              Nothing -> enviarTexto conexion "Ocurrió un problema al recibir el mensaje"
        enviarTexto :: WS.Connection -> String -> IO ()
        enviarTexto conexion = mapM_ (WS.sendTextData conexion) . split "\n" . B.UTF8.fromString

data Mensaje = AbrirMenuDeCarga
              | MostrarRegistros
              | MostrarConResumenPorDia
              | ResumenesPorDia
              | CargarRegistro RegistroRecibido deriving (Show, Eq, Generic)

instance FromJSON Mensaje where
  parseJSON (Object o) = do
    tipoMensaje <- (o .: "accion" :: Parser String)
    case tipoMensaje of
      "cargar" -> pure AbrirMenuDeCarga
      "mostrar" -> pure MostrarRegistros
      "mostrar-con-resumen-por-dia" -> pure MostrarConResumenPorDia
      "registrar" -> CargarRegistro <$> o .: "registro"
      "resumenes-por-dia" -> pure ResumenesPorDia
      _ -> mempty


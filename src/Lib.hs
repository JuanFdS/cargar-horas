{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.Map.Strict as Map
import System.Environment ( getArgs )
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import System.Process ( readProcess )
import GHC.Generics ( Generic )
import Data.Aeson
    ( decodeFileStrict, encodeFile, FromJSON, ToJSON, parseJSON, Value(..), (.:), (.:?), (.!=) )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import qualified Data.Function as F (on)
import qualified Data.List as List ( groupBy, sort, group )
import Data.Ord ( comparing )
import qualified Data.ByteString as B
import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
import System.Directory (listDirectory)
import Tiempo
    ( Momento,
      Duracion,
      formateado,
      momentoActual,
      Dia,
      dia,
      Formato(DiaYHora, HoraYMinutos, AnioMesDia),
      tiempoEntre )
import Data.Maybe (fromMaybe)

type Registros = [Registro]
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
  tiempo :: Momento,
  tag :: String,
  metadata :: [MetaData]
} deriving (Eq, Show, Generic)

data RegistroRecibido = RegistroRecibido {
  eventoRecibido :: Evento,
  tiempoRecibido :: Maybe Momento,
  tagRecibido :: Maybe String,
  metadataRecibida :: [MetaData]
} deriving (Eq, Show, Generic)

instance FromJSON RegistroRecibido where
  parseJSON (Object o) = RegistroRecibido <$> o .: "evento"
                                          <*> o .:? "tiempo"
                                          <*> o .:? "tag"
                                          <*> o .: "metadata"

convertirEnRegistro :: RegistroRecibido -> IO Registro
convertirEnRegistro (RegistroRecibido evento talVezTiempo talVezTag metadata) = do
  let tag = fromMaybe "" talVezTag
  case talVezTiempo of
    Just tiempo -> pure $ Registro evento tiempo tag metadata
    Nothing -> Registro evento <$> momentoActual <*> pure tag <*> pure metadata


instance FromJSON Registro where
  parseJSON (Object o) = Registro <$> o .: "evento"
                                  <*> o .: "tiempo"
                                  <*> o .:? "tag" .!= ""
                                  <*> o .: "metadata"

instance ToJSON Registro

data BloqueDeHoras = BloqueDeHoras {
  horas :: Duracion,
  eventoBloque :: Evento,
  tagBloque :: String
} deriving (Eq, Generic)

instance Show BloqueDeHoras where
  show (BloqueDeHoras horas eventoBloque tagBloque) = "[" <> tagBloque <> "] " <> show eventoBloque <> ": " <> formateado HoraYMinutos horas

groupBy :: (Ord b) => (a -> b) -> [a] -> Map.Map b [a]
groupBy compareField = Map.fromListWith (flip (++)) . map (\x -> (compareField x, [x]))

porDia :: [Registro] -> Map.Map Dia [Registro]
porDia = groupBy (dia . tiempo) . List.sort

bloquesDeHorasPorDia :: [Registro] -> Map.Map Dia [BloqueDeHoras]
bloquesDeHorasPorDia =
  fmap (\registros -> agruparBloquesIguales $ zipWith crearBloque registros (tail registros)) . porDia
    where crearBloque registroInicio registroFin =
            BloqueDeHoras ((tiempoEntre `F.on` tiempo) registroFin registroInicio) (evento registroInicio) (tag registroInicio)

agruparBloquesIguales :: [BloqueDeHoras] -> [BloqueDeHoras]
agruparBloquesIguales = fmap (foldr1 agruparBloqueIgual) . List.groupBy ((==) `F.on` eventoBloque)
  where agruparBloqueIgual (BloqueDeHoras horas evento tag) (BloqueDeHoras otrasHoras _ _) = BloqueDeHoras (horas + otrasHoras) evento tag

agruparBloques :: BloqueDeHoras -> BloqueDeHoras -> BloqueDeHoras
agruparBloques (BloqueDeHoras horas evento tag) (BloqueDeHoras otrasHoras otroEvento _) =
              BloqueDeHoras (horas + otrasHoras) (Evento $ show evento <> ", " <> show otroEvento) tag

bloquesPorTag :: [BloqueDeHoras] -> Map.Map String BloqueDeHoras
bloquesPorTag = fmap (foldr1 agruparBloques) . groupBy tagBloque

joinString :: String -> [String] -> String
joinString separator = foldr (\acum text -> acum <> separator <> text) ""

mostrarBloquesDeUnDia :: [BloqueDeHoras] -> String
mostrarBloquesDeUnDia = joinString "\n" . fmap show

mostrarBloquesAgrupadosDeUnDia :: [BloqueDeHoras] -> String
mostrarBloquesAgrupadosDeUnDia = joinString "\n" . fmap show . Map.elems . bloquesPorTag

mostrarSinEvento :: BloqueDeHoras -> String
mostrarSinEvento bloque = "[" <> tagBloque bloque <> "]: " <> formatTime defaultTimeLocale "%H:%0M" (horas bloque)

mostrarResumenDeUnDia :: [BloqueDeHoras] -> String
mostrarResumenDeUnDia bloques =
  mostrarBloquesDeUnDia bloques <> "\n-Resumen-\n" <> (joinString "\n" . fmap mostrarSinEvento . Map.elems . bloquesPorTag $ bloques)

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
obtenerMetadata = sequence []

cargarEvento :: IORef Registros -> CargarEvento
cargarEvento registros metadataDelEvento nuevoEvento = do
  tiempoActual <- momentoActual
  metadataDefault <- obtenerMetadata
  modifyIORef registros (cargar $ Registro nuevoEvento tiempoActual "" (metadataDelEvento <> metadataDefault))

mostrarDia :: Registro -> String
mostrarDia registro = dia <> "\n" <> separador
  where dia = formateado AnioMesDia . tiempo $ registro
        separador = "-------"

mostrarRegistro :: Registro -> String
mostrarRegistro registro = hora <> " - " <> eventoDelRegistro <> " - " <> metadataDelRegistro
  where hora = formatTime defaultTimeLocale "%T" (tiempo registro)
        eventoDelRegistro = show (evento registro)
        metadataDelRegistro = show (metadata registro)

mostrarRegistrosDelDia :: [Registro] -> String
mostrarRegistrosDelDia registrosDelDia =
  unlines $ mostrarDia (head registrosDelDia) : map mostrarRegistro registrosDelDia

mostrarRegistros :: Registros -> String
mostrarRegistros = foldMap mostrarRegistrosDelDia . Map.elems . porDia

mostrarTodosLosRegistros :: FilePath -> IO String
mostrarTodosLosRegistros directorio = do
  archivosDeRegistros <- fmap ((directorio <> "/") <>) <$> listDirectory directorio
  todosLosRegistros <- foldMap leerRegistros archivosDeRegistros
  pure $ mostrarRegistros todosLosRegistros

leerRegistros :: FilePath -> IO Registros
leerRegistros rutaARegistros = do
  resultado <- decodeFileStrict rutaARegistros
  case resultado of
    Just registros -> pure registros
    Nothing -> error "Error al leer el JSON de registros"

guardarRegistros :: FilePath -> IORef Registros -> IO ()
guardarRegistros rutaARegistros registrosIO = readIORef registrosIO >>= encodeFile rutaARegistros

rutaARegistros :: FilePath -> IO FilePath
rutaARegistros directorio = do
  ahora <- momentoActual
  pure $ directorio <> "/" <> "registros-" <> formateado DiaYHora ahora <> ".json"

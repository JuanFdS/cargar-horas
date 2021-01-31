{-# LANGUAGE DerivingVia #-}
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
import qualified Data.List as List (sortOn,  groupBy, sort, group )
import Data.Ord ( comparing )
import qualified Data.ByteString as B
import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
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
import Data.Char (toLower)

type Registros = [Registro]
newtype Evento = Evento String deriving (Eq, Generic)

instance FromJSON Evento
instance ToJSON Evento

data MetaData = MetaData { tipo :: String, info :: String } deriving (Eq, Generic)

instance Show MetaData where
  show (MetaData tipo info) = "(" <> tipo <> ": " <> info <> ")"

instance FromJSON MetaData
instance ToJSON MetaData

newtype Tag = Tag String deriving (FromJSON, ToJSON) via String

instance Ord Tag where
  compare (Tag unTag) (Tag otroTag) = compare (map toLower unTag) (map toLower otroTag)
instance Eq Tag where
  (Tag unTag) == (Tag otroTag) = map toLower unTag == map toLower otroTag
instance Show Tag where
  show (Tag tag) = map toLower tag

data Registro = Registro {
  evento :: Evento,
  tiempo :: Momento,
  tag :: Tag,
  metadata :: [MetaData]
} deriving (Eq, Show, Generic)

juntarEventos :: Evento -> Evento -> Evento
juntarEventos (Evento e1) (Evento e2) = Evento (e1 <> ", " <> e2)

data RegistroRecibido = RegistroRecibido {
  eventoRecibido :: Evento,
  tiempoRecibido :: Maybe Momento,
  tagRecibido :: Maybe Tag,
  metadataRecibida :: [MetaData]
} deriving (Eq, Show, Generic)

instance FromJSON RegistroRecibido where
  parseJSON (Object o) = RegistroRecibido <$> o .: "evento"
                                          <*> o .:? "tiempo"
                                          <*> o .:? "tag"
                                          <*> o .: "metadata"

convertirEnRegistro :: RegistroRecibido -> IO Registro
convertirEnRegistro (RegistroRecibido evento talVezTiempo talVezTag metadata) = do
  let tag = fromMaybe (Tag "") talVezTag
  case talVezTiempo of
    Just tiempo -> pure $ Registro evento tiempo tag metadata
    Nothing -> Registro evento <$> momentoActual <*> pure tag <*> pure metadata


instance FromJSON Registro where
  parseJSON (Object o) = Registro <$> o .: "evento"
                                  <*> o .: "tiempo"
                                  <*> o .:? "tag" .!= Tag ""
                                  <*> o .: "metadata"

instance ToJSON Registro

instance Show Evento where
    show (Evento evento) = evento

cargar :: Registro -> Registros -> Registros
cargar registro registros = registro : registros

type CargarEvento = String -> Momento -> [MetaData] -> Evento -> IO ()

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

conMetadataAgregada :: [MetaData] -> Registro -> Registro
conMetadataAgregada metadataAgregada registro = registro { metadata = metadataAgregada ++ metadata registro }

cargarNuevoRegistro :: IORef Registros -> Registro -> IO ()
cargarNuevoRegistro registros nuevoRegistro = do
  metadataDefault <- obtenerMetadata
  modifyIORef registros (cargar $ conMetadataAgregada metadataDefault nuevoRegistro)

mostrarDia :: Registro -> String
mostrarDia registro = dia <> "\n" <> separador
  where dia = formateado AnioMesDia . tiempo $ registro
        separador = "-------"

mostrarRegistro :: Registro -> String
mostrarRegistro registro = hora <> " - " <> "[" <> show (tag registro) <> "]" <> eventoDelRegistro <> " - " <> metadataDelRegistro
  where hora = formatTime defaultTimeLocale "%T" (tiempo registro)
        eventoDelRegistro = show (evento registro)
        metadataDelRegistro = show (metadata registro)

mostrarRegistrosDelDia :: [Registro] -> String
mostrarRegistrosDelDia registrosDelDia =
  unlines $ mostrarDia (head registrosDelDia) : map mostrarRegistro registrosDelDia

groupBy :: (Ord b) => (a -> b) -> [a] -> Map.Map b [a]
groupBy compareField = Map.fromListWith (flip (++)) . map (\x -> (compareField x, [x]))

registrosPorDia :: [Registro] -> Map.Map Dia [Registro]
registrosPorDia = groupBy (dia . tiempo) . List.sortOn tiempo

mostrarRegistros :: Registros -> String
mostrarRegistros = foldMap mostrarRegistrosDelDia . Map.elems . registrosPorDia

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

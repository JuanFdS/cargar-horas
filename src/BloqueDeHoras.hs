{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module BloqueDeHoras (
    horasPorDia,
    mostrarHorasPorDia
) where

import Lib (registrosPorDia, juntarEventos, groupBy, Tag, Evento, MetaData, Registro(..), Evento(..))
import Tiempo (Formato(HoraYMinutos), formateado, tiempoEntre, Dia, Duracion, Momento)
import GHC.Generics (Generic)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Aeson (ToJSON)
import Data.Foldable (Foldable(fold))

data BloqueDeHoras = BloqueDeHoras {
  rangoHoras :: (Momento, Momento),
  eventoBloque :: Evento,
  tagBloque :: Tag,
  metadataBloque :: [MetaData]
} deriving (Eq, Generic, ToJSON)

duracionBloque :: BloqueDeHoras -> Duracion
duracionBloque bloque = tiempoEntre fin inicio
  where (inicio, fin) = rangoHoras bloque

representanElMismoEvento :: BloqueDeHoras -> BloqueDeHoras -> Bool
representanElMismoEvento unBloque otroBloque = eventoBloque unBloque == eventoBloque otroBloque &&
                                              tagBloque unBloque == tagBloque otroBloque &&
                                              metadataBloque unBloque == metadataBloque otroBloque

unirBloquesSeguidosQueRepresentanElMismoEvento :: [BloqueDeHoras] -> [BloqueDeHoras]
unirBloquesSeguidosQueRepresentanElMismoEvento = fmap (foldr1 unirBloquesQueRepresentanElMismoEvento) . List.groupBy representanElMismoEvento

unirBloquesQueRepresentanElMismoEvento :: BloqueDeHoras -> BloqueDeHoras -> BloqueDeHoras
unirBloquesQueRepresentanElMismoEvento (BloqueDeHoras (minimo, _) evento tag metadata)
                                       (BloqueDeHoras (_, maximo) _ _ _) = BloqueDeHoras (minimo, maximo) evento tag metadata

bloquesDeHorasPorDia :: [Registro] -> Map.Map Dia [BloqueDeHoras]
bloquesDeHorasPorDia =
  fmap (\registros -> unirBloquesSeguidosQueRepresentanElMismoEvento $ zipWith registrosSucesivosABloque registros (tail registros)) . registrosPorDia
  where
        registrosSucesivosABloque :: Registro -> Registro -> BloqueDeHoras
        registrosSucesivosABloque registroInicio registroFin = 
          BloqueDeHoras (tiempo registroInicio, tiempo registroFin) (evento registroInicio) (tag registroInicio) (metadata registroInicio)

resumenes :: [BloqueDeHoras] -> [ResumenDeHoras]
resumenes = Map.elems . Map.mapWithKey hacerResumenDeHoras . groupBy tagBloque
  where
        hacerResumenDeHoras :: Tag -> [BloqueDeHoras] -> ResumenDeHoras
        hacerResumenDeHoras tag bloquesDeHoras = ResumenDeHoras (sum $ map duracionBloque bloquesDeHoras)
                                                                (foldr1 juntarEventos $ map eventoBloque bloquesDeHoras)
                                                                tag

horasPorDia :: [Registro] -> Map.Map Dia HorasDeUnDia
horasPorDia = Map.filter (not . null . bloquesDeHoras) .
              Map.map (\registros -> HorasDeUnDia (fold $ bloquesDeHorasPorDia registros)
                                                  (resumenes . fold $ bloquesDeHorasPorDia registros)) . registrosPorDia

data ResumenDeHoras = ResumenDeHoras {
  horasResumen :: Duracion,
  eventoResumen :: Evento,
  tagResumen :: Tag
} deriving (Eq, Generic, ToJSON)

data HorasDeUnDia = HorasDeUnDia {
    bloquesDeHoras :: [BloqueDeHoras],
    resumenesDeHoras :: [ResumenDeHoras]
} deriving (Eq, Generic, ToJSON)

-- Mostrar

instance Show BloqueDeHoras where
  show bloque@(BloqueDeHoras (minimo, maximo) eventoBloque tagBloque metadata) =
      formateado HoraYMinutos minimo <> "-" <> formateado HoraYMinutos maximo <> " -- " <>
    "[" <> show tagBloque <> "]: " <> show eventoBloque <> " " <>
    "(" <> formateado HoraYMinutos (duracionBloque bloque) <> ")"

mostrarBloquesDeUnDia :: [BloqueDeHoras] -> String
mostrarBloquesDeUnDia = List.intercalate "\n" . fmap show

instance Show ResumenDeHoras where
  show (ResumenDeHoras horas evento tag) = "[" <> show tag <> "]" <> ": " <> formateado HoraYMinutos horas <> "\n" <>
                                           "Eventos: " <> show evento

mostrarHorasDeUnDia :: Dia -> HorasDeUnDia -> String
mostrarHorasDeUnDia dia (HorasDeUnDia bloques resumenes) = List.intercalate "\n" [show dia,
                                                                                  mostrarBloquesDeUnDia bloques,
                                                                                  "-Resumen-",
                                                                                  foldMap (\resumen -> show resumen <> "\n") resumenes]

mostrarHorasPorDia :: [Registro] -> String
mostrarHorasPorDia = List.intercalate "------\n" . Map.elems . Map.mapWithKey mostrarHorasDeUnDia . horasPorDia

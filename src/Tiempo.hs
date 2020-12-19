module Tiempo where

import Data.Time (diffLocalTime, FormatTime, Day, getZonedTime, zonedTimeToLocalTime, localDay,   defaultTimeLocale, formatTime, NominalDiffTime, LocalTime)

type Momento = LocalTime
type Duracion = NominalDiffTime
type Dia = Day

data Formato = HoraYMinutos | AnioMesDia | DiaYHora

dia :: Momento -> Dia
dia = localDay

tiempoEntre :: Momento -> Momento -> Duracion
tiempoEntre = diffLocalTime

formateado :: (FormatTime t) => Formato -> t -> String
formateado formato = formatTime defaultTimeLocale formato'
    where formato' = case formato of
                        HoraYMinutos -> "%0H:%0M"
                        AnioMesDia -> "%F"
                        DiaYHora -> "%F-%T"

momentoActual :: IO Momento
momentoActual = zonedTimeToLocalTime <$> getZonedTime
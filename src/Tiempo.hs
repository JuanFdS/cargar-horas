module Tiempo where

import Data.Time
    (makeTimeOfDayValid,  Day,
      NominalDiffTime,
      LocalTime(localDay, localTimeOfDay),
      FormatTime,
      formatTime,
      defaultTimeLocale,
      diffLocalTime,
      getZonedTime,
      TimeOfDay(todHour, todMin),
      ZonedTime(zonedTimeToLocalTime) )
import Data.Time.LocalTime

type Momento = LocalTime
type Duracion = NominalDiffTime
type Dia = Day
data HoraDelDia = HoraDelDia { hora :: Int, minutos :: Int }

data Formato = HoraYMinutos | AnioMesDia | DiaYHora

mkValidHoraDelDia :: Int -> Int -> Maybe HoraDelDia
mkValidHoraDelDia horas minutos = case (horas, minutos) of
    (horas, minutos) | horas >= 0 && horas <= 23 && minutos >= 0 && minutos <= 59 -> Just $ HoraDelDia horas minutos
    _ -> Nothing

horaDelMomento :: Momento -> Int
horaDelMomento = hora . horaDelDia

minutosDelMomento :: Momento -> Int
minutosDelMomento = minutos . horaDelDia

mkMomento :: HoraDelDia -> Day -> Momento
mkMomento (HoraDelDia hora minutos) dia =
    LocalTime dia (TimeOfDay hora minutos 0)

dia :: Momento -> Dia
dia = localDay

horaDelDia :: Momento -> HoraDelDia
horaDelDia momento = HoraDelDia (todHour $ localTimeOfDay momento)
                                (todMin $ localTimeOfDay momento)

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

conHora :: Momento -> HoraDelDia -> Momento
conHora momento horaDelDia = mkMomento horaDelDia (dia momento)
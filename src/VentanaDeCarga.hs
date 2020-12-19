module VentanaDeCarga where

import Lib (CargarEvento, Evento(..))
import Graphics.UI.Gtk (widgetDestroy, entryGetText, onEntryActivate, widgetShowAll, windowMove, windowSetKeepAbove, containerChild, windowDecorated, containerBorderWidth, AttrOp((:=)), windowWindowPosition, WindowPosition(WinPosCenter), windowDefaultHeight, windowDefaultWidth, set, entryNew, screenGetHeight, screenGetWidth, windowGetScreen, windowNew)

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

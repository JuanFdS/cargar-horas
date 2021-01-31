{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module VentanaDeCarga where

import Lib (Registro(..), Evento(..), Tag(..))
import Graphics.UI.Gtk.Misc.Calendar (calendarNew)
import Graphics.UI.Gtk.Layout.Table (tableAttachDefaults, tableNew)
import Graphics.UI.Gtk.Abstract.Container (containerAdd)
import Graphics.UI.Gtk.Entry.Entry (entrySetText)
import Tiempo (Momento, minutosDelMomento, horaDelMomento, conHora, mkValidHoraDelDia, minutos, hora, momentoActual)
import Control.Monad (forM_)
import Graphics.UI.Gtk
    (statusIconActivated, menuPopup, statusIconPopupMenu, menuItemActivated, on, onActivate, menuShellAppend, menuItemNewWithLabel, mainQuit, menuNew, statusIconSetTooltipText, statusIconSetVisible, statusIconNewFromStock, statusIconNew,  Window,
      Table,
      WindowPosition(WinPosCenter),
      SpinButton,
      AttrOp((:=)),
      WidgetClass,
      containerChild,
      windowDefaultHeight,
      windowDefaultWidth,
      windowWindowPosition,
      set,
      widgetDestroy,
      widgetShowAll,
      entryGetText,
      entryNew,
      entrySetCompletion,
      onEntryActivate,
      entryCompletionModel,
      entryCompletionNew,
      entryCompletionSetTextColumn,
      spinButtonGetValueAsInt,
      spinButtonNewWithRange,
      spinButtonSetValue,
      spinButtonUpdate,
      screenGetHeight,
      screenGetWidth,
      tableAttachDefaults,
      tableNew,
      customStoreSetColumn,
      listStoreNew,
      makeColumnIdString,
      windowDecorated,
      windowGetScreen,
      windowMove,
      windowNew,
      windowSetKeepAbove,
      GObjectClass,
      ColumnId,
      Entry,
      EntryClass,
      EntryCompletion,
      ObjectClass )

mkmenu s = do
  m <- menuNew
  mapM_ (mkitem m) [("Quit" :: String,mainQuit)]
  return m
    where
        mkitem menu (label,act) =
            do i <- menuItemNewWithLabel label
               menuShellAppend menu i
               on i menuItemActivated act

mostrarVentanaDeCarga :: [String] -> Maybe String -> (Registro -> IO ()) -> IO ()
mostrarVentanaDeCarga tags defaultTag cargarRegistro = do
  icon <- statusIconNewFromStock "_Quit"
  statusIconSetVisible icon True
  statusIconSetTooltipText icon (Just "This is a test" :: Maybe String)
  menu <- mkmenu icon
  on icon statusIconPopupMenu  $ \b a -> do
         widgetShowAll menu
         print (b,a)
        --  menuPopup menu noWidget noWidget noMenuPositionFunc b a
  on icon statusIconActivated $
         putStrLn "'activate' signal triggered"
  registroInput <- registroInputNew tags
  window <- ventanaDeCargaNew
  set window [containerChild := contenedorRegistroInput registroInput]

  forM_ defaultTag (setDefaultTag registroInput)
  onRegistroInputActivate registroInput $ do
    getRegistro registroInput >>= \case
      Just registro -> cargarRegistro registro >> widgetDestroy window
      Nothing -> putStrLn "Algo salió mal"

  widgetShowAll window

ventanaDeCargaNew :: IO Window
ventanaDeCargaNew = do
  window <- windowNew
  pantalla <- windowGetScreen window
  (width, height) <- (,) <$> screenGetWidth pantalla <*> screenGetHeight pantalla
  set window [windowDefaultWidth := 500,
              windowDefaultHeight := 100,
              windowWindowPosition := WinPosCenter,
              windowDecorated := False]
  windowSetKeepAbove window True
  windowMove window width height
  pure window

data RegistroInput = RegistroInput {
  contenedorRegistroInput :: Table,
  momentoInput :: MomentoInput,
  tagEntry :: Entrada Tag,
  eventoEntry :: Entrada Evento
}

registroInputNew :: [String] -> IO RegistroInput
registroInputNew tagsPosibles = do
  eventoEntry <- eventoEntryNew
  tagEntry <- Entrada <$> entryWithCompletionNew tagsPosibles
  momentoInput <- momentoActual >>= momentoInputNew

  table <- tableNew 2 2 True
  tableAttachDefaults table eventoEntry                          0 2 0 1
  tableAttachDefaults table tagEntry                             0 1 1 2
  tableAttachDefaults table (momentoInputContainer momentoInput) 1 2 1 2

  pure $ RegistroInput table momentoInput tagEntry eventoEntry

setDefaultTag :: RegistroInput -> String -> IO ()
setDefaultTag registroInput defaultTag = entrySetText (tagEntry registroInput) defaultTag

getRegistro :: RegistroInput -> IO (Maybe Registro)
getRegistro (RegistroInput contenedor momentoInput tagEntry eventoEntry) = do
  evento <- getValor Evento eventoEntry
  tag <- getValor Tag tagEntry
  momento <- getMomento momentoInput
  pure $ Registro evento <$> momento <*> pure tag <*> pure []

onRegistroInputActivate :: RegistroInput -> IO () -> IO ()
onRegistroInputActivate registroInput accion = do
  onMomentoInputActivate (momentoInput registroInput) accion
  onEntryActivate (eventoEntry registroInput) accion
  onEntryActivate (tagEntry registroInput) accion
  pure ()

newtype Entrada valorEntrada = Entrada {
  _entry :: Entry
 } deriving (EntryClass, WidgetClass, ObjectClass, GObjectClass) via Entry

eventoEntryNew :: IO (Entrada Evento)
eventoEntryNew = Entrada <$> entryNew

getValor :: (String -> valorEntrada) -> Entrada valorEntrada -> IO valorEntrada
getValor f entrada = f <$> entryGetText entrada

entryWithCompletionNew :: [String] -> IO Entry
entryWithCompletionNew opciones = do
  autocompletado <- completionNew opciones
  entry <- entryNew
  entrySetCompletion entry autocompletado
  pure entry

completionNew :: [String] -> IO EntryCompletion
completionNew opciones = do
  completion <- entryCompletionNew
  store <- listStoreNew opciones
  customStoreSetColumn store compareCol id
  entryCompletionSetTextColumn completion compareCol
  set completion [entryCompletionModel := Just store]
  pure completion
  where compareCol :: ColumnId String String
        compareCol = makeColumnIdString 0

data MomentoInput = MomentoInput {
  _tiempoDeCreacion :: Momento,
  momentoInputContainer :: Table,
  _horasSpinButton :: SpinButton,
  _minutosSpinButton :: SpinButton
}

momentoInputNew :: Momento -> IO MomentoInput
momentoInputNew ahora = do
  horasEntrada <- spinButtonNewWithRange 0 23 1
  minutosEntrada <- spinButtonNewWithRange 0 59 1

  spinButtonSetValue horasEntrada (fromIntegral $ horaDelMomento ahora)
  spinButtonSetValue minutosEntrada (fromIntegral $ minutosDelMomento ahora)

  contenedor <- tableNew 1 2 True
  tableAttachDefaults contenedor horasEntrada 0 1 0 1
  tableAttachDefaults contenedor minutosEntrada 1 2 0 1

  pure $ MomentoInput ahora contenedor horasEntrada minutosEntrada

getMomento :: MomentoInput -> IO (Maybe Momento)
getMomento (MomentoInput tiempoActual contenedor horasSpinButton minutosSpinButton) = do
  horas <- spinButtonUpdate horasSpinButton >> spinButtonGetValueAsInt horasSpinButton
  minutos <- spinButtonUpdate minutosSpinButton >> spinButtonGetValueAsInt minutosSpinButton
  pure $ conHora tiempoActual <$> mkValidHoraDelDia horas minutos

onMomentoInputActivate :: MomentoInput -> IO () -> IO ()
onMomentoInputActivate momentoInput accion =
  forM_ [_horasSpinButton momentoInput, _minutosSpinButton momentoInput] (`onEntryActivate` accion)

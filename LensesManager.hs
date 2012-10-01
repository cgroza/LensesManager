module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Pixbuf
import LenseTools
import qualified Data.Map as M
import Data.Maybe
import Data.IORef
import Data.Char

data AppData = AppData {openButton :: FileChooserButton, lensesView :: IconView, lensesStore :: ListStore (String, Pixbuf), 
                        editWindow :: Window, lensData :: IORef [Lens]}

data EditEntries = EditEntries {nameEntry :: Entry, descriptionEntry :: Entry, shortcutEntry :: Entry,
                                hintEntry :: Entry, visibleCheckButton :: CheckButton, 
                                iconFileChooser :: FileChooserButton}

populateIconView :: AppData -> IO ()
populateIconView gui = do     
  listStoreClear $ lensesStore gui
  mapM_ (addIcon (lensesStore gui)) =<< readIORef (lensData gui)
  where addIcon lstore lens = do  
          p <- pixbufNewFromFile $ fromJust $ M.lookup LenseTools.Icon lens
          listStoreAppend lstore (fromJust $ M.lookup Name lens , p)

getSelectedLens :: AppData -> IO (Maybe Lens)
getSelectedLens AppData {lensesView = lview, lensData = ldata, lensesStore = lstore} = do
  selected <- iconViewGetSelectedItems lview 
  if not $ null selected then do
    itemRef <- treeRowReferenceNew lstore . head =<< iconViewGetSelectedItems lview 
    case itemRef of
      Just treeRef -> do val <- listStoreGetValue lstore . head =<< treeRowReferenceGetPath treeRef
                         return  . Just . (getLensByName (fst val)) =<< readIORef ldata
      Nothing -> return Nothing
  else return Nothing


onEditButton :: AppData -> EditEntries -> IO ()
onEditButton appData editEntries  = showEditWindow appData getSelectedLens
                                    editEntries

onOpenButton :: AppData -> IO ()
onOpenButton = undefined

showEditWindow :: AppData -> (AppData -> IO (Maybe Lens)) -> EditEntries -> IO ()
showEditWindow appData@(AppData {editWindow = ewin}) lensGetter
               EditEntries { descriptionEntry = dentry, nameEntry = nentry, shortcutEntry = sentry, 
                             hintEntry = hentry, visibleCheckButton = vcheck, iconFileChooser = ichooser}
  = do
  selected <- lensGetter appData
  case selected of
    Just selectedLens -> do
      entrySetText nentry $ getField Name selectedLens
      entrySetText dentry $ getField Description selectedLens
      entrySetText sentry $ getField Shortcut selectedLens
      entrySetText hentry $ getField SearchHint selectedLens
      fileChooserSetFilename ichooser $ getField LenseTools.Icon selectedLens
      toggleButtonSetActive vcheck $ (map toLower $ getField Visible selectedLens) `elem` ["","true"]
      widgetShowAll $ ewin
    Nothing -> return ()

closeSaveEditLensWin :: AppData -> EditEntries -> IO ()
closeSaveEditLensWin appData EditEntries { descriptionEntry = dentry, nameEntry = nentry, shortcutEntry = sentry, 
                                       hintEntry = hentry, visibleCheckButton = vcheck, iconFileChooser = ichooser}
  = do  widgetHideAll $ editWindow appData
        Just lens <- getSelectedLens appData
        nameVal <- entryGetText nentry
        descriptionVal <- entryGetText dentry
        hintVal <- entryGetText hentry
        shortcutVal <- entryGetText sentry
        visibleVal <- return . map toLower . show =<< toggleButtonGetActive vcheck
        iconPathVal <- return . fromJust  =<< fileChooserGetFilename ichooser
    
        newLens <- return $ foldl (\l (f, v) -> changeLens f v l ) lens $ zip [Name, Description, SearchHint, Shortcut, Visible, LenseTools.Icon]  
                      [nameVal, descriptionVal, hintVal, shortcutVal, visibleVal, iconPathVal]
        writeLens newLens
        writeIORef (lensData appData) =<< getAllLens
        populateIconView appData
main = 
  do
    initGUI
    Just xml <- xmlNew "GUI.glade"
    Just editWinXml <- xmlNew  "EditWindow.glade"

    editWindow  <- xmlGetWidget editWinXml castToWindow "edit_window"
    windowSetDeletable editWindow False

    openButton <- xmlGetWidget xml castToFileChooserButton "open_button"

    nameEntry <- xmlGetWidget editWinXml castToEntry "name_txt"
    shortcutEntry <- xmlGetWidget editWinXml castToEntry "shortcut_txt"
    descriptionEntry <- xmlGetWidget editWinXml castToEntry "description_txt"
    hintEntry <- xmlGetWidget editWinXml castToEntry "hint_txt"
    iconChooser <- xmlGetWidget editWinXml castToFileChooserButton "icon_chooser"
    visibilityCheck <- xmlGetWidget editWinXml castToCheckButton "visible_checkbox"

    window <- xmlGetWidget xml castToWindow "top_window"
    widgetSetSizeRequest window 700 400
    onDestroy window mainQuit

    -- setup the icon view
    lensesView <- xmlGetWidget xml castToIconView "lenses_view"
    storeSource <- listStoreNew ([] :: [(String, Pixbuf)]) 
    treeModelSetColumn storeSource (makeColumnIdString 1) fst
    treeModelSetColumn storeSource (makeColumnIdPixbuf 2) snd
    iconViewSetModel lensesView (Just storeSource)
    iconViewSetTextColumn lensesView (makeColumnIdString 1)
    iconViewSetPixbufColumn lensesView (makeColumnIdPixbuf 2)
    iconViewSetModel lensesView (Just storeSource)

    lensData <- newIORef =<< getAllLens

    let gui = AppData openButton lensesView storeSource editWindow lensData
    let editEntries = EditEntries nameEntry descriptionEntry shortcutEntry hintEntry visibilityCheck iconChooser 

    afterFileActivated openButton $ onOpenButton gui

    editButton <- xmlGetWidget xml castToButton "edit_button"
    onClicked editButton $ onEditButton gui editEntries

    okButton <- xmlGetWidget xml castToButton "ok_button"
    onClicked okButton mainQuit

    editWinOkBt <- xmlGetWidget editWinXml castToButton "ok_button"
    onClicked editWinOkBt (closeSaveEditLensWin gui editEntries)


    populateIconView gui
    widgetShowAll window
    mainGUI



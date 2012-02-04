module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Pixbuf
import LenseTools
import qualified Data.Map as M
import Data.Maybe


data GUI = GUI {lensesView :: IconView, lensesStore :: ListStore (String, Pixbuf)}

populateIconView :: GUI -> IO ()
populateIconView gui = do     
  mapM_ (addIcon (lensesStore gui)) =<< getAllLens
  where addIcon lstore lens = do  
          p <- pixbufNewFromFile $ fromJust $ M.lookup LenseTools.Icon lens
          listStoreAppend lstore (fromJust $ M.lookup Name lens , p)


showEditWindow :: GUI -> IO ()
showEditWindow GUI {lensesView = lview, lensesStore = lstore} = do
  selected <- treeRowReferenceNew lstore . head =<< iconViewGetSelectedItems lview 
  case selected of
    Just treeRef -> do val <- listStoreGetValue lstore . head =<< treeRowReferenceGetPath treeRef
                       print $ fst val
    Nothing -> return ()
  

main = 
  do
    initGUI
    Just xml <- xmlNew "GUI.glade"
    window   <- xmlGetWidget xml castToWindow "top_window"

    widgetSetSizeRequest window 300 350
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

    let gui = GUI lensesView storeSource

    editButton <- xmlGetWidget xml castToButton "edit_button"
    onClicked editButton $ showEditWindow gui

    okButton <- xmlGetWidget xml castToButton "ok_button"
    onClicked okButton mainQuit

    populateIconView gui

    widgetShowAll window
    lens <- getAllLens
    mapM_ print lens


    mainGUI



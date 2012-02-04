module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import LenseTools

data GUI = GUI {lensesView :: IconView}

populateIconView :: IO ()
populateIconView = undefined

showEditWindow :: IO ()
showEditWindow = undefined

main = 
  do
    initGUI
    Just xml <- xmlNew "GUI.glade"
    window   <- xmlGetWidget xml castToWindow "top_window"
    widgetSetSizeRequest window 300 350
    onDestroy window mainQuit

    editButton <- xmlGetWidget xml castToButton "edit_button"
    onClicked editButton showEditWindow

    okButton <- xmlGetWidget xml castToButton "ok_button"
    onClicked okButton mainQuit

    lensesView <- xmlGetWidget xml castToIconView "lenses_view"
    let gui = GUI lensesView

    widgetShowAll window

    lens <- getAllLens
    mapM_ print lens


    mainGUI



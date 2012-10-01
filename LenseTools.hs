module LenseTools  where

import System.Directory
import System.FilePath.Posix
import System.Directory
import qualified System.IO.Strict as IOS
import System.IO
import Text.Regex
import Data.List
import Data.Maybe
import Data.List
import qualified Data.Map as M

type Lens = M.Map Field Value

data Field = Icon | Name | Description | SearchHint | Shortcut | Visible | Path  deriving (Show, Eq, Ord)
type Value = String

lensDir = "/usr/share/unity/lenses"

changeLens :: Field -> Value -> Lens -> Lens
changeLens = M.insert

writeLens :: Lens -> IO ()
writeLens lens = do
  content <- IOS.readFile path
  writeFile tempPath $ foldl changeLensTxt content $ M.toList $ M.delete Path lens
  copyFile tempPath path
  removeFile tempPath
  where path = getField Path lens
        tempPath = addExtension path "lmng"
        changeLensTxt lensTxt (field, val) =
          let regex = mkRegex $ "^" ++ show field ++ ".*=.*$" 
              replacement = show field ++ "=" ++ val in 
          if isNothing $ matchRegexAll regex lensTxt then
            subRegex (mkRegex "^\\[Desktop Entry\\]") lensTxt (replacement ++ "\n[Desktop Entry]")
          else
            subRegex regex lensTxt replacement

readAttr :: String -> Field -> Value
readAttr txt field =  case matchRegexAll regex txt of
  Just (_, s, _, _) -> if not $ null s then  
                         splitRegex (mkRegex "=") ( s) !! 1
                       else ""
  Nothing ->  ""
  where regex = mkRegex $ "^" ++ show field ++ ".*=.*$"

readLens :: FilePath -> IO Lens
readLens file = do contents <- IOS.readFile file
                   return $ M.insert Path file $ M.fromList $ zip fields $ map 
                     (readAttr contents) fields 
                     where fields = [Icon , Name , Description , SearchHint , Shortcut , Visible]

getLens :: FilePath -> IO [Lens]
getLens filePath = (mapM readLens . map ((lensDir </>) . (filePath </>)) . filter isLens) =<< (getDirectoryContents (lensDir </> filePath))
  where isLens = isSuffixOf ".lens" 

getField :: Field -> Lens -> Value
getField f = fromMaybe "" . M.lookup f

getAllLens :: IO [Lens]
getAllLens = return . concat =<< mapM getLens =<< getDirectoryContents lensDir

getLensByName :: String -> [Lens] -> Lens
getLensByName name lens = lens !! (fromJust $ findIndex ((name ==) . fromMaybe "" . M.lookup Name) lens)
  
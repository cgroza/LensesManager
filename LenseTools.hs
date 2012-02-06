module LenseTools  where

import System.Directory
import System.FilePath.Posix
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
  content <- readFile path
  writeFile path $ foldl changeLensTxt content $ M.toList lens
  where path = getField Path lens
        changeLensTxt lensTxt (field, val) =
          let regex = mkRegex $ "^" ++ show field ++ ".*=.*$" in 
          subRegex regex lensTxt (show field ++ "=" ++ val)

readAttr :: String -> Field -> Value
readAttr txt field =  case matchRegexAll regex txt of
  Just (_, s, _, _) -> if not $ null s then  
                         splitRegex (mkRegex "=") ( s) !! 1
                       else ""
  Nothing ->  ""
  where regex = mkRegex $ "^" ++ show field ++ ".*=.*$"

getLens :: FilePath -> IO [Lens]
getLens filePath = (mapM readLens . map ((lensDir </>) . (filePath </>)) . filter isLens) =<< (getDirectoryContents (lensDir </> filePath))
  where fields = [Icon , Name , Description , SearchHint , Shortcut , Visible]
        isLens = isSuffixOf ".lens" 
        readLens file = do contents <- readFile file
                           return $ M.insert Path file $ M.fromList $ zip fields $ map (readAttr contents) fields 
          
getField :: Field -> Lens -> Value
getField f = fromMaybe "" . M.lookup f

getAllLens :: IO [Lens]
getAllLens = return . concat =<< mapM getLens =<< getDirectoryContents lensDir

getLensByName :: String -> [Lens] -> Lens
getLensByName name lens = lens !! (fromJust $ findIndex ((name ==) . fromMaybe "" . M.lookup Name) lens)
  
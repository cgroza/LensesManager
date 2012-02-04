module LenseTools  where

import System.Directory
import System.FilePath.Posix
import qualified System.IO.Strict as IOS
import System.IO
import Text.Regex
import Data.List
import Data.Maybe
import qualified Data.Map as M

type Lens = M.Map Field Value

data Field = Icon | Name | Description | SearchHint | Shortcut | Visible | Path  deriving (Show, Eq, Ord)
type Value = String

lensDir = "/usr/share/unity/lenses"

changeLens :: Lens -> Field -> Value -> IO ()
changeLens lens field val = do
  writeFile path . flip (subRegex regex)  (show field ++ "=" ++val) =<< IOS.readFile path
  where regex = mkRegex $ "^" ++ show field ++ ".*=.*$"
        path = fromJust $ M.lookup Path lens

getAttr :: String -> Field -> Value
getAttr txt field = 
  case matchRegexAll regex txt of
    Just (_, s, _, _) -> if not $ null s then  
                splitRegex (mkRegex "=") ( s) !! 1
              else ""
    Nothing ->  ""
  where regex = mkRegex $ "^" ++ show field ++ ".*=.*$"

getLens :: FilePath -> IO [Lens]
getLens filePath = (mapM readLens . map ((lensDir </>) . (filePath </>)) . filter isLens) =<< (getDirectoryContents (lensDir </> filePath))
  where fields = [Icon , Name , Description , SearchHint , Shortcut , Visible]
        isLens = isSuffixOf ".lens" 
        readLens :: FilePath -> IO Lens
        readLens file = do
          contents <- readFile file
          return $ M.insert Path file $ M.fromList $ zip fields $ map (getAttr contents) fields 
          

getAllLens :: IO [Lens]
getAllLens = return . concat =<< mapM getLens =<< getDirectoryContents lensDir




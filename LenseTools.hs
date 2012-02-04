module LenseTools  where

import System.Directory
import System.FilePath.Posix
import qualified System.IO.Strict as IOS
import System.IO
import Text.Regex
import Data.List


data Lens = Lens {name :: String , path :: FilePath } deriving Show

data Field = Icon | Name | Description | SearchHint | Shortcut | Visible deriving Show
type Value = String

lensDir = "/usr/share/unity/lenses"

changeLens :: Lens -> Field -> Value -> IO ()
changeLens lens field val = do
  writeFile (path lens) . flip (subRegex regex)  (show field ++ "=" ++val) =<< IOS.readFile (path lens)
  where regex = mkRegex $ "^" ++ show field ++ ".*=.*$"

getLens :: FilePath -> IO [Lens]
getLens filePath = (return .  map readLens . map ((lensDir </>) . (filePath </>)) . filter isLens) =<< (getDirectoryContents (lensDir </> filePath))
  where isLens = isSuffixOf ".lens" 
        readLens f = let name = dropExtension $ snd $ splitFileName f
                         path = f in Lens name path

getAllLens :: IO [Lens]
getAllLens = return . concat =<< mapM getLens =<< getDirectoryContents lensDir




module Main (main) where
import Aur
import Data.Aeson 
import System.Environment
import Control.Applicative
import Control.Monad
import System.Console.GetOpt
import Data.Maybe


main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then do
            let pkg = head args
            d <- (eitherDecode <$> getJSON pkg) :: IO (Either String AUR)
            case d of
                Left err -> print err
                Right ps -> mapM_ prettyPrint $ getPKGList ps
    else putStrLn "Usage: aur PKG"

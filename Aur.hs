{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Aur where

import Data.Aeson 
import Data.Aeson.Types hiding (Options)
import qualified Data.Text as DT
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
import Network.HTTP.Conduit (simpleHttp)
import System.Console.ANSI  -- requires ansi-terminal installed
import System.Environment
import System.Console.GetOpt
import Data.Maybe


data AUR = AUR
         { _version :: Int
         , _type :: String
         , resultcount :: Int
         , results :: [Package]
         } deriving (Show, Generic)


data Package = Package
             { id               :: Int
             , name             :: String
             , packageBaseID    :: Int
             , packageBase      :: String
             , version          :: String
             , categoryID       :: Int
             , description      :: String
             , url              :: String
             , numVotes         :: Int
             , outOfDate        :: Int
             , maintainer       :: Maybe String
             , firstSubmitted   :: Int
             , lastModified     :: Int
             , license          :: Maybe String
             , urlPath          :: String
             } deriving (Show, Generic)


instance FromJSON Package where
    parseJSON (Object o) = Package <$>
                            o .: "ID"               <*>
                            o .: "Name"             <*>
                            o .: "PackageBaseID"    <*>
                            o .: "PackageBase"      <*>
                            o .: "Version"          <*>
                            o .: "CategoryID"       <*>
                            o .: "Description"      <*>
                            o .: "URL"              <*>
                            o .: "NumVotes"         <*>
                            o .: "OutOfDate"        <*>
                            o .:? "Maintainer"      <*>
                            o .: "FirstSubmitted"   <*>
                            o .: "LastModified"     <*>
                            o .:? "License"         <*>
                            o .: "URLPath"
    parseJSON _ = mzero


instance FromJSON AUR where
    parseJSON (Object o) = AUR <$>
                            o .: "version"        <*>
                            o .: "type"           <*>
                            o .: "resultcount"    <*>
                            o .: "results"
    parseJSON _ = mzero


putColorStr :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
putColorStr fgi fg bgi bg line = do
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground fgi fg, SetColor Background bgi bg]
    putStr line
    setSGR []


getJSON :: String -> IO BL.ByteString
getJSON arg = simpleHttp $ baseURL ++ arg
    where baseURL = "https://aur.archlinux.org/rpc.php?type=search&arg="


getPKGList :: AUR -> [Package]
getPKGList = results


wrapLine :: Int -> String -> String
wrapLine len str = unwords $ wrap [""] $ words str
    where
        wrap :: [String] -> [String] -> [String]
        wrap ls [] = ls
        wrap ls@(l:rest) wl@(w:ws)
            | length l <= len = wrap (l : (w ++ " ")) ws
            | length l >= len = wrap rest wl
            | otherwise = ls


prettyPrint :: Package -> IO ()
prettyPrint pkg = do
    let prefix = "https://aur.archlinux.org"
    putColorStr Vivid Magenta Dull Black ("* " ++ name pkg)
    putColorStr Vivid Green Dull Black (" " ++ version pkg ++ "\n")
    --putColorStr Vivid Blue Dull Black ("    " ++ prefix ++ urlPath pkg ++ "\n")
    --mapM_ (\x -> putColorStr Dull White Dull Black ("    " ++ x ++ "\n")) (wrapLine $ description pkg)
    putColorStr Vivid Blue Dull Black ("    Homepage: " ++ url pkg ++ "\n")

module Model where

import Yesod hiding (get)
import qualified Data.ByteString as S
import Data.ByteString.Char8 (unpack)
import System.Directory
import System.IO
import Data.Serialize
import Control.Arrow
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative
import Control.Exception (SomeException, handle, evaluate)
import Data.Time
import System.Locale

cs = unpack

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

loadEntry :: String -> IO (Maybe Entry)
loadEntry slug = do
    let fp = "entries/" ++ slug
    exists <- doesFileExist fp
    if exists
        then do
            withFile fp ReadMode $ \h -> do
                title <- S.hGetLine h
                date <- S.hGetLine h
                contents <- S.hGetContents h
                return $ Just Entry
                    { entrySlug = slug
                    , entryTitle = cs title
                    , entryDate = read $ cs date
                    , entryContent = preEscapedString $ cs contents
                    }
        else return Nothing

data Entry = Entry
    { entrySlug :: String
    , entryTitle :: String
    , entryDate :: Day
    , entryContent :: Html
    }

loadArchive :: IO Archive
loadArchive = do
    let fp = "archive.dat"
    exists <- doesFileExist fp
    ar <- if exists
            then do
                s <- S.readFile fp
                case decode s of
                    Left _ -> return Nothing
                    Right x -> return $ Just x
            else return Nothing
    case ar of
        Just x -> return x
        Nothing -> do
            x <- loadArchive'
            S.writeFile fp $ encode x
            return x

loadArchive' :: IO Archive
loadArchive' = do
    let top = "entries/"
    allContents <- getDirectoryContents top
    allFiles <- filterM (\f -> doesFileExist $ top ++ f) allContents
    pairs <- mapM (go top) allFiles
    return $ map (fst . head &&& map snd)
           $ groupBy ((==) `on` fst)
           $ map (first toYearMonth)
           $ reverse
           $ sortBy (compare `on` fst)
             pairs
  where
    go top f = do
        withFile (top ++ f) ReadMode $ \h -> do
            print f
            title <- cs <$> S.hGetLine h
            date <- cs <$> S.hGetLine h
            let handler _ = do putStrLn $ "Error parsing date: " ++ date
                               return undefined
            day <- handleAny handler $ evaluate (read date :: Day)
            let result = (day, EntryInfo f title)
            return result
    toYearMonth = formatTime defaultTimeLocale "%B %Y"

data EntryInfo = EntryInfo
    { eiSlug :: String
    , eiTitle :: String
    }
  deriving Show

instance Serialize EntryInfo where
    put (EntryInfo a b) = put a >> put b
    get = EntryInfo <$> get <*> get

type YearMonth = String

type Archive = [(YearMonth, [EntryInfo])]

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%B %d, %Y"

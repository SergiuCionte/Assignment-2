module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
import Text.Printf (printf)

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
              let newEmptyDB = DB.empty
              DB.save newEmptyDB
              return ();

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  loadedDB <- DB.load
  result <- case loadedDB of
    (Error err) -> return "Failed to load DB"
    (Success db) ->
      let
        foundEntry = DB.findFirst (\entry -> entryId entry == getOptId getOpts) db
      in
        case foundEntry of
            Nothing -> return "No entry was found"
            Just entry -> return $ entrySnippet entry
  putStrLn result
-- | Handle the search command and print the result
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  loadedDB <- DB.load
  result <- case loadedDB of
    (Error err) -> return "Failed to load DB"
    (Success db) ->
      let
        foundEntries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) db
      in
        case foundEntries of
          [] -> return "No entries found"
          _ -> return $ unlines (map (show . FmtEntry) foundEntries)
  putStrLn result
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  fileContent <- readFile (addOptFilename addOpts)
  loadedDB <- DB.load
  case loadedDB of
    (Error err) -> putStrLn "Failed to load DB"
    (Success db) ->
      case DB.findFirst (\entry -> entrySnippet entry == fileContent) db of
        Just entry ->
          putStrLn $ "Entry with this content already exists:\n" ++ getEntryIdAndFileName entry
        Nothing -> do
          DB.modify (DB.insertWith (\id -> makeEntry id fileContent addOpts))
          putStrLn "Entry added successfully"

  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

{- Helper function:
-- print only the id and the fileName of the given Entry (the first line from the formatted string with FmtEntry)
-}
getEntryIdAndFileName :: Entry -> String
getEntryIdAndFileName entry = head $ lines $ show (FmtEntry entry)

addOptsToEntry :: Int -> String -> AddOptions -> Entry
addOptsToEntry id snippet addOpts =
  Entry
  { entryId = id
  , entrySnippet = snippet
  , entryFilename = addOptFilename addOpts
  , entryLanguage = addOptLanguage addOpts
  , entryDescription = addOptDescription addOpts
  , entryTags = addOptTags addOpts
  }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
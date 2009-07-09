{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
module Main where

import Container.Tree.Persistent
import Control.Monad.State
import Data.Char
import Data.List hiding (insert, lookup)
import Data.OBO
import Generic.Aspect (unwrap)
import Prelude hiding (lookup)
import Storage.FileStorage
import System.Environment
import System.IO
import System.Posix.Files

instance TreeClass String
instance TreeClass Entry
type OBO_DB = TreeP String Entry

trim :: String -> String
trim =
    reverse
  . dropWhile isSpace
  . reverse
  . dropWhile isSpace

insertEntry :: Entry -> OBO_DB  -> Storage t OBO_DB
insertEntry b p = do
  liftIO $ (putChar '.' >> hFlush stdout)
  insert (name b) b p

sep :: Show b => String -> Storage t b -> Storage t b
sep a b =
  do liftIO $ putStrLn ("\n----[  " ++ a ++ "  ]-----------------\n")
     r <- b
     liftIO $ print (":::", r)
     liftIO $ putStrLn "- - - - - - -"
     debug
     return r

fromList :: [Entry] -> Storage t OBO_DB
fromList xs = foldl' (\a b -> a >>= insertEntry b) empty xs

main :: IO ()
main = 
  do args <- getArgs
     case args of
       ["build", source, db] -> build source db
       ["query", db]         -> query db
       ["stats", db]         -> stats db
       ["dump",  db]         -> dump  db
       _  ->
         do putStrLn "[ERROR] Invalid arguments, try one of the following:"
            putStrLn "  main build <source.obo> <database.db>"
            putStrLn "  main query <database.db>"
            putStrLn "  main stats <database.db>"
            putStrLn "  main dump  <database.db>"

build :: FilePath -> FilePath -> IO ()
build source db = do
  file <- readFile source
  let k = parseOBO file
  case k of
    Left e    -> print e
    Right doc -> 
      do let stanzas = docStanzas doc
             entries = map stanzaToEntry stanzas
         setFileSize db 0 -- reset DB.
         run db $
           do o <- store nullP
              p <- fromList entries
              liftIO $ putStrLn []
              liftIO $ print (o, p)
              reuse o (unwrap p)
              liftIO $ print "done"

              return ()

query :: FilePath -> IO ()
query db = 
  run db $
    do p <- retrieve nullP
       liftIO $ print p
       loop p
  where
    loop p =
      do s <- liftIO $ (putStr "query> " >> hFlush stdout >> getLine)
         lookup (trim s) p >>= \a -> liftIO (print (a :: Maybe Entry))
         loop p

stats :: FilePath -> IO ()
stats db = 
  run db $
    do p <- (retrieve nullP :: Storage t OBO_DB)
       liftIO $ print p
       count p >>= \(c :: Int) -> liftIO (putStr "count: " >> print c)
       depth p >>= \(c :: Int) -> liftIO (putStr "depth: " >> print c)

dump :: FilePath -> IO ()
dump db = run db debug


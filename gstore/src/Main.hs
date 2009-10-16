module Main where

import Control.Applicative
import Control.Monad.Lazy
import Control.Monad.State
import Data.Char
import Data.List
import Data.OBO
import Prelude
import Generics.Seq
import Heap.Heap
import System.Environment
import System.IO
import qualified Container.Tree.PersistentCont as C
import qualified Container.Tree.PersistentMorph as M

type OBO_DB = M.Tree String Entry

insertEntry :: Entry -> OBO_DB -> HeapW OBO_DB
insertEntry b p =
  do liftIO (putChar '.' >> hFlush stdout)
     M.insert (name b) b p

fromList :: [Entry] -> HeapW OBO_DB
fromList xs = foldl' (\a b -> a >>= insertEntry b) C.empty xs

main :: IO ()
main = 
  do args <- getArgs
     putStrLn "Started."
     case args of
       ["build", source, db] -> build source db
       ["query", db]         -> query db
       ["stats", db]         -> stats db
--        ["dump",  db]         -> dump  db
       _  ->
         do putStrLn "[ERROR] Invalid arguments, try one of the following:"
            putStrLn "  main build <source.obo> <database.db>"
            putStrLn "  main query <database.db>"
            putStrLn "  main stats <database.db>"
            putStrLn "  main dump  <database.db>"

build :: FilePath -> FilePath -> IO ()
build source db =
  do file <- readFile source
     let k = parseOBO file
     case k of
       Left e    -> print e
       Right doc -> 
         do let stanzas = docStanzas doc
                entries = map stanzaToEntry stanzas
            run db $
              do o <- store nullPtr
                 p <- fromList entries
                 liftIO (putStrLn [])
                 liftIO (print (o, p))
                 unsafeReuse o p
                 liftIO (print "done")
                 return ()

query :: FilePath -> IO ()
query db =
    do p <- run db $ liftLazy $
         do o <- retrieve nullPtr
            v <- step o
            liftIO $ print (dseq v ())
            return v
       print p

step :: OBO_DB -> HeapR (Maybe Entry)
step p =
  do s <- liftIO (putStr "\nM-query> " >> hFlush stdout >> getLine)
     M.lookup (trim s) p

stats :: FilePath -> IO ()
stats db = 
  do (c, d) <- run db $ liftLazy $
       do p <- retrieve nullPtr :: HeapR OBO_DB
          ((,) <$> C.count p <*> C.depth p) :: HeapR (Int, Int)
     print c
     print d

-- dump :: FilePath -> IO ()
-- dump db = run db debug

trim :: String -> String
trim =
    reverse
  . dropWhile isSpace
  . reverse
  . dropWhile isSpace


module Main where

import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.List
import Data.OBO
import Prelude
import Storage.FileHeap
import System.Environment
import System.IO
-- import System.Posix.Files
-- import qualified Container.Tree.Abstract as F
import qualified Container.Tree.PersistentCont as C
import qualified Container.Tree.PersistentMorph as M
-- import Generics.Representation
-- import System.IO.Unsafe

type OBO_DB = M.Tree String Entry

insertEntry :: Entry -> OBO_DB -> HeapRW OBO_DB
insertEntry b p =
  do liftIO (putChar '.' >> hFlush stdout)
     M.insert (name b) b p

fromList :: [Entry] -> HeapRW OBO_DB
fromList xs = foldl' (\a b -> a >>= insertEntry b) C.empty xs

main :: IO ()
main = 
  do args <- getArgs
     putStrLn "Started."
     case args of
--        ["build", source, db] -> build source db
       ["query", db]         -> query db
--        ["stats", db]         -> stats db
--        ["dump",  db]         -> dump  db
--        ["test",  db]         -> test  db
       _  ->
         do putStrLn "[ERROR] Invalid arguments, try one of the following:"
            putStrLn "  main build <source.obo> <database.db>"
            putStrLn "  main query <database.db>"
            putStrLn "  main stats <database.db>"
            putStrLn "  main dump  <database.db>"
            putStrLn "  main test  <database.db>"

{-build :: FilePath -> FilePath -> IO ()
build source db =
  do file <- readFile source
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
                 liftIO (putStrLn [])
                 liftIO (print (o, p))
                 reuse o p
                 liftIO (print "done")
                 return ()-}

{-test :: FilePath -> IO ()
test db =
  do () <- runHeap db $ lazy $
       do p <- retrieve nullP :: HeapRO OBO_DB
          q <- retrieve p 
--           (l, r) <- level q
--           (ll, lr) <- level l
--           (rl, rr) <- level r
          liftIO $ print (q) -- {-p, q,{- l, ll, lr, -}r {-rl-},-} rr)
     return ()

level f =
  do a <- (retrieve . out . F.leftT)  f
     b <- (retrieve . out . F.rightT) f
     return (a, b)
-}

query :: FilePath -> IO ()
query db = 
  do xs <- runHeap db (lazy (retrieve nullP >>= reader))
     print "blaat"
     print xs
  where
    reader p =
      do s <- liftIO (putStr "M-query> " >> hFlush stdout >> getLine)
         M.lookup (trim s) p :: HeapRO (Maybe Entry)

{-

stats :: FilePath -> IO ()
stats db = 
  run db $
    do p <- (retrieve nullP :: Storage t OBO_DB)
       liftIO (print p)
       C.count p >>= \(c :: Int) -> liftIO (putStr "count: " >> print c)
       C.depth p >>= \(c :: Int) -> liftIO (putStr "depth: " >> print c)

dump :: FilePath -> IO ()
dump db = run db debug
-}

trim :: String -> String
trim =
    reverse
  . dropWhile isSpace
  . reverse
  . dropWhile isSpace


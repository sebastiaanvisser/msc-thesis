{-# LANGUAGE TemplateHaskell #-}
module Meta where

import Data.List
import Data.Either
import Data.Ord
import Data.Maybe
import Control.Monad
import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------

-- Commonalities.

instance Functor ((,,) a b) where
  fmap f (a, b, c) = (a, b, f c)

-------------------------------------------------------------------------------

-- Common TH utilities.

declName :: Dec -> Maybe Name
declName (FunD n _)            = Just n
declName (DataD _ n _ _ _)     = Just n
declName (NewtypeD _ n _ _ _)  = Just n
declName (TySynD n _ _)        = Just n
declName (ClassD _ n _ _ _)    = Just n
declName (SigD n _)            = Just n
declName _                     = Nothing

isFun :: Dec -> Bool
isFun (FunD _ _) = True
isFun _          = False

isSig :: Dec -> Bool
isSig (SigD _ _) = True
isSig _          = False

isData :: Dec -> Bool
isData (DataD _ _ _ _ _) = True
isData _                 = False

{-
Check whether a list of declarations is a proper function/signature pair or
something completely different.
-}

funSig :: [Dec] -> Either [Dec] (Dec, Dec)
funSig [s@(SigD n _), f@(FunD m _)] | n == m = Right (s, f)
funSig [f@(FunD n _), s@(SigD m _)] | n == m = Right (s, f)
funSig ds                                    = Left ds

{-
-}

groupDeclsByName :: [Dec] -> [[Dec]]
groupDeclsByName = 
    groupBy (\a b -> declName a == declName b)
  . sortBy (comparing declName)

namedDecls :: [[Dec]] -> [[Dec]]
namedDecls = filter (isJust . declName . head)

unnamedDecls :: [[Dec]] -> [Dec]
unnamedDecls = concat . filter (isNothing . declName . head)

sigFunDecls :: [[Dec]] -> [(Dec, Dec)]
sigFunDecls = rights . map funSig

dataDecls :: [[Dec]] -> [Dec]
dataDecls = concat . filter (isData . head)

-- Replace the occurrence of a certain part of a type with another part.

typeReplace :: Type -> Type -> Type -> Type
typeReplace a b t =
  let rec = typeReplace a b in
  case t of
    t | a == t    -> b
    ForallT n c t -> ForallT n c (rec t)
    AppT t v      -> AppT (rec t) (rec v)
    t             -> t

ctorReplaceType :: Type -> Type -> Con -> Con
ctorReplaceType a b con =
  let rec  = fmap (typeReplace a b)
      rec' = fmap (typeReplace a b) in
  case con of
    NormalC n ts    -> NormalC n (map rec ts)
    RecC n ts       -> RecC n (map rec' ts)
    InfixC l n r    -> InfixC (rec l) n (rec r)
    ForallC n c con -> ForallC n c (ctorReplaceType a b con)

-- Create a type from a `data' declaration as it would look when it is fully used.

fullTypeFromDataDecl :: Dec -> Type
fullTypeFromDataDecl (DataD ctx nm ps cs ds) = 
  foldl AppT (ConT nm) (map VarT ps) 

undefinedClause :: ClauseQ
undefinedClause = clause [] (return $ NormalB (VarE 'undefined)) []

-------------------------------------------------------------------------------

-- Real world stuff we need to know.

type Producer f g m   = (f g -> m g) -> m g
type Query    f g m c = (g -> m c) -> f g -> m c
type Modifier f g m c = (f g -> m g) -> (g -> m c) -> f g -> m c

-------------------------------------------------------------------------------

{-

Parametrize data type with additional fresh variable f and change all recursive
occurrences of the data type within itself with this f.

Query Type Translation:
  parseQuerySig returns   (A, B)
  translate this into:    Query A f m B
  so the original type:   ctx => tp (A -> B)
  becomes:                ctx => tp (Query A f m B)

-}

-------------------------------------------------------------------------------

convert :: Q [Dec] -> Q [Dec]
convert decls = do
  ds <- decls
  let named = namedDecls $ groupDeclsByName ds
      datas = dataDecls named
      funs  = sigFunDecls named
  newDatas <- mapM openupDataDecl datas
  newFuns  <- mapM monadifyQuery  funs
  return (newDatas ++ concat newFuns)

-------------------------------------------------------------------------------

-- (AppT (AppT (ConT n) (ps !! 0)) (ps !! 1))

openupDataDecl :: Dec -> Q Dec
openupDataDecl dec@(DataD ctx nm ps cs ds) = do
  f <- newName "f"
  let ps'  = ps ++ [f]
      self = fullTypeFromDataDecl dec
      cs'  = map (ctorReplaceType self (VarT f)) cs
  runIO (mapM_ print cs)
  runIO (mapM_ print cs')
  return (DataD ctx nm ps' cs' ds)
openupDataDecl dec = return dec

-------------------------------------------------------------------------------

monadifyQuery :: (Dec, Dec) -> Q [Dec]
monadifyQuery (sig, fun) = do
  fun' <- changeFunction fun
  sig' <- mkQueryType sig
  runIO (print (ppr sig'))
  return [sig', fun']

changeFunction :: Dec -> Q Dec
changeFunction (FunD nm clauses) = do
--   cs <- mapM changeClause clauses
  cs <- undefinedClause
  return (FunD nm [cs])

changeClause :: Clause -> Q Clause
changeClause (Clause pats body decs) =
  return (Clause pats body decs)


-- Turn a type signature for a query function into a monadic variant.

mkQueryType :: Dec -> Q Dec
mkQueryType (SigD nm tp) = do
  let (QueryType vs cx t g c) = parseQuerySig tp
  m <- newName "m"
  f <- newName "f"
  let q     = ConT (mkName "Query") `AppT` g `AppT` (VarT f) `AppT` (VarT m) `AppT` c
      monad = AppT (ConT (mkName "Monad")) (VarT m)
      vs'   = m : f : vs
  return (SigD nm (ForallT vs' (monad : cx) q))

parseQuerySig :: Type -> QueryType
parseQuerySig tp =
  fromMaybe (error "processing non-query function") (rec tp)
  where
    rec (ForallT v c t)          = fmap (\qt -> qt { qtVars = v, qtCxt = c}) (rec t)
    rec (AppT (AppT ArrowT a) b) = rec b <|> Just (QueryType [] [] a a b)
    rec _                        = Nothing

{-
Select the container type and the query result type from a query type
signature. A query type signature has the following form:

  forall <x>. ctx >= A (f -> c)
  
Where `<x>' are one or more type variables.
Where `ctx' is some arbitrary context.
Where `A' as some type constructor, possible contaiing `->'
Where `f' is the container data type.
Where `c' is the query result.
-}

data QueryType =
  QueryType {
    qtVars  :: [Name]
  , qtCxt   :: Cxt
  , qtType  :: Type
  , qtCont  :: Type
  , qtRes   :: Type
  }


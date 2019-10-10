{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module OdbcUtils where
import Control.Lens
import qualified Database.ODBC.SQLServer as O
import Database.ODBC.SQLServer (ResultSet(..), Column(..), ResultSets)
import Control.Lens.Error
import Data.Maybe
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import GHC.Stack
import Data.List
import GHC.Generics (Generic)
import Numeric

pattern BinaryValueP :: ByteString -> O.Value
pattern BinaryValueP bs = O.BinaryValue (O.Binary bs)

bmonoid :: HasCallStack => O.Value -> ByteString
bmonoid =
  \case
     O.BinaryValue (O.Binary a) -> a
     O.ByteStringValue a -> a
     O.NullValue -> mempty
     o -> error $ "bconcat: expected ByteStringValue or BinaryValue or NullValue only but found " ++ show o

bconcat :: HasCallStack => [O.Value] -> ByteString
bconcat = foldl' (\a b -> a <> bmonoid b) mempty


newtype IntLike = IntLike { _unIntLike :: Int } deriving (Generic, Show, Eq)

makeLenses ''IntLike

instance O.FromValue IntLike where
  fromValue =
    \case
       O.DoubleValue d -> IntLike <$> truncEpsilon d
       O.FloatValue d -> IntLike <$> truncEpsilon d
       O.IntValue i -> pure $ IntLike i
       O.ByteStringValue b -> case readFloat @Double (BS.unpack b) of
                              [(d,"")] -> IntLike <$> truncEpsilon d
                              _ -> Left $ "IntValue: invalid number " ++ BS.unpack b
       o -> Left $ "IntLike conversion error: " ++ show o

truncEpsilon :: RealFrac w => w -> Either String Int
truncEpsilon d =
  let r = truncate @_ @Int d
  in if abs (d - fromIntegral r) <= 0.01 then pure r
     else Left "diff larger than epsilon"

newtype StringLike = StringLike { _unStringLike :: Text } deriving (Generic, Show, Eq)

makeLenses ''StringLike

instance O.FromValue StringLike where
  fromValue =
    \case
       O.TextValue t -> pure $ StringLike t
       O.ByteStringValue b -> pure $ StringLike $ T.decodeUtf8 b
       o -> Left $ "StringLike conversion error: " ++ show o

pattern Row1P :: [O.Value] -> ResultSets
pattern Row1P r <- [RData _ [r]]

pattern Row1Col1P :: O.Value -> ResultSets
pattern Row1Col1P c <- [RData _ [[c]]]

pattern Row1Col2P :: O.Value -> O.Value -> ResultSets
pattern Row1Col2P c d <- [RData _ [[c,d]]]

pattern Upd1P :: Int -> ResultSets
pattern Upd1P rc = [RUpd rc]

pattern Ddl1P :: ResultSets
pattern Ddl1P = [RDdl]


_RDdl :: Prism' ResultSet ()
_RDdl = prism' (const RDdl) (\case
                                   RDdl -> Just ()
                                   _ -> Nothing)

_RUpd :: Prism' ResultSet Int
_RUpd = prism' RUpd (\case
                                   RUpd rc -> Just rc
                                   _ -> Nothing)

_RData :: Prism' ResultSet ([Column], [[O.Value]])
_RData = prism' (uncurry RData) (\case
                                   RData a b -> Just (a,b)
                                   _ -> Nothing)

_RDataMeta :: Traversal' ResultSet [Column]
_RDataMeta = _RData . _1

_RDataData :: Traversal' ResultSet [[O.Value]]
_RDataData = _RData . _2

oneP :: Prism' [a] a
oneP = prism' (:[]) (\case
                       [a] -> Just a
                       _ -> Nothing)

twoP :: Prism' [a] (a,a)
twoP = prism' (\(a,b) -> [a,b]) $ \case
                       [a,b] -> Just (a,b)
                       _ -> Nothing

oneMaybeP :: Prism' [a] (Maybe a)
oneMaybeP = prism' maybeToList $ \case
                       [a] -> Just (Just a)
                       [] -> Just Nothing
                       _ -> Nothing

twoMaybeP :: Prism' [a] (Maybe (a,a))
twoMaybeP = prism' (maybe [] (\(a,b) -> [a,b]))  (\case
                       [a,b] -> Just (Just (a,b))
                       [] -> Just Nothing
                       _ -> Nothing)

-- lifted version if we are already in a maybe!
-- there has to be something in lens for this: functor prism?
oneMaybeLiftedP :: Prism' (Maybe [a]) (Maybe a)
oneMaybeLiftedP = prism' (\case
                        Nothing -> Just []
                        Just a -> Just [a]
                    )
                    (\case
                       Just [a] -> Just (Just a)
                       Nothing -> Just Nothing
                       _ -> Nothing
                    )

twoMaybeLiftedP :: Prism' (Maybe [a]) (Maybe (a,a))
twoMaybeLiftedP = prism' (\case
                        Nothing -> Just []
                        Just (a,b) -> Just [a,b]
                    )
                    (\case
                       Just [a,b] -> Just (Just (a,b))
                       Nothing -> Just Nothing
                       _ -> Nothing
                    )
nOnly :: (LensFail [String] f, Applicative f) => Int -> LensLike' f [a] [a]
nOnly n = maybeFizzleWith (\xs -> let len = length xs
                                  in if len == n then Nothing
                                     else Just ["expected " ++ show n ++ " entries but found " ++ show len]
                                 )

nMaybe :: (LensFail [String] f, Applicative f) => Int -> LensLike' f [a] [a]
nMaybe n = maybeFizzleWith (\xs -> let len = length xs
                                   in if len `elem` [0,n] then Nothing
                                      else Just ["expected " ++ show n ++ " entries or none but found " ++ show len]
                                 )

nMaybeLifted :: (LensFail [String] f, Applicative f) => Int -> LensLike' f (Maybe [a]) (Maybe [a])
nMaybeLifted n = maybeFizzleWith $ \case
                                      Nothing -> Nothing
                                      Just xs -> let len = length xs
                                                 in if len `elem` [0,n] then Nothing
                                                    else Just ["expected " ++ show n ++ " entries or none but found " ++ show len]


-- nDdl is successful or fails if they are all rddl or not
nDdl :: Int -> ResultSets -> ([String], ())
nDdl n rss = rss ^&. nOnly n . traversed . vddl -- all n will be RDdl else error [returns ()]

-- gives a lists of rcs but you would still need to pattern match on the list! (sort of useful)
nUpd :: Int -> ResultSets -> ([String], [Int])
nUpd n rss = rss ^&.. nOnly n . traversed . vupd -- all n will be RUpd else error [returns a list of int return codes]

-- traverses the stuff but doesnt peel out the data so might not be so useful (ie get a list of stuff)
-- see row1col1 which actually extract stuff
{-
nMetaAndData :: Int -> ResultSets -> ([String], [[[O.Value]]])
nMetaAndData n rss = rss ^&.. nOnly n . traversed . vdata

nMeta :: Field1 [[O.Value]] [[O.Value]] b b => Int -> ResultSets -> ([String], [b])
nMeta n rss = rss ^&.. nOnly n . traversed . vdata . _1

nData :: Field2 [[O.Value]] [[O.Value]] b b => Int -> ResultSets -> ([String], [b])
nData n rss = rss ^&.. nOnly n . traversed . vdata . _2
-}
oneOnly :: (LensFail [String] f, Applicative f, Show a) => String -> LensLike' f [a] a
oneOnly msg = oneP `orFizzleWith` (\x -> ["expected one " ++ msg ++ " but found " ++ show x])

twoOnly :: (LensFail [String] f, Applicative f, Show a) => String -> LensLike' f [a] (a,a)
twoOnly msg = twoP `orFizzleWith` (\x -> ["expected two " ++ msg ++ " but found " ++ show x])

oneMaybe :: (LensFail [String] f, Applicative f, Show a) => String -> LensLike' f [a] (Maybe a)
oneMaybe msg = oneMaybeP `orFizzleWith` (\x -> ["expected one or none " ++ msg ++ " but found " ++ show x])

oneMaybeLifted :: (LensFail [String] f, Applicative f, Show a) => String -> LensLike' f (Maybe [a]) (Maybe a)
oneMaybeLifted msg = oneMaybeLiftedP `orFizzleWith` (\x -> ["expected one or none " ++ msg ++ " but found " ++ show x])

twoMaybe :: (LensFail [String] f, Applicative f, Show a) => String -> LensLike' f [a] (Maybe (a,a))
twoMaybe msg = twoMaybeP `orFizzleWith` (\x -> ["expected two or none " ++ msg ++ " but found " ++ show x])

twoMaybeLifted :: (LensFail [String] f, Applicative f, Show a) => String -> LensLike' f (Maybe [a]) (Maybe (a,a))
twoMaybeLifted msg = twoMaybeLiftedP `orFizzleWith` (\x -> ["expected two or none " ++ msg ++ " but found " ++ show x])

-- combinators to use with row1 stuff
vddl :: (LensFail [String] f, Applicative f) => LensLike' f ResultSet ()
vddl = _RDdl `orFizzleWith` (\x -> ["expected ddl but found " ++ show x])

vupd :: (LensFail [String] f, Applicative f) => LensLike' f ResultSet Int
vupd = _RUpd `orFizzleWith` (\x -> ["expected upd but found " ++ show x])

vmetaanddata :: (LensFail [String] f, Applicative f) => LensLike' f ResultSet ([O.Column], [[O.Value]])
vmetaanddata = _RData `orFizzleWith` (\x -> ["expected data but found " ++ show x])

vmeta:: (LensFail [String] f, Applicative f) => LensLike' f ResultSet [O.Column]
vmeta = (_RData . _1) `orFizzleWith` (\x -> ["expected data but found " ++ show x])

vdata :: (LensFail [String] f, Applicative f) => LensLike' f ResultSet [[O.Value]]
vdata = (_RData . _2) `orFizzleWith` (\x -> ["expected data but found " ++ show x])

-- validation prisms
ddl1 :: (Applicative f, LensFail [String] f) => LensLike' f ResultSets ()
ddl1 = oneOnly "resultset" . vddl

-- todo: easier would be to count the entries then traverse to make sure they are all of the same type
-- then we dont need most of this crap
--ddl2 :: (Applicative f, LensFail [String] f) => LensLike' f ResultSets ()
--ddl2 = twoOnly "resultset" . vddl

-- these are good cos they give us the exact type! ie unwrap stuff
upd1 :: (Applicative f, LensFail [String] f) => LensLike' f ResultSets Int
upd1 = oneOnly "resultset" . vupd

row1col1 :: (Applicative f, LensFail [String] f) => LensLike' f ResultSets O.Value
row1col1 = row1 . oneOnly "column"

-- patterns work better
row1col2 :: (Applicative f, LensFail [String] f) => LensLike' f ResultSets (O.Value, O.Value)
row1col2 = row1 . twoOnly "columns"

row1 :: (Applicative f, LensFail [String] f) => LensLike' f ResultSets [O.Value]
row1 = oneOnly "resultset" . vdata . oneOnly "row"

-- very common usecase
row1col1Maybe :: (Applicative f, LensFail [String] f) => LensLike' f ResultSets (Maybe O.Value)
row1col1Maybe = row1Maybe . oneMaybeLifted "columns (1 row + 1 column or no rows)"

row1col2Maybe :: (Applicative f, LensFail [String] f) => LensLike' f ResultSets (Maybe (O.Value, O.Value))
row1col2Maybe = row1Maybe . twoMaybeLifted "columns (1 row+ 2 columns or no rows)"

row1Maybe :: (Applicative f, LensFail [String] f) => LensLike' f ResultSets (Maybe [O.Value])
row1Maybe = oneOnly "resultset" . vdata . oneMaybe "row"


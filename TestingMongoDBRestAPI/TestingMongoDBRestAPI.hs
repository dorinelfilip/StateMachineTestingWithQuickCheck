{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Test.QuickCheck         ( Arbitrary
                               , Args
                               , Gen
                               , Property
                               , arbitrary
                               , choose
                               , frequency
                               , maxSuccess
                               , quickCheckWith
                               , shrink
                               , stdArgs
                               , vectorOf
                               )
import Test.QuickCheck.Monadic ( assert
                               , monadicIO
                               , run
                               )

import qualified Data.Set as Set
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit    ( simpleHttp )
import qualified Data.ByteString.Lazy as L
import Database.MongoDB
    ( Select(select),
      close,
      master,
      insertMany,
      delete,
      access,
      host,
      connect,
      (=:) )
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Text hiding (map)
import Data.Maybe


data RowListElement = RowListElement { id :: Value, abc :: Int } deriving Show
data RootJSONElement = RootJSONElement { offset :: Value, rows :: [RowListElement], total_rows :: Value, query :: Value, milis :: Value } deriving Show

instance FromJSON RowListElement where
    parseJSON (Object v) =
        RowListElement <$> v .: "_id" <*> v .: "abc"
    parseJSON _ = mzero

instance FromJSON RootJSONElement where
    parseJSON (Object v) =
        RootJSONElement <$> v .: "offset" <*> v .: "rows" <*> v .: "total_rows" <*> v .: "query" <*> v .: "millis"
    parseJSON _ = mzero


getIdsFromJSONString :: L.ByteString -> [Int]
getIdsFromJSONString s = result where
  decodedInput = decode s :: Maybe RootJSONElement
  xRowsList = rows $ fromJust decodedInput
  result = Prelude.map abc xRowsList


addInDatabase list = do
   pipe <- connect (host "127.0.0.1")
   e <- access pipe master "qctest" $ addQuery list
   close pipe
   --print e


clearDatabase = do
   pipe <- connect (host "127.0.0.1")
   e <- access pipe master "qctest" clearQuery
   close pipe

clearQuery = do
   clearIds

addQuery list = do
   insertIds list


clearIds = delete (select [] "randids")

myInsertAbc list = insertMany "randids" $ map (\x -> ["abc" =: x])  list

insertIds list = myInsertAbc list

checkPropAux list = do
  _ <- clearDatabase
  _ <- addInDatabase $ 1 : list
  let inputSet = Set.fromList (1 : list)
  --print inputSet
  restingJSON <- simpleHttp "http://127.0.0.1:28017/qctest/randids/"
  let restList = getIdsFromJSONString restingJSON
  let restingSet = Set.fromList  (1 : restList)
  return $ inputSet == restingSet


checkProp :: [Int] -> Property
checkProp list = monadicIO $ do
  isOk <- run $ do
    (checkPropAux list)
  assert isOk

customArgs :: Args
customArgs = ( stdArgs { maxSuccess = 1000000 } )

main :: IO ()
main = quickCheckWith customArgs checkProp
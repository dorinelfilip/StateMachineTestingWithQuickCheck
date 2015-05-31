import Control.Applicative     ( (<$>) )

import Data.List (group)
import Data.List.Split (chunksOf)
import Test.QuickCheck         ( Arbitrary
                               , Args
                               , Gen
                               , Property
                               , arbitrary
                               , choose
                               , maxSuccess
                               , quickCheckWith
                               , stdArgs
                               , vectorOf
                               , elements
                               )

data EvenInt = ZERO | TWO | FOUR | EIGHT deriving (Eq, Show)

convertEvenIntToInt :: EvenInt -> Int
convertEvenIntToInt ZERO = 0
convertEvenIntToInt TWO = 2
convertEvenIntToInt FOUR = 4
convertEvenIntToInt EIGHT = 8

newtype EvenIntList = MkEvenIntList [EvenInt] deriving (Eq, Show)

instance Arbitrary EvenInt where
    arbitrary = elements [ZERO, TWO, FOUR, EIGHT]

instance Arbitrary EvenIntList where
    arbitrary = do
        len <- choose(30, 50)
        MkEvenIntList <$> ( vectorOf len $ (arbitrary :: Gen EvenInt) )

extractFromEvenIntList :: EvenIntList -> [EvenInt]
extractFromEvenIntList (MkEvenIntList list) = list

convertEvenIntToIntList :: EvenIntList -> [Int]
convertEvenIntToIntList list = map convertEvenIntToInt $ extractFromEvenIntList $ list

nextStep :: [Int] -> [Int]
nextStep inputList =
  let
    len = length inputList
    aux_list =
      map sum
      $ concat
      $ map (chunksOf 2)
      $ group
      $ filter (/= 0) inputList
  in
    take len $ aux_list ++ (replicate len 0)


prop ::  EvenIntList -> Bool
proprietate myList =
    let
        trueList = convertEvenIntToIntList myList
        rarelen = length . (filter (==0))
    in
        rarelen trueList <= (rarelen $ nextStep trueList)

prop2 :: EvenIntList -> Bool
prop2 myList =
    let
        trueList = convertEvenIntToIntList myList
        after = nextStep trueList
    in
        all (==0) $ snd $ break (==0) $ after

customArgs :: Args
customArgs = ( stdArgs { maxSuccess = 1000000 } )


main :: IO ()
main = quickCheckWith customArgs prop2
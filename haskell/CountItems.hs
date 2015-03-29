module CountItems (
    printSorted,
    countItems
    ) where

import qualified Data.Map as M

newtype Count = C { getCount :: Int }
    deriving Eq

instance Ord Count where
    x <= y = (getCount x) >= (getCount y)

instance Show Count where
    show (C x) = show x

type Item = String

type ItemCounts = M.Map Item Count
type Counts = M.Map Count [Item]


countItem :: Item -> ItemCounts -> ItemCounts
countItem = M.alter count
    where count :: Maybe Count -> Maybe Count
          count Nothing  = Just $ C 1
          count (Just (C n)) = Just $ C (n + 1)

countItems :: [Item] -> ItemCounts
countItems = foldr countItem M.empty

reverseCounts :: ItemCounts -> Counts
reverseCounts counts = M.foldlWithKey addSwapped acc counts
    where acc = M.empty :: Counts
          addSwapped acc item count = M.alter (addCountedItem item) count acc

          addCountedItem :: Item -> Maybe [Item] -> Maybe [Item]
          addCountedItem item Nothing = Just [item]
          addCountedItem item (Just items) = Just (item:items)


showItemCount :: Count -> [Item] -> IO ()
showItemCount count items = putStrLn $ (show count) ++ ": " ++ (show items)

printSorted :: ItemCounts -> IO ()
printSorted counts = M.traverseWithKey showItemCount (reverseCounts counts) >> return ()

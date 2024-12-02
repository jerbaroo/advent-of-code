module Day1 where

-- import Control.FromSum (fromEitherM)
-- import Control.Monad.Error.Class (throwError)
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans.Except (ExceptT)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.List (foldl', sort)
import Data.Map qualified as Map
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

main :: IO ()
main = readLists >>= \case
  Left err -> putStrLn $ "Error: " <> err
  Right lists -> do
    print $ part1 lists
    print $ part2 lists

readLists :: IO (Either String ([Int], [Int]))
readLists = readFile "src/Day1.txt" <&> bimap show unzip . parseFile

parseFile :: String -> Either (ParseErrorBundle String Void) [(Int, Int)]
parseFile = M.runParser (M.many parseLine) ""

parseLine :: Parsec Void String (Int, Int)
parseLine = do
  let parseInt = (read <$> M.takeP Nothing 5) <* MC.space
  (,) <$> parseInt <*> parseInt

part1 :: ([Int], [Int]) -> Int
part1 = sum . map abs . uncurry (zipWith (-)) . bimap sort sort

part2 :: ([Int], [Int]) -> Int
part2 (l, r) = do
  let rCounts = foldl' (\m a -> Map.insertWith (\_ old -> old + 1) a 1 m) Map.empty r
  sum $ l <&> \a -> maybe 0 (* a) $ Map.lookup a rCounts

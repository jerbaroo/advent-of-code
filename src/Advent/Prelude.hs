module Advent.Prelude (module Advent.Prelude, module X) where

import Control.Applicative as X ((<|>))
import Control.Arrow as X ((&&&), (***), (>>>), first, left, second)
import Control.FromSum as X (fromEither, fromEitherM)
import Control.Monad as X ((>=>), (<=<), foldM, forM, forM_, guard, void)
import Control.Monad.Error.Class as X (throwError)
import Control.Monad.IO.Class as X (MonadIO, liftIO)
import Control.Monad.Trans.Except as X (ExceptT)
import Data.Bifunctor as X (bimap)
import Data.Either as X (fromRight)
import Data.Foldable as X (toList)
import Data.Function as X ((&))
import Data.Functor as X ((<&>), ($>))
import Data.Maybe as X (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Data.List as X (foldl', sort, sortOn)
import Data.Tuple as X (swap)
import Data.Tuple.Extra as X (both)
import Data.Void as X (Void)
import Debug.Trace as X (trace)
import Safe as X (headMay, lastMay, minimumMay, readMay, tailMay)

infixl 4 <$$>
(<$$>):: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 4 <&&>
(<&&>):: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (<$$>)

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe b x = if b then Just x else Nothing

debug :: Show a => a -> a
debug = debugPre ""

debugPre :: (Show a) => String -> a -> a
debugPre pre a = trace (pre <> show a) a

pairwise :: (a -> a -> c) -> (b -> b -> d) -> (a, b) -> (a, b) -> (c, d)
pairwise f g (a1, b1) (a2, b2) = (a1 `f` a2, b1 `g` b2)

times :: Int -> (a -> a) -> a -> a
times 0 _ a = a
times n f a = times (n - 1) f $ f a

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a, b, c, d) = f a b c d

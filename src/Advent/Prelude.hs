module Advent.Prelude (module Advent.Prelude, module X) where

import Control.Applicative as X ((<|>))
import Control.Arrow as X ((&&&), (***), (>>>), first, left, second)
import Control.FromSum as X (fromEitherM)
import Control.Monad as X (foldM, forM, guard)
import Control.Monad.Error.Class as X (throwError)
import Control.Monad.IO.Class as X (liftIO)
import Control.Monad.Trans.Except as X (ExceptT)
import Data.Bifunctor as X (bimap)
import Data.Function as X ((&))
import Data.Functor as X ((<&>), ($>))
import Data.Maybe as X (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Data.List as X (foldl', sort)
import Data.Tuple as X (swap)
import Data.Void as X (Void)
import Debug.Trace as X (trace)
import Safe as X (headMay, readMay)

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe b x = if b then Just x else Nothing

debug :: Show a => a -> a
debug a = trace (show a) a

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a, b, c, d) = f a b c d

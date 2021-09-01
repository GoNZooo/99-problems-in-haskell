module ConsUncons where

import Core
import Maybe

class Cons m where
  cons :: a -> m a -> m a

class Uncons m where
  uncons :: m a -> Maybe (Tuple a (m a))

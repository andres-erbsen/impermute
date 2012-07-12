module Control.Loop (
    while
  , times
  , pass
  , andif
) where

import Control.Monad

while :: Monad m => m Bool -> m () -> m ()
while test body = do
  val <- test
  if val then body >> while test body
         else return ()

infixr 0 `times`
times :: (Enum a, Num a, Monad m) => a -> m () -> m ()
n `times` action = forM_ [1..n] $ \i -> action

pass :: Monad m => m ()
pass = return ()

infixr 0 `andif`
first `andif` second = do
  b <- first
  when b second

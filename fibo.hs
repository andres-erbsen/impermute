import Control.Monad.ST
import Control.Mutable
import Control.Loop

fib :: (Enum a, Num a) => a -> Integer
fib n = runST $ do
  a <- ref 1       -- initialize
  b <- ref 0 
  n `times` do
    t <- a #+# b   -- like +, but inouts variables not values
    a #=# b        -- assign mutable to mutable
    b #= t         -- assign pure to mutable
  val b            -- read the value (like return in imperative languages)

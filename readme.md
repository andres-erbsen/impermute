# Imperative programming in Haskell!
In some cases there just does not exist a beautiful functional representation of the algorithm at hand. For these cases, this package provides useable syntax for imperative computations. At the moment the goal is to support variables, loops and conditionals in IO, ST and STM monads.

See fibo.hs for example code. Note that it is (arguably, of course) way cleaner than http://www.haskell.org/haskellwiki/Monad/ST and https://github.com/mmirman/ImperativeHaskell/blob/master/Main.hs

The code is based on seemingly abandoned ArrayRef package in Hackage.

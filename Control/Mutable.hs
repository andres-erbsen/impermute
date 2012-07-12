{-# LANGUAGE FunctionalDependencies, FlexibleInstances, MultiParamTypeClasses,
 BangPatterns #-}

module Control.Mutable (
  -- * Monad-independent interface for references
    Ref
  , newRef
  , readRef
  , writeRef
  -- * Monad-independent interface for mutable variables
  , Mutable
  , readVar
  , modifyVar
  , modifyVarM
  -- * Operations
  , writeVar
  , writeVarM
  -- * Syntax sugar
  , ref
  , val
  , (#=)
  , (#=#)
  , (#<-)
  , (#)
  , (+=)
  , (-=)
  , (.=)
  , (#+#)
  , (#-#)
  , (#*#)
  , (#/#)
  ) where
{- |
   Module     : Data.Mutable
   License    : GPL3

   Maintainer : Andres Erbsen <andres.erbsen@gmail.com>
   Stability  : experimental

Universal interface and syntax sugar (+=, #=, ...)for reading and writing
mutable values. Based on code from ArrayRef package, but with a single goal.
-}

import Data.IORef
import Data.STRef
import Control.Monad.ST (ST)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar

-- | This class allows to create new boxed reference in monad-independent way
--     (suitable for writing code that wil work in IO, ST and other monads)
class (Monad m) => Ref m r | m->r, r->m where
    -- | Create a new 'Ref' with given initial value
    newRef :: a -> m (r a)
    -- | Read the value of an 'Ref'
    readRef   :: r a -> m a
    -- | Write new value into an 'Ref'
    writeRef  :: r a -> a -> m ()

instance Ref IO IORef where
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef

instance Ref (ST s) (STRef s) where
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef

instance Ref STM TVar where
    newRef = newTVar
    readRef = readTVar
    writeRef = writeTVar

-- | This class allows monad and type independent assignments to mutable variables
class (Monad m) => Mutable m r a | r->a where
    -- | Read the value of an 'Mutable'
    readVar  :: r -> m a
    -- | Write new value into an 'Mutable'
    writeVar :: r -> a -> m ()

instance Ref m r => Mutable m (r a) a where
    readVar  = readRef
    writeVar = writeRef

-- | Write new monadic value into an 'Mutable'
{-# INLINE writeVarM #-}
writeVarM :: Mutable m r a => r -> m a -> m ()
writeVarM var = (writeVar var =<<)

-- | Modify the contents of an 'Mutable' by applying pure function to it
{-# INLINE modifyVar #-}
modifyVar :: (Mutable m r b) => r -> (b -> b) -> m ()
modifyVar  var f = writeVar var . f =<< readVar var

-- | Modify the contents of an 'Mutable' by applying monadic computation to it
{-# INLINE modifyVarM #-}
modifyVarM :: (Mutable m r a) => r -> (a -> m a) -> m ()
modifyVarM var f = writeVar var =<< f =<< readVar var


-- Syntax sugar for using mutable variables

-- |create new boxed reference
ref :: (Ref m r) => a -> m (r a)
ref = newRef
 
-- | read current value of mutable
val :: (Mutable m r a) => r -> m a
val = readVar

infixl 0 #=, #<-, #, #+#, #-#, #*#, #/#, +=, -=, .=

(#=) :: (Mutable m r b) => r -> b -> m ()
(#=) = writeVar

(#=#) :: (Mutable m r b) => r -> r -> m ()
var1 #=# var2 = writeVar var1 =<< readVar var2

(#<-) :: (Mutable m r b) => r -> m b -> m ()
(#<-) = writeVarM

-- | apply pure function to the value of mutable
(.=) :: (Mutable m r b) => r -> (b -> b) -> m ()
var .= f  = modifyVar  var (\old -> f old)

-- | increase value of mutable
{-# INLINE (+=) #-}
(+=) :: (Mutable m r b, Num b) => r -> b -> m ()
(+=) var !x  = var .= (+x)

-- | decrease value of mutable
{-# INLINE (-=) #-}
(-=) :: (Mutable m r b, Num b) => r -> b -> m ()
(-=) var !x  = var .= (\v ->  v-x) 

-- | Lift a function to use mutable value as input
liftV :: (Mutable m r a) => (a -> b) -> r -> m b
liftV f var = do                               
  x <- readVar var
  return $ f x

liftV2 :: (Mutable m r1 a, Mutable m r2 b) => (a -> b -> c) -> r1 -> r2 -> m c
liftV2 f var1 var2 = do
  x <- val var1
  y <- val var2
  return $ f x y

-- | get result of pure function of value of the mutable
(#) :: (Mutable m r a) => (a -> b) -> r -> m b
(#) = liftV

(#+#) :: (Mutable m r b, Num b) => r -> r -> m b
(#+#) = liftV2 (+)

(#-#) :: (Mutable m r b, Num b) => r -> r -> m b
(#-#) = liftV2 (-)

(#*#) :: (Mutable m r b, Num b) => r -> r -> m b
(#*#) = liftV2 (*)

(#/#) :: (Mutable m r b, Fractional b) => r -> r -> m b
(#/#) = liftV2 (/)


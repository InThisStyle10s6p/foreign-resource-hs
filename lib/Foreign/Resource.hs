{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Foreign.Resource
  ( module X
  , ForeignResource(..)
  , resource'
  , allocR
  , allocR'
  , allocLongR
  , allocLongR'
  , withR
  , withR'
  , ForeignRead(..)
  , readR
  , readR'
  , ForeignWrite(..)
  , writeR
  , writeR'
  , mkWritePassthrough
  , (.$=)
  , ($=)
  , (<$=)
  , (~&)
  , ForeignUpdate(..)
  , updateR
  , updateR'
  , (.$=%)
  , ($=%)
  , (<$=%)
  , ForeignName(..)
  , genName
  , genName'
  , genNames
  , genNames'
  , isName
  , deleteName
  , deleteNames
  )
where

import Data.Acquire as X (Acquire, mkAcquire, mkAcquireType, ReleaseType(..))
import Data.Acquire
import Control.Monad.IO.Class as X
import Control.Monad.Trans.Resource as X
import Data.Functor (void)
import Data.Foldable

class ForeignName r a | r -> a where
  genName_    :: a -> IO r
  genNames_    :: Int -> a -> IO [r]
  isName_     :: r -> IO Bool
  deleteName_ :: r -> IO ()
  deleteNames_ :: [r] -> IO ()

  genName_ = fmap head . genNames 1
  genNames_ n = traverse genName_ . replicate n
  deleteName_ = deleteNames_ . (:[])
  deleteNames_ = traverse_ deleteName_

genName :: (MonadIO m, ForeignName r a) => a -> m r
genName = liftIO . genName_
genNames :: (MonadIO m, ForeignName r a) => Int -> a -> m [r]
genNames n = liftIO . genNames_ n
isName :: (MonadIO m, ForeignName r a) => r -> m Bool
isName = liftIO . isName_
deleteName :: (MonadIO m, ForeignName r a) => r -> m ()
deleteName = liftIO . deleteName_
deleteNames :: (MonadIO m, ForeignName r a) => [r] -> m ()
deleteNames = liftIO . deleteNames_

genName' :: (MonadIO m, ForeignName r ()) => m r
genName' = liftIO . genName_ $ ()
genNames' :: (MonadIO m, ForeignName r ()) => Int -> m [r]
genNames' = liftIO . flip genNames_ ()

class ForeignResource s a where
  resource :: a -> Acquire s

resource' :: ForeignResource s () => Acquire s
resource' = resource ()

allocR :: (MonadResource m, ForeignResource s a) => a -> m (ReleaseKey, s)
allocR = allocateAcquire . resource

allocR' :: (MonadResource m, ForeignResource s ()) => m (ReleaseKey, s)
allocR' = allocR ()

allocLongR :: (MonadResource m, ForeignResource s a) => a -> m s
allocLongR = fmap snd . allocR

allocLongR' :: (MonadResource m, ForeignResource s ()) => m s
allocLongR' = allocLongR ()

withR :: (MonadBaseControl IO m, ForeignResource s a) => a -> (s -> m b) -> m b
withR a = with (resource a)

withR' :: (MonadBaseControl IO m, ForeignResource s ()) => (s -> m b) -> m b
withR' = with resource'

class ForeignRead t s r | t s -> r where
  readR_ :: t -> s -> IO r

readR :: (MonadIO m, ForeignRead t s r) => t -> s -> m r
readR t = liftIO . readR_ t

readR' :: (MonadIO m, ForeignRead () s r) => s -> m r
readR' = readR ()

class ForeignWrite t s w | t s -> w where
  writeR_ :: t -> s -> w -> IO s

writeR :: (MonadIO m, ForeignWrite t s w) => t -> s -> w -> m s
writeR t s = liftIO . writeR_ t s

writeR' :: (MonadIO m, ForeignWrite () s w) => s -> w -> m s
writeR' = writeR ()

mkWritePassthrough :: Monad m => (s -> w -> m ()) -> s -> (w -> m s)
mkWritePassthrough f s w = f s w >> return s

infix 4 .$=
(.$=) :: (MonadIO m, ForeignWrite t s w) => t -> w -> s -> m s
t .$= w = \s -> writeR t s w

infix 4 $=
($=) :: (MonadIO m, ForeignWrite () s w) => s -> w -> m ()
s $= w = void (writeR () s w)

infix 4 <$=
(<$=) :: (MonadIO m, ForeignWrite () s w) => s -> w -> m s
(<$=) = writeR'

infixl 1 ~&
(~&) :: Functor f => s -> (s -> f s) -> f ()
s ~& f = void (f s)

class (ForeignRead t s a, ForeignWrite t s a) => ForeignUpdate t s a where
  updateR_ :: t -> (a -> IO a) -> s -> IO s

  updateR_ t f s = readR_ t s >>= f >>= writeR_ t s


updateR :: (MonadIO m, ForeignUpdate t s a) => t -> (a -> IO a) -> s -> m s
updateR t f = liftIO . updateR_ t f

updateR' :: (MonadIO m, ForeignUpdate () s a) => (a -> IO a) -> s -> m s
updateR' = updateR ()

infix 4 .$=%
(.$=%) :: (MonadIO m, ForeignUpdate t s a) => t -> (a -> IO a) -> s -> m s
(.$=%) = updateR

infix 4 <$=%
(<$=%) :: (MonadIO m, ForeignUpdate () s a) => s -> (a -> IO a) -> m s
(<$=%) = flip updateR'

infix 4 $=%
($=%) :: (MonadIO m, ForeignUpdate () s a) => s -> (a -> IO a) -> m ()
($=%) s = void . flip updateR' s

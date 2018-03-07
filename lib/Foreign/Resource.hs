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

class ForeignRead s t r where
  readR_ :: s -> t -> IO r

readR :: (MonadIO m, ForeignRead s t r) => s -> t -> m r
readR s = liftIO . readR_ s

readR' :: (MonadIO m, ForeignRead s () r) => s -> m r
readR' s = readR s ()

class ForeignWrite s t w where
  writeR_ :: s -> t -> w -> IO s

writeR :: (MonadIO m, ForeignWrite s t w) => s -> t -> w -> m s
writeR s t = liftIO . writeR_ s t

writeR' :: (MonadIO m, ForeignWrite s () w) => s -> w -> m s
writeR' s = writeR s ()

mkWritePassthrough :: Monad m => s -> (s -> w -> m ()) -> (w -> m s)
mkWritePassthrough s f w = f s w >> return s

infix 4 .$=
(.$=) :: (MonadIO m, ForeignWrite s t w) => t -> w -> s -> m s
t .$= w = \s -> writeR s t w

infix 4 $=
($=) :: (MonadIO m, ForeignWrite s () w) => s -> w -> m ()
s $= w = void (writeR s () w)

infix 4 <$=
(<$=) :: (MonadIO m, ForeignWrite s () w) => s -> w -> m s
s <$= w = writeR s () w

infixl 1 ~&
(~&) :: Functor f => s -> (s -> f s) -> f ()
s ~& f = void (f s)

class (ForeignRead s t a, ForeignWrite s t a) => ForeignUpdate s t a where
  updateR_ :: t -> (a -> IO a) -> s -> IO s

  updateR_ t f s = readR_ s t >>= f >>= writeR_ s t


updateR :: (MonadIO m, ForeignUpdate s t a) => t -> (a -> IO a) -> s -> m s
updateR t f = liftIO . updateR_ t f

updateR' :: (MonadIO m, ForeignUpdate s () a) => (a -> IO a) -> s -> m s
updateR' = updateR ()

infix 4 .$=%
(.$=%) :: (MonadIO m, ForeignUpdate s t a) => t -> (a -> IO a) -> s -> m s
(.$=%) = updateR

infix 4 <$=%
(<$=%) :: (MonadIO m, ForeignUpdate s () a) => s -> (a -> IO a) -> m s
s <$=% f = updateR () f s

infix 4 $=%
($=%) :: (MonadIO m, ForeignUpdate s () a) => s -> (a -> IO a) -> m ()
s $=% f = void (updateR () f s)

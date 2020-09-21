{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, DeriveTraversable, RankNTypes #-}

module Windows.Com.Monad where

import Windows.Types (HRESULT)
import Windows.Const.HResult (isSucceeded)
import Control.Exception (Exception, throwIO)
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT, (>=>))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Cont (MonadCont)
import Control.Monad.Zip (MonadZip)
import Control.Monad.IO.Class (MonadIO)
import Data.Traversable (Traversable(..))
import Data.Foldable (Foldable)
import Numeric (showHex)

newtype ComError = ComError HRESULT
instance Show ComError where
  show (ComError 0x80070057) = "ComError: Invalid Arguments"
  show (ComError 0x80004002) = "ComError: Interface is not supported"
  show (ComError hr) = "ComError: " ++ showHex hr ""
instance Exception ComError

newtype ComT m a = ComT { unComT :: ExceptT ComError m a } deriving (Functor, Applicative, Monad, MonadTrans, MonadFix, MonadIO, MonadCont, MonadZip, Traversable, Foldable)
comT :: m (Either ComError a) -> ComT m a
comT = ComT . ExceptT
runComT :: ComT m a -> m (Either ComError a)
runComT = runExceptT . unComT
handleHRESULT :: Monad m => HRESULT -> ComT m ()
handleHRESULT hr = if isSucceeded hr then ComT $ pure () else throwError $ ComError hr
instance Monad m => MonadError ComError (ComT m) where
  throwError = ComT . throwError
  catchError a h = ComT $ catchError (unComT a) (unComT . h)
instance MonadReader r m => MonadReader r (ComT m) where
  ask = ComT $ ask
  local f a = ComT $ local f $ unComT a
instance MonadWriter w m => MonadWriter w (ComT m) where
  writer = ComT . writer
  listen = ComT . listen . unComT
  pass = ComT . pass . unComT
instance MonadState s m => MonadState s (ComT m) where
  get = ComT $ get
  put = ComT . put

{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans
import Control.Monad.Trans.Reader (Reader, runReader, ask)
import Control.Monad.Identity
import Control.Monad (liftM)

-- Exercises: EitherT

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))
  (EitherT fab) <*> (EitherT mea) = EitherT $ (<*>) <$> fab <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f = EitherT $ do
    v <- mea
    case v of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

-- transformer version of swapEither.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT ac bc (EitherT mab) = do
  v <- mab
  case v of
    Left a -> ac a
    Right b -> bc b

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT (pure (pure x))
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

-- Exercises: StateT
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s -> fmap go (sma s)
    where
      go (a, s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (StateT fab) <*> (StateT sma) = StateT $ \s -> do
    (ab, s') <- fab s
    (a, s'') <- sma s'
    return (ab a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

-- Exercise: Wrap It Up
embedded ::
  MaybeT
    ( ExceptT
        String
        (ReaderT () IO)
    )
    Int
embedded = MaybeT (ExceptT (ReaderT (return <$> const (Right (Just 1)))))


instance MonadTrans (ReaderT r) where
  lift :: m a -> ReaderT r m a
  lift = ReaderT . const

-- Exercises: Lift More

instance MonadTrans (EitherT e) where
  lift ma = EitherT $ liftM Right ma

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> (,) <$> ma <*> return s


-- Example MonadIO instances

data MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just

  (MaybeT mf) <*> (MaybeT ma) =
    MaybeT $
      (fmap (<*>) mf) <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure

  (MaybeT ma) >>= k = MaybeT $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      Just a' -> runMaybeT $ k a'

instance MonadTrans MaybeT where
  lift m = MaybeT $ fmap Just m

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO


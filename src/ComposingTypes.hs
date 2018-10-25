{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module ComposingTypes where

import Control.Applicative(liftA2)
import Control.Monad((=<<))

newtype Identity a = Identity { runIdentity :: a }
newtype Compose f g a = Compose { runCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f fga = Compose $ (fmap . fmap) f (runCompose fga)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose $ (pure . pure) x
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgab) <*> (Compose fga) = Compose $ (fmap (<*>) fgab) <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  sequenceA :: Applicative h => Compose f g (h a) -> h (Compose f g a)
  sequenceA (Compose fgha) = fmap Compose $ traverse sequenceA fgha

-- Monad transformers, yay!
newtype IdentityT f a = IdentityT { runIdentityT :: f a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT $ fmap f ma

instance Applicative m => Applicative (IdentityT m) where
  pure x = (IdentityT . pure) x
  (IdentityT mf) <*> (IdentityT ma) = IdentityT $ mf <*> ma

instance Monad m => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= f'
    where f' = runIdentityT . f

-- MaybeT transformer --

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT x) = MaybeT $ (fmap . fmap) f x

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (<*>) :: MaybeT m (a ->b) -> MaybeT m a -> MaybeT m b
  (MaybeT f) <*> (MaybeT a) = MaybeT $ f' <*> a
    where f' = fmap (<*>) f

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . (pure . pure)
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mMaybeA) >>= f =
    MaybeT $ mMaybeA >>= f'
    where f' Nothing = pure Nothing
          f' (Just a) = (runMaybeT . f) a

-- EitherT trasnformer --

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT x) = EitherT $ (fmap . fmap) f x

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT f) <*> (EitherT a) = EitherT $ (fmap (<*>) f) <*> a

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = EitherT $ do
    a <- ma
    case a of
      (Left e)  -> return $ Left e
      (Right v) -> runEitherT . f $ v

swapEitherT :: (Monad m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma)= EitherT $ do
  a <- ma
  case a of
    Left e -> return $ Right e
    Right v -> return $ Left v

swapEitherT' :: (Monad m) => EitherT e m a -> EitherT a m e
swapEitherT' (EitherT ma)= EitherT $ (\case (Left e) -> Right e
                                            (Right v) -> Left v) <$> ma

-- ReaderT --

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT g) = ReaderT $ (fmap f) . g

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT mf) <*> (ReaderT ma) = ReaderT $ (fmap (<*>) mf) <*> ma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) :: forall r m a b . Monad m => ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ do
    ma <- rma
    f' <- rf
    return $ ma >>= f'
    where rf :: r -> a -> m b
          rf = flip (runReaderT . f)

-- StateT --

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }


instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \s ->
    (\(a, s1) -> (f a, s1)) <$> g s

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (<*>) :: Monad m => StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smf) <*> (StateT sma) = StateT $ \s -> do
    (f, s')  <- smf s
    (a, s'') <- sma s'
    return (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= asmb = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (asmb a) s'

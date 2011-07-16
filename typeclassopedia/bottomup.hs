import Prelude (zipWith, map, concatMap, concat, (++), Bool(True, False))

-- utility
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)
infixr 9 .

($) :: (a -> b) -> a -> b
f $ x = f x
infixr 0 $

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

const :: a -> b -> a
const x _ = x


-- classes {{{1
-- Monoid {{{2
class Monoid m where
	mempty :: m
	mappend :: m -> m -> m
-- Functor {{{2
class Functor f where
	fmap :: (a -> b) -> f a -> f b

-- Pointed {{{2
class (Functor f) => Pointed f where
	pure :: a -> f a

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

(<$) :: (Functor f) => a -> f b -> f a
(<$) = fmap . const
infixl 4 <$

-- Applicative {{{2
class (Pointed f) => Applicative f where
	(<*>) :: f (a -> b) -> f a -> f b

(*>) :: (Applicative f) => f a -> f b -> f b
l *> r = (\x y -> y) <$> l <*> r

(<*) :: (Applicative f) => f a -> f b -> f a
l <* r = (\x y -> x) <$> l <*> r

(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)

sequence :: (Applicative f) => [f a] -> f [a]
sequence [] = pure []
sequence (x:xs) = (++) <$> (fmap pure x) <*> (sequence xs)

-- Alternative {{{2
class Monoid f => Alternative f

empty :: Alternative f => f
empty = mempty

(<|>) :: Alternative f => f -> f -> f
(<|>) = mappend

-- Monad {{{2
class (Applicative m) => Monad m where
	join :: m (m a) -> m a

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
x >>= f = join $ fmap f x

return :: (Monad m) => a -> m a
return = pure

(>>) :: (Monad m) => m a -> m b -> m b
(>>) = (*>)

liftM :: (Monad m) => (a -> b) -> (m a -> m b)
liftM = fmap

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = (<*>)

when :: (Monad m) => Bool -> m () -> m ()
when True x = x
when False _ = return ()

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence $ map f xs

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g x = f x >>= g

-- MonadPlus {{{2
class (Monoid m) => MonadPlus m

mzero :: MonadPlus m => m
mzero = mempty

mplus :: MonadPlus m => m -> m -> m
mplus = mappend

-- MonadTrans {{{2
class MonadTrans t where
	lift :: Monad m => m a -> t m a

-- instances {{{1
-- List {{{2
instance Functor [] where
	fmap = map

instance Pointed [] where
	pure x = [x]

instance Applicative [] where
   fs <*> xs = concatMap (\f -> map f xs) fs

instance Monad [] where
	join = concat


-- ZipList {{{2
newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
	fmap f (ZipList xs) = ZipList (fmap f xs)

instance Pointed ZipList where
	pure x = ZipList $ pure x

instance Applicative ZipList where
	(ZipList fs) <*> (ZipList xs) = ZipList $ zipWith ($) fs xs

instance Monad ZipList where
	join (ZipList xs) = ZipList $ concatMap getZipList xs

-- Maybe {{{2
data Maybe a = Nothing | Just a
instance Functor Maybe where
	fmap f Nothing = Nothing
	fmap f (Just x) = Just (f x)

instance Pointed Maybe where
	pure x = Just x

instance Applicative Maybe where
	Nothing <*> _ = Nothing
	_ <*> Nothing = Nothing
	(Just f) <*> (Just x) = Just $ f x

instance Monad Maybe where
	join Nothing = Nothing
	join (Just Nothing) = Nothing
	join (Just (Just x)) = Just x

-- Error {{{2
data Either a b = Left a | Right b
instance Functor (Either e) where
	fmap f (Right x) = Right (f x)
	fmap _ (Left x) = Left x

instance Pointed (Either e) where
	pure x = Right x

instance Applicative (Either e) where
	(Left e) <*> _ = Left e
	_ <*> (Left e) = Left e
	(Right f) <*> (Right x) = Right $ f x

instance Monad (Either e) where
	join (Left e) = Left e
	join (Right (Left e)) = Left e
	join (Right (Right e)) = Right e

-- Reader {{{2
instance Functor ((->)e) where
  fmap f g = \x -> f (g x)

instance Pointed ((->)e) where
	pure x = \_ -> x 

instance Applicative ((->)e) where
	f <*> x = \y -> (f y) (x y)

instance Monad ((->)e) where
	join f = \x -> (f x) x

-- Writer {{{2
instance Functor ((,)e) where
	fmap f (x, y) = (x, f y)

instance (Monoid e) => Pointed ((,)e) where
	pure x = (mempty, x)

instance (Monoid e) => Applicative ((,)e) where
	(e,f) <*> (e',x) = (e `mappend` e', f x)

instance (Monoid e) => Monad ((,)e) where
	join (e, (e', x)) = (e `mappend` e',x)

-- State {{{2
data State s a = State { runState :: s -> (a, s) }
instance Functor (State s) where
	fmap f (State x) = State $ \s -> let (x',s') = x s in (f x', s')

instance Pointed (State s) where
	pure x = State $ \s -> (x, s)

instance Applicative (State s) where
	(State f) <*> (State x) = State $ \s -> let 
		(f', s') = f s
		(x', s'') = x s'
		in (f' x', s'')

instance Monad (State s) where
	join (State f) = State $ \s -> let
		(State f', s') = f s
		in f' s'

{-# Language TypeFamilies #-}
module Temporal.Class(
   DurOf, Duration(..), Melody(..), Harmony(..), Compose(..),
   loopBy, melMap, harMap,
   Delay(..), (+|),
   Rest(..), Stretch(..), (*|), Limit(..),
   Loop(..)
) where

-- | Calculates duration.
class Duration a where
   dur :: a -> DurOf a

-- | Duration for the given type.
type family DurOf a :: *

class Melody a where
   -- | Sequent composition for a list of values (melody).
   mel :: [a] -> a
   -- | Sequent composition. Play first track then second.
   (+:+) :: a -> a -> a

   a +:+ b = mel [a, b]
   mel = foldl1 (+:+)

class Harmony a where
   -- | Parallel composition for a list of values (harmony).
   har :: [a] -> a

   -- | Parallel composition. Play two tracks simultaneously.
   (=:=) :: a -> a -> a

   a =:= b = har [a, b]
   har = foldl1 (=:=)


class (Melody a, Harmony a) => Compose a where

-- | Repeats the given audio segment several times.
loopBy :: Melody a => Int -> a -> a
loopBy n = mel . replicate n

class Delay a where
   -- | Delays the sound source by the given duration.
   del :: DurOf a -> a -> a

class Stretch a where
   -- | Delays the sound source by the given duration factor.
   str :: DurOf a -> a -> a

-- | Infix 'del' function.
(+|) :: Delay a => DurOf a -> a -> a
(+|) = del

-- | Infix 'str' function.
(*|) :: Stretch a => DurOf a -> a -> a
(*|) = str

class Rest a where
   rest :: DurOf a -> a

class Limit a where
   -- | Limits the duration of the sound source.
   lim :: DurOf a -> a -> a

class Loop a where
   -- | Loops over the sound
   loop :: a -> a

-- | Transforms a sequence and then applies a mel.
melMap :: (Melody b) => (a -> b) -> [a] -> b
melMap f xs = mel $ fmap f xs

-- | Transforms a sequence and then applies a har.
harMap :: (Harmony b) => (a -> b) -> [a] -> b
harMap f xs = har $ fmap f xs

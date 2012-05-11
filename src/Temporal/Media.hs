{-# Language 
        TypeFamilies, 
        FlexibleContexts, 
        BangPatterns #-}

-- | An embedded domain-specific language (EDSL) for creating 
-- lists of constant time events related in time.
module Temporal.Media(
    -- * Introduction

    -- | "Temporal.Media" is an embedded domain-specific 
    -- language (EDSL) for creating lists of constant time 
    -- events related in time. Constant time event is value
    -- that starts at some fixed time and lasts for some 
    -- fixed time. Library provides functions to build lists
    -- of such events with time-relations like sequent,  
    -- parallel or delayed. 
    --
    -- Core type of library is 'Track'. It provides interface
    -- to compose list of events. 
    --
    -- Examples of usage can be found in package 'temporal-music-notation' [1].
    -- Score module is based on this library.
    --
    -- \[1\] <http://hackage.haskell.org/package/temporal-music-notation>
    
    -- * Classes
    Time(..), Temporal(..), Media(..), Clippable(..),
    (+|), (*|), (=:=), (+:+), (=:/),
    line, chord, chordT, loop, rest,
     
    -- * Monoid synonyms
    --
    -- | This package heavily relies on 'Monoid's, so there are shorcuts
    -- for 'Monoid' methods.
    nil,
    module Data.Monoid,
    -- * Types
    Event(..),
    Track,
    filterE, sustain, sustainT, temp,   
    -- * Mappings
    mapE, tmap, tmapRel,
    -- * Rendering
    render, alignByZero
)
    where

import Data.Monoid
import Data.Foldable(Foldable(foldMap))

import Control.Applicative
import Control.Monad

import Data.Ratio

-- TODO : optimise loops
-- reflect ??? 

-- | Class 'Time' is an synonym for intersection of
-- 'Num' and 'Ord' clases. It contains no methods.
class (Num t, Ord t) => Time t 

instance Time Int
instance Time Integer
instance Time Float
instance Time Double
instance Integral a => Time (Ratio a)


-- Monoid shortcuts

-- | Synonym for method 'mempty'.
nil :: Monoid a => a 
nil = mempty

-- Classes

-- | Class for temporal values. Temporal values happens
-- in some time and lasts for some time. They can be 
-- delayed in time domain (start time is shifted), and 
-- stretched (duration is multiplied by some factor).
--
-- Properties:
--
-- > dur (delay t a) = t + dur a
-- > dur (stretch t a) = t * dur a
--
-- > delay a . delay b = delay (a + b)
-- > delay a . delay b = delay b . delay a
-- > stretch a . stretch b = stretch (a * b)
-- > stretch a . stretch b = stretch b . stretch a
class Temporal t where
    type Dur t :: *
    -- | Duration of the value
    dur     :: t -> Dur t           
    -- | Delaying values in time domain
    delay   :: Dur t -> t -> t  
    -- | Stretching values in time domain
    stretch :: Dur t -> t -> t  

-- | Infix 'delay' function.
(+|) :: Temporal t => Dur t -> t -> t
(+|) = delay

-- | Infix 'stretch' function.
(*|) :: Temporal t => Dur t -> t -> t
(*|) = stretch

-- | 'Clippable' values can be divided in parts. 
-- 
-- properties:
--
-- > dur (clip a b val) = b - a
class Temporal t => Clippable t where
    -- | @clip a b val@ is a part of value that starts on @a@ 
    -- and ends on @b@.
    clip :: Dur t -> Dur t -> t -> t

  
-- | 'Media' is something that can be transformed in time domain
-- and can be played in parallel ('Monoid' method '<>'). 
--
-- properties:
--
-- > dur nil = 0
-- > dur (a <> b) = dur a `max` dur b
class (Monoid t, Temporal t) => Media t

-- | Parallel composition. Play two medias
-- simultaneously.
(=:=) :: Media t => t -> t -> t
a =:= b = a <> b 

-- | Sequent composition. Play first media then
-- second.
(+:+) :: Media t => t -> t -> t
a +:+ b = a <> delay (dur a) b

-- | Turncating parallel composition. Total duration
-- equals to minimum of the two medias. All events
-- that goes beyond the lmimt are dropped.
(=:/) :: (Time (Dur t), Media t, Clippable t) => t -> t -> t
a =:/ b = clip 0 (dur a `min` dur b) $ a <> b

-- | Parallel composition on list of medias.
chord :: Media t => [t] -> t
chord = mconcat


-- | Sequent composition on list of medias.
line :: Media t => [t] -> t
line = foldr (+:+) nil

-- | Turncating parallel composition on list of medias.
chordT :: (Time (Dur t), Clippable t, Media t) => [t] -> t
chordT xs = clip 0 (minimum $ dur <$> xs) $ chord xs

-- | Analog of 'replicate' function for medias. Replicated
-- medias are played sequentially.
loop :: Media t => Int -> t -> t
loop n = line . replicate n

-- | Reversing the media
reflect :: (Num (Dur t), Temporal t) => t -> t
reflect a = delay (dur a) $ stretch (-1) a

-- | Empty media that lasts for some time.
rest :: Media t => Dur t -> t
rest = flip delay nil

----------------------------------------------
-- Track

-- | 'Track' is a set of 'Event' s. There is total duration
-- of the track, but Events can go beyond the scope of total duration
-- (as a result of 'mapE' function). Total duration is used in sequent 
-- composition of tracks. 
data Track t a = Track t (TList t a)
    deriving (Show, Eq)

instance Functor (Track t) where
    fmap f (Track d es) = Track d $ fmap f es

instance (Time t) => Monoid (Track t a) where
    mempty = Track 0 mempty
    mappend (Track d es) (Track d' es') = 
        Track (max d d') $ mappend es es'

instance Time t => Temporal (Track t a) where
    type Dur (Track t a) = t
    
    dur (Track d _) = d
    stretch k (Track d es) = Track (k*d) $ stretch k es
    delay   k (Track d es) = Track (k+d) $ delay   k es

instance Time t => Media (Track t a)

instance Foldable (Track t) where
    foldMap f (Track _ x) = foldMap f x        
   
instance Time t => Clippable (Track t a) where
    clip t0 t1 = clipDur . delay (-t0) . filterE (within t0 t1)
        where clipDur (Track _ a) = Track (t1 - t0) a


temp :: (Num t, Ord t) => a -> Track t a
temp = Track 1 . Single


-- | Get all events on recordered on the track. 
render :: Num t => Track t a -> [Event t a]
render (Track d es) = renderTList es


-----------------------------------------------
-- Event

-- | Constant time events. Value @a@ starts at some time 
-- and lasts for some time.
data Event t a = Event {
        eventStart      :: t,
        eventDur        :: t,
        eventContent    :: a 
    } deriving (Show, Eq)

instance Functor (Event t) where
    fmap f e = e{ eventContent = f (eventContent e) }

instance Num t => Temporal (Event t a) where
    type Dur (Event t a) = t
    dur = eventDur
    delay   d e = e{ eventStart = eventStart e + d }
    stretch d e = e{ eventStart = eventStart e * d,
                     eventDur   = eventDur   e * d }

within :: Time t => t -> t -> Event t a -> Bool
within t0 t1 (Event s d _) = within' t0 t1 s && within' t0 t1 (s + d)
    where within' a b x = x <= a && x >= b

-- | General mapping. Mapps not only values but events. 
mapE :: Fractional t 
    => (Event t a -> Event t b) -> Track t a -> Track t b 
mapE f (Track d es) = Track d $ eventMapTList f es

-- | Filter track. 
filterE :: (Time t) => (Event t a -> Bool) -> Track t a -> Track t a
filterE p (Track d es) = Track d $ filterTList p es

-- | Mapps values and time stamps.
tmap :: Fractional t 
    => (Event t a -> b) -> Track t a -> Track t b
tmap f = mapE $ \e@(Event s d _) -> Event s d (f e)

tmapRel :: (Fractional t, Time t) 
    => (Event t a -> b) -> Track t a -> Track t b
tmapRel f x = tmap (f . stretch (1 / dur x)) x


-- | After this transformation events last longer
-- by some constant amount of time.
sustain :: Fractional t => t -> Track t a -> Track t a
sustain a = mapE $ \(Event s d c) -> Event s (a+d) c

-- | Prolongated events can not exceed total track duration.
-- All event are sustained but those that are close to 
-- end of the track are clipped. It resembles sustain on piano,
-- when track ends you release the pedal.
sustainT :: (Fractional t, Time t) => t -> Track t a -> Track t a
sustainT a x = mapE (\(Event s d c) -> Event s (turncate $ a+d) c) x
    where turncate = min $ dur x


-- | Shifts all events so that minimal start time
--  equals to zero.
alignByZero :: Time t => [Event t a] -> [Event t a]
alignByZero es = alignEvent <$> es
    where minT = minimum $ eventStart <$> es
          alignEvent e = e{ eventStart = eventStart e - minT } 

      
-----------------------------------------------
-- Temporal List

data TList t a =  Empty
                | Single a
                | Append  (TList t a) (TList t a)
                | TFun (Tfm t) (TList t a)
            deriving (Show, Eq)


foldT :: b -> (a -> b) -> (b -> b -> b) -> (Tfm t -> b -> b) 
    -> TList t a -> b
foldT empty single append tfun x = case x of
        Empty        -> empty
        Single a     -> single a
        Append a b   -> append (f a) (f b)
        TFun t a     -> tfun t (f a)
    where f = foldT empty single append tfun


instance Monoid (TList t a) where
    mempty                  = Empty
    mappend Empty   a       = a
    mappend a       Empty   = a
    mappend a       b       = Append a b


instance Functor (TList t) where
    fmap f = foldT Empty (Single . f) Append TFun


instance (Num t, Ord t) => Temporal (TList t a) where
    type Dur (TList t a) = t
    dur = maximum . fmap totalEventDur . renderTList 
        where totalEventDur = (+) <$> eventStart <*> eventDur

    stretch k x = case x of
        TFun t a   -> TFun (stretch k t) a
        Empty      -> Empty
        a          -> TFun (Tfm k 0) a

    delay k x = case x of
        TFun t a    -> TFun (delay k t) a
        Empty       -> Empty
        a           -> TFun (Tfm 1 k) a 


instance Foldable (TList t) where
    foldMap f = foldT mempty f mappend (flip const)

instance Monad (TList t) where
    return      = Single
    ma >>= mf   = foldT Empty mf Append TFun ma

eventMapTList :: Fractional t => 
    (Event t a -> Event t b) -> TList t a -> TList t b 
eventMapTList f =  (mapSingleEvent . f =<<) . eventList 
    where mapSingleEvent e = TFun (inv $ tfmFromEvent e) $ 
                             Single $ eventContent e


renderTList :: Num t => TList t a -> [Event t a]
renderTList = ($[]) . foldMap (:) . eventList 

eventList :: Num t => TList t a -> TList t (Event t a) 
eventList = iter unit
    where iter tfm x = case x of 
                Empty       -> Empty
                Single a    -> Single (eventFromTfm tfm a)
                TFun t a    -> iter (tfm `composeTfm` t) a    
                Append a b  -> Append (iter tfm a) (iter tfm b)
    

-- filtering

filterTList :: (Num t, Ord t) => (Event t a -> Bool) 
      -> TList t a -> TList t a
filterTList p = (filterEvent =<< ) . eventList
    where filterEvent e = if p e 
                    then Single (eventContent e) 
                    else Empty

nfTList :: (Num t, Ord t) => TList t a -> TList t a
nfTList = foldT Empty Single mappend (\(Tfm s d) -> delay d . stretch s)

-- transformation 
-- it's a pair of (stretch factor, delay offset)
data Tfm t = Tfm !t !t
    deriving (Show, Eq)

unit :: Num t => Tfm t
unit = Tfm 1 0

instance Num t => Temporal (Tfm t) where
    type Dur (Tfm t) = t
    dur (Tfm str del) = str + del
    stretch k (Tfm str del) = Tfm (k*str)  (k*del)
    delay   k (Tfm str del) = Tfm str      (k+del) 


eventFromTfm :: Tfm t -> a -> Event t a
eventFromTfm (Tfm str del) = Event del str

tfmFromEvent :: Event t a -> Tfm t
tfmFromEvent = Tfm <$> eventDur <*> eventStart

-- composition on transformations:
--  s2 `composeTfm` s1
composeTfm :: Num t => Tfm t -> Tfm t -> Tfm t
composeTfm (Tfm s2 d2) (Tfm s1 d1) = Tfm (s1*s2) (d1*s2 + d2)


-- inverse transformation 
-- t `composeTfm` inv t == unit
inv :: Fractional t => Tfm t -> Tfm t
inv (Tfm s d) = Tfm (1/s) (-d/s)



{-# Language 
        BangPatterns #-}

-- | A library for creating lists of constant time events related in time.
module Temporal.Media(
    -- * Introduction

    -- | "Temporal.Media" is a library for creating lists of constant time 
    -- events related in time. Constant time event is value
    -- that starts at some fixed time and lasts for some 
    -- fixed time. Library provides functions to build lists
    -- of such events with time-relations like sequent,  
    -- parallel or delayed. 
    --
    -- Core type of library is 'Track'. It provides interface
    -- to compose list of events. 
    
    -- * Types
    Time(..), Event(..), Track, dur, within, eventEnd,
    -- * Composition
    temp, stretch, delay, reflect, (+|), (*|), (=:=), (+:+), (=:/),
    line, chord, chordT, loop, rest, sustain, sustainT,    
    
    -- * Filtering
    clip, takeT, dropT, filterE,     
    -- * Mappings
    mapE, tmap, tmapRel,
    -- * Rendering
    render, alignByZero, sortEvents,
    
    -- * Monoid synonyms
    --
    -- | This package heavily relies on 'Monoid's, so there are shorcuts
    -- for 'Monoid' methods.    
    nil,
    module Data.Monoid,
    -- * Miscellaneous
    linseg, linsegRel

) where

import Data.Monoid
import Data.Foldable(Foldable(foldMap))

import Data.Ratio(Ratio)
import Control.Applicative

import Data.List(sortBy)
import Data.Ord(comparing)

class (Ord t, Num t) => Time t where

instance Time Int
instance Time Integer
instance Time Float
instance Integral a => Time (Ratio a)

-- TODO : optimise loops
-- reflect ??? 

-- Monoid shortcuts

-- | Synonym for method 'mempty'.
nil :: Monoid a => a 
nil = mempty

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

instance Time t => Monoid (Track t a) where
    mempty = Track 0 mempty
    mappend (Track d es) (Track d' es') = 
        Track (max d d') $ mappend es es'

-- | Calculates track's duration.
dur :: Track t a -> t
dur (Track d _) = d

-- | Stretches track in time domain.
stretch :: Time t => t -> Track t a -> Track t a
stretch k (Track d es) = Track (k*d) $ stretchTList k es

-- | Delays all events by given duration. 
delay :: Time t => t -> Track t a -> Track t a
delay k (Track d es) = Track (k+d) $ delayTList k es

-- | Infix 'delay' function.
(+|) :: Time t => t -> Track t a -> Track t a 
(+|) = delay

-- | Infix 'stretch' function.
(*|) :: Time t => t -> Track t a -> Track t a
(*|) = stretch

-- | Parallel composition. Play two tracks simultaneously.
(=:=) :: (Time t) => Track t a -> Track t a -> Track t a
a =:= b = a <> b 

-- | Sequent composition. Play first track then second.
(+:+) :: (Time t) => Track t a -> Track t a -> Track t a
a +:+ b = a <> delay (dur a) b

-- | Turncating parallel composition. Total duration
-- equals to minimum of the two tracks. All events
-- that goes beyond the lmimt are dropped.
(=:/) :: (Time t) => Track t a -> Track t a -> Track t a
a =:/ b = clip 0 (dur a `min` dur b) $ a <> b

-- | Parallel composition on list of tracks.
chord :: (Time t, Ord t) => [Track t a] -> Track t a
chord = mconcat

-- | Sequent composition on list of tracks.
line :: (Time t) => [Track t a] -> Track t a
line = foldr (+:+) nil

-- | Turncating parallel composition on list of tracks.
chordT :: (Time t) => [Track t a] -> Track t a
chordT xs = clip 0 (minimum $ dur <$> xs) $ chord xs

-- | Analog of 'replicate' function for tracks. Replicated
-- tracks are played sequentially.
loop :: (Time t) => Int -> Track t a -> Track t a
loop n = line . replicate n

-- | Reversing the tracks
reflect :: (Time t) => Track t a -> Track t a
reflect a = mapE (\e -> e{ eventStart = d - (eventStart e + eventDur e) }) a
    where d = dur a


-- | Empty track that lasts for some time.
rest :: (Time t) => t -> Track t a
rest = flip delay nil


instance Foldable (Track t) where
    foldMap f (Track _ x) = foldMap f x        

-- | 'clip' cuts piece of value within given time interval.
-- for @('clip' t0 t1 m)@, if @t1 < t0@ result is reversed.
-- If @t0@ is negative or @t1@ goes beyond @'dur' m@ blocks of
-- nothing inserted so that duration of result equals to 
-- @'abs' (t0 - t1)@.
clip :: (Time t) => t -> t -> Track t a -> Track t a
clip t0 t1 
    | t0 < t1   = clip' t0 t1
    | otherwise = reflect . clip' t1 t0

clip' :: (Time t) => t -> t -> Track t a -> Track t a
clip' t0 t1 = clipDur . delay (-t0) . filterE (within t0 t1)
    where clipDur (Track _ a) = Track (t1 - t0) a

-- | @('takeT' t)@ is equivalent to @('clip' 0 t)@.
takeT :: (Time t) => t -> Track t a -> Track t a
takeT t1 = clip 0 t1

-- | @('dropT' t m)@ is equivalent to @('clip' t (dur a) a)@.
dropT :: (Time t, Ord t) => t -> Track t a -> Track t a
dropT t0 a = clip t0 (dur a) a

-- | 'temp' constructs just an event. 
-- Value of type a lasts for one time unit and starts at zero.
temp :: (Time t) => a -> Track t a
temp = Track 1 . Single

-- | Get all events on recordered on the track. 
render :: Time t => Track t a -> [Event t a]
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

-- | End point of event (start time plus duration).
eventEnd :: Num t => Event t a -> t
eventEnd e = eventStart e + eventDur e

instance Functor (Event t) where
    fmap f e = e{ eventContent = f (eventContent e) }

durEvent = eventDur
delayEvent d e = e{ eventStart = eventStart e + d }
stretchEvent d e = e{ eventStart = eventStart e * d,
                      eventDur   = eventDur   e * d }

-- | Tests if given 'Event' happens between two time stamps.
within :: (Time t) => t -> t -> Event t a -> Bool
within t0 t1 e = within' t0 t1 (eventStart e) && within' t0 t1 (eventEnd e)
    where within' a b x = x >= a && x <= b

-- | General mapping. Mapps not only values but events. 
mapE :: Time t => (Event t a -> Event t b) -> Track t a -> Track t b 
mapE = onEvents . fmap

-- | Filter track. 
filterE :: Time t => (Event t a -> Bool) -> Track t a -> Track t a
filterE = onEvents . filter


onEvents :: Time t => ([Event t a] -> [Event t b]) -> Track t a -> Track t b
onEvents phi t@(Track d es) = Track d $ fromEventList $ phi $ render t


-- | Mapps values and time stamps.
tmap :: Time t => (Event t a -> b) -> Track t a -> Track t b
tmap f = mapE $ \e -> e{ eventContent = f e }

-- | Relative tmap. Time values are normalized by argument's duration. 
tmapRel :: (Time t, Fractional t) => (Event t a -> b) -> Track t a -> Track t b
tmapRel f x = tmap (f . stretchEvent (1 / dur x)) x


-- | After this transformation events last longer
-- by some constant amount of time.
sustain :: Time t => t -> Track t a -> Track t a
sustain a = mapE $ \e -> e{ eventDur = a + eventDur e }

-- | Prolongated events can not exceed total track duration.
-- All event are sustained but those that are close to 
-- end of the track are clipped. It resembles sustain on piano,
-- when track ends you release the pedal.
sustainT :: (Time t) => t -> Track t a -> Track t a
sustainT a x = mapE (\e -> turncate $ e{ eventDur = a + eventDur e }) x
    where turncate e
            | eventEnd e > d    = e{ eventDur = max 0 $ d - eventStart e }
            | otherwise         = e
          d = dur x


-- | Shifts all events so that minimal start time
--  equals to zero if first event has negative start time.
alignByZero :: (Time t) => [Event t a] -> [Event t a]
alignByZero es 
    | minT < 0  = alignEvent <$> es
    | otherwise = es
    where minT = minimum $ eventStart <$> es
          alignEvent e = e{ eventStart = eventStart e - minT } 

-- | Sorts all events by start time.
sortEvents :: Ord t => [Event t a] -> [Event t a]
sortEvents = sortBy (comparing eventStart)

-----------------------------------------------
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


durTList = maximum . fmap totalEventDur . renderTList 
    where totalEventDur = (+) <$> eventStart <*> eventDur

stretchTList k x = case x of
        TFun t a   -> TFun (stretchTfm k t) a
        Empty      -> Empty
        a          -> TFun (Tfm k 0) a

delayTList k x = case x of
        TFun t a    -> TFun (delayTfm k t) a
        Empty       -> Empty
        a           -> TFun (Tfm 1 k) a 

instance Foldable (TList t) where
    foldMap f = foldT mempty f mappend (flip const)


renderTList :: Num t => TList t a -> [Event t a]
renderTList = ($[]) . foldMap (:) . eventList 

eventList :: Num t => TList t a -> TList t (Event t a) 
eventList = iter unit
    where iter !tfm x = case x of 
                Empty       -> Empty
                Single a    -> Single (eventFromTfm tfm a)
                TFun t a    -> iter (tfm `composeTfm` t) a    
                Append a b  -> Append (iter tfm a) (iter tfm b)
    

fromEventList :: [Event t a] -> TList t a
fromEventList = foldr (mappend . phi) mempty
    where phi e = TFun (tfmFromEvent e) (Single $ eventContent e)

-- transformation 
-- it's a pair of (stretch factor, delay offset)
data Tfm t = Tfm !t !t
    deriving (Show, Eq)

unit :: Num t => Tfm t
unit = Tfm 1 0

durTfm (Tfm str del) = str + del
stretchTfm k (Tfm str del) = Tfm (k*str)  (k*del)
delayTfm   k (Tfm str del) = Tfm str      (k+del) 

eventFromTfm :: Tfm t -> a -> Event t a
eventFromTfm (Tfm str del) = Event del str

tfmFromEvent :: Event t a -> Tfm t
tfmFromEvent = Tfm <$> eventDur <*> eventStart

-- composition on transformations:
--  s2 `composeTfm` s1
composeTfm :: Num t => Tfm t -> Tfm t -> Tfm t
composeTfm (Tfm s2 d2) (Tfm s1 d1) = Tfm (s1*s2) (d1*s2 + d2)


---------------------------------------------------------------
-- Misc

-- | Linear interpolation. Can be useful with 'mapE' for 
-- envelope changes.
--
-- > linseg [a, da, b, db, c, ... ]
--
-- @a, b, c ...@ - values
--
-- @da, db, ...@ - duration of segments
linseg :: (Ord t, Fractional t) => [t] -> t -> t
linseg xs t = 
    case xs of
        (a:dur:b:[])      -> seg a dur b t
        (a:dur:b:(x:xs')) -> if t < dur 
                             then seg a dur b t
                             else linseg (b:x:xs') (t - dur)
    where seg a dur b t 
                | t < 0     = a
                | t >= dur  = b
                | otherwise = a + (b - a)*(t/dur)


-- | With 'linsegRel' you can make linear interpolation
-- function that has equal distance between points.
-- First argument gives total length of the interpolation function
-- and second argument gives list of values. So call
--
-- > linsegRel dur [a1, a2, a3, ..., aN]
--
-- is equivalent to:
--
-- > linseg [a1, dur/N, a2, dur/N, a3, ..., dur/N, aN]
linsegRel :: (Ord t, Fractional t) => t -> [t] -> t -> t
linsegRel dur xs = linseg $ init $ f =<< xs
    where dt  = dur / (fromIntegral $ length xs)
          f x = [x, dt]

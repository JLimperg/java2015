module Data.River
( RiverSystem
, mkSource
, mkJunction
, mkMouth
, isUpstreamReachable
, testNetwork
) where

import           Data.Maybe
import           Data.Monoid

-- | Representation of the structure of a river network, parameterised over
-- the information contained in each node. Note that pointers to other
-- networks are pointers to the /upstream/ (rather than downstream) parts of a
-- network, which is perhaps less intuitive but fits the problem we're
-- solving here better.
--
-- For simplicity, this representation allows @Mouth@s as upstreams, which
-- doesn't semantically make sense. The smart constructors prevent this.
data Network a
    = Source a
    | Junction a [Network a]
    | Mouth a (Network a)
  deriving (Read, Show, Eq, Ord)

instance Functor Network where
    fmap f (Source x)             = Source (f x)
    fmap f (Junction x upstreams) = Junction (f x) (map (fmap f) upstreams)
    fmap f (Mouth x upstream)     = Mouth (f x) (fmap f upstream)

instance Foldable Network where
    foldMap f (Source a)             = f a
    foldMap f (Junction a upstreams) = f a <> mconcat (map (foldMap f) upstreams)
    foldMap f (Mouth a upstream)     = f a <> foldMap f upstream

data RiverPoint
    = RiverPoint
    { coords :: (Int, Int)
    , label  :: String
    }
  deriving (Read, Show, Eq, Ord)

newtype RiverSystem = RiverSystem { fromRiverSystem :: Network RiverPoint }
  deriving (Read, Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Smart constructors which check for duplicate coordinates

mkSource :: RiverPoint -> RiverSystem
mkSource = RiverSystem . Source

mkJunction :: RiverPoint -> [RiverSystem] -> Maybe RiverSystem
mkJunction pt upstreams
    | null upstreams             = Nothing
    | any isMouth upstreams'     = Nothing
    | any (pt `elem`) upstreams' = Nothing
    | otherwise                  = Just . RiverSystem $ Junction pt upstreams'
  where
    upstreams' = map fromRiverSystem upstreams

mkMouth :: RiverPoint -> RiverSystem -> Maybe RiverSystem
mkMouth pt upstream
    | isMouth upstream'   = Nothing
    | pt `elem` upstream' = Nothing
    | otherwise           = Just . RiverSystem $ Mouth pt upstream'
  where
    upstream' = fromRiverSystem upstream

isMouth :: Network a -> Bool
isMouth (Mouth _ _) = True
isMouth _           = False

-------------------------------------------------------------------------------
-- Member checking (essentially free via Foldable and Functor)

isUpstreamReachable :: (Int, Int) -> RiverSystem -> Bool
isUpstreamReachable pt (RiverSystem sys) = pt `elem` fmap coords sys

-------------------------------------------------------------------------------
-- The network from the exercise

testNetwork :: RiverSystem
testNetwork =
    let s = mkSource $ RiverPoint (0, 0) "s"
        t = mkSource $ RiverPoint (2, 0) "t"
        b = fromJust $ mkJunction (RiverPoint (1, 1) "b") [s, t]
        u = mkSource $ RiverPoint (3, 1) "u"
        a = fromJust $ mkJunction (RiverPoint (2, 2) "a") [b, u]
        m = fromJust $ mkMouth (RiverPoint (2, 3) "m") a
    in m

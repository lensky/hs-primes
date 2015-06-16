module Primes
       (
       -- * Lazy prime number lists
        primes
       ,primes'
       -- * Efficient prime number routines
       ,primesTo
       ,primesTo')
where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

import qualified Data.Heap as DH

import Data.Maybe (fromJust)

import Control.Monad.ST (runST)

type SEPQ = (DH.MinHeap [Int], Int)

-- | The default wheel size for prime number sieves.
wheelSize :: Int
wheelSize = 6

getNextC :: SEPQ -> Int
getNextC = snd

insertPrime :: SEPQ -> Int -> SEPQ
insertPrime (pq,c) p = if p2 > c
                          then (DH.insert p2l pq, c)
                          else let (cl,pq') = fromJust $ DH.view pq
                               in (DH.insert (tail cl) $ DH.insert p2l pq', p2)
  where p2 = p*p
        p2l = [p2,p2+2*p..]

nextMin :: SEPQ -> SEPQ
nextMin sepq@(pq,_) = case DH.view pq of
                          Just (ml,pq') -> let pq'' = DH.insert (tail ml) pq'
                                           in (pq'', head $ fromJust $ DH.viewHead pq'')
                          Nothing -> sepq

-- | Infinite list of prime numbers.
primes' :: Int -- ^ sieve wheel size
        -> [Int]
primes' nw = reverse ps ++ start:sieve initSepq iws (start + iw)
  where (start, iw:iws, ps) = wheelFromPrimes nw
        initSepq = let s2 = start * start
                   in (DH.insert [s2,s2+2*start..] DH.empty, s2)
        sieve sepq (w:ws) i = case compare (getNextC sepq) i of
                                GT -> let sepq' = insertPrime sepq i
                                      in i : sieve sepq' ws (i+w)
                                EQ -> sieve (nextMin sepq) ws (i+w)
                                LT -> sieve (nextMin sepq) (w:ws) i

-- | 'primes'' with default wheel size.
primes :: [Int]
primes = primes' wheelSize

type Sieve = UV.Vector Bool

primeSieveTo' :: Int -> Int -> ([Int], Sieve)
primeSieveTo' nw n = let (start,wh,ips) = wheelFromPrimes nw
                     in runST $ do is <- MUV.replicate n False
                                   (ps,ms) <- spin wh start (ips, is)
                                   (,) ps <$> UV.freeze ms
  where updateSieve s p = mapM_ (flip (MUV.unsafeWrite s) True) [lstart,lstart + lstep..n-1]
          where lstart = p*p - 1
                lstep = 2*p
        isComposite s i = MUV.unsafeRead s (i - 1)
        sieveFold v@(ps,s) i = do composite <- isComposite s i
                                  if composite
                                     then return v
                                     else updateSieve s i >> return (i:ps, s)
        spin (w:whs) i v = if i >= n
                              then return v
                              else sieveFold v i >>= spin whs (i+w)

-- | List of primes up to (exclusive) some number. 
-- Can be (inefficiently) defined in terms of 'primes'' as
--
-- prop> primesTo' nw n = takeWhile (<n) $ primes' nw
primesTo' :: Int -- ^ wheel size
          -> Int -- ^ exclusive upper bound for prime numbers
          -> [Int]
primesTo' nw = fst . primeSieveTo' nw

-- | 'primesTo'' with default wheel size.
primesTo :: Int -- ^ exclusive upper bound for prime numbers
         -> [Int]
primesTo = primesTo' wheelSize

wheelPrimes :: Int -> ([Int], [Int])
wheelPrimes 0 = (repeat 1, [])
wheelPrimes 1 = (1:1:repeat 2, [2])
wheelPrimes n = (wheelInit ++ wheelCycle, ps)
  where (pwheel, ps') = wheelPrimes (n-1)
        ps = (sum . take n $ pwheel) + 1:ps'
        wheelInit = take (n+1) pwheel
        cycleInx1 = pwheel !! (n+1)
        genCycle lim s | lim > s = nextInx : genCycle lim (s + nextInx)
                       | otherwise = []
          where nextInx = head . dropWhile invalidInx $ [2..] 
                  where invalidInx i = any ((==) 0 . rem (s + i)) ps
        wheelCycle = let startInx = 1 + sum wheelInit
                         finalInx = startInx + product ps
                     in cycle $ cycleInx1 : genCycle finalInx (startInx + cycleInx1)

wheelFromPrimes :: Int -> (Int,[Int],[Int])
wheelFromPrimes n = let (w',ps) = wheelPrimes n
                        start = (sum . take (n+1) $ w') + 1
                        w = drop (n+1) w'
                    in (start, w, ps)


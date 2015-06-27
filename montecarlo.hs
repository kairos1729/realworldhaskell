module MonteCarlo (
  monteCarlo
  , cesaroTest
  , estimatePi
  , estimatePiMyRand
  , estimatePiMyStateRand
  , rands
  ) where
import System.Random
import Control.Applicative
import Control.Monad
import Control.Monad.State

-- this is not from real world haskell, but I wanted to try
-- the monte-carlo exercise from sicp in Haskell since the
-- purpose of that exercise was to show the benefits of
-- assignment for modularity.  Haskell doesn't have assignment,
-- so does this mean you cannot have modular programs?  No, I
-- think that monads are Haskell's answer to this.
-- The modular bit in the monte-carlo exercise was providing
-- an experiment which returned true or false when it was called,
-- but it was called with no arguments, so in Haskell this is not
-- possible.  Instead of providing a function for the experiment,
-- in haskell we can provide a monad, which allows us to stay
-- modular.
monteCarlo :: (Monad m) => Int -> m Bool -> m Double
monteCarlo numberOfTrials experiment = iter numberOfTrials (0 :: Int)
  where
    iter remaining passed =
          if remaining == 0
          then return $ (fromIntegral passed) / (fromIntegral numberOfTrials)
          else do
            experimentPassed <- experiment
            if experimentPassed
              then iter (remaining - 1) (passed + 1)
              else iter (remaining - 1) passed
          
cesaroTest :: IO Bool
cesaroTest = do
  r1 <- randomIO :: IO Int
  r2 <- randomIO :: IO Int
  return $ (gcd r1 r2) == 1

estimatePi :: Int -> IO Double
estimatePi numberOfTrials = do
  mc <- monteCarlo numberOfTrials cesaroTest
  return $ sqrt (6.0 / mc)

randUpdate :: Int -> Int
randUpdate x = (((1103515245 * x) + 12345) `div` 65536) `mod` 32768

newtype RandomValue a = RandomValue (Int -> (a, Int))

instance Monad RandomValue where
  return v = RandomValue (\s -> (v, s))
  RandomValue(rv) >>= f = RandomValue(newrandom)
    where newrandom s = r newseed
            where (result, newseed) = rv s
                  RandomValue(r) = f result

instance Functor RandomValue where
  fmap = liftM

instance Applicative RandomValue where
  pure = return
  (<*>) = ap
  

myCesaroTestMonad :: RandomValue Bool
myCesaroTestMonad = RandomValue(myCesaroTest)

myCesaroTest :: Int -> (Bool, Int)
myCesaroTest s = ((gcd r1 r2) == 1, r2)
  where r1 = randUpdate s
        r2 = randUpdate r1

estimatePiMyRand :: Int -> Int -> Double
estimatePiMyRand seed numberOfTrials = sqrt (6.0 / result)
  where RandomValue(mc) = monteCarlo numberOfTrials myCesaroTestMonad
        (result, _) = mc seed

rands :: (Num a, Eq a) => a -> Int -> [Int]
rands n seed = rands_iter n seed []
  where rands_iter 0 s1 vals = randUpdate s1 : vals
        rands_iter num s1 vals = newrand : rands_iter (num - 1) newrand vals
          where newrand = randUpdate s1

myStateCesaroTest :: State Int Bool
myStateCesaroTest = state $ \s -> let r1 = randUpdate s
                                      r2 = randUpdate r1
                                      in ((gcd r1 r2) == 1, s + 1)

estimatePiMyStateRand :: Int -> Int -> Double
estimatePiMyStateRand seed numberOfTrials = sqrt (6.0 / result)
  where s = runState $ monteCarlo numberOfTrials myStateCesaroTest
        (result, _) = s seed
        

type MyRandGen = State Int Int

myrand :: MyRandGen
myrand  = state $ \s -> let s1 = randUpdate s in
                         (s1, s1)
  

mycycle :: Int -> Int -> MyRandGen -> Int
mycycle max seed rg = cycle_iter seed seed 0
  where cycle_iter t h n
          | n > max = -1
          | otherwise = if (tval == hval) then n else cycle_iter newt newh (n + 1)
          where (tval, newt) = genrand t
                (_, temph) = genrand h
                (hval, newh) = genrand temph
        genrand = runState rg
          

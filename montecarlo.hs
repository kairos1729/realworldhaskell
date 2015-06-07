module MonteCarlo (monteCarlo, cesaroTest, estimatePi) where
import System.Random

-- this is not from real world haskell, but I wanted to try
-- the monte-carlo exercise from sicp in Haskell since the
-- purpose of that exercise was to show the benefits of
-- assignment for modularity.  Haskell doesn't have assignment,
-- so does this mean you cannot have modular programs?  No, I
-- think that modads are Haskell's answer to this.
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

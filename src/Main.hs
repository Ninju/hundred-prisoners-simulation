module Main where
import ListHelpers (shuffle)
import Control.Monad (replicateM)
import Data.List (all)
import qualified Data.Map as Map

data SimulationOutcome = SimulationOutcome
  {
    wins :: Int
  , fails :: Int
  } deriving Show

data Outcome = Freedom | Fail
             deriving (Eq)

type Box = Int
type Prisoner = Int

-- Given a prisoner, which boxes do they pick?
type Strategy = Prisoner -> [Box]

-- ^ halfAndHalfSrategy takes an nth prisoner and returns the list of box #s that prisoner should search
halfAndHalfSrategy :: Strategy
halfAndHalfSrategy nthPrisoner =
  if nthPrisoner < 50
  then [1..50]
  else [51..100]

main :: IO ()
main =
  let numberOfRuns = 1000 in
  do results <- runSimulationWithStrategy numberOfRuns halfAndHalfSrategy
     putStrLn $ "Ran " ++ show numberOfRuns ++ " times"
     print results

runSimulationWithStrategy ntimes strategy =
  do individualRunResults <- replicateM ntimes (runSimulation' strategy)
     let wins = length $ filter (== True) individualRunResults
     let fails = length $ filter (== False) individualRunResults
     return $ SimulationOutcome { wins = wins, fails = fails }


runSimulation' strategy =
  do assignments <- assignPrisonersToBoxes
     randomlyOrderedPrisoners <- shuffle [1..100]
     let prisonerChoices = zip randomlyOrderedPrisoners $ map strategy randomlyOrderedPrisoners
     let prisonerChoicesAsMap = Map.fromList prisonerChoices
     return $ evaluatePrisonerChoices assignments prisonerChoicesAsMap

evaluatePrisonerChoices assignments choices =
    Map.isSubmapOfBy elem assignments choices && Map.isSubmapOfBy (flip elem) choices assignments


assignPrisonersToBoxes =
  do boxes <- shuffle [1..100]
     return $ Map.fromList (zip [1..100] boxes)

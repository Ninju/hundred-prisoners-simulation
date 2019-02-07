module Main where
import Control.Monad (replicateM)
import Data.List (all)
import qualified Data.Map as Map

import App.Core (Box, Prisoner)
import App.Helpers.List (shuffle)
import App.Prisoners.Strategies (runStrategy, halfAndHalfSrategy)

data SimulationOutcome = SimulationOutcome
  {
    wins :: Int
  , fails :: Int
  } deriving Show

type Box = Int
type Prisoner = Int

main =
  let numberOfRuns = 1000 in
  do results <- runSimulationWithStrategy numberOfRuns (runStrategy halfAndHalfSrategy)
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

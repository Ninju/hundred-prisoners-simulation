module App.Prisoners.Strategies
  (
    -- type @Strategy@
    Strategy
  , runStrategy
  , halfAndHalfSrategy
  ) where

import App.Core (Box, Prisoner)

-- Given a prisoner, which boxes do they pick?
data Strategy = Strategy { runStrategy :: Prisoner -> [Box] }

-- ^ halfAndHalfSrategy takes an nth prisoner and returns the list of box #s that prisoner should search
halfAndHalfSrategy :: Strategy
halfAndHalfSrategy = Strategy
  {
    runStrategy = \nthPrisoner -> if nthPrisoner < 51
                                  then [1..50]
                                  else [51..100]
  }

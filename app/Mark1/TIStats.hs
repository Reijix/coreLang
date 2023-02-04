module TIStats (TIStats, tiStatInitial, tiStatIncSteps, tiStatGetSteps) where

-- interface
tiStatInitial :: TIStats
tiStatIncSteps :: TIStats -> TIStats
tiStatGetSteps :: TIStats -> Int

-- implementation
type TIStats = Int

tiStatInitial  = 0
tiStatIncSteps = (+ 1)
tiStatGetSteps = id
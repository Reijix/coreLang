module TIStats (TIStats, tiStatInitial, tiStatIncSteps, tiStatGetSteps, tiStatIncScReductions, tiStatGetScReductions, tiStatIncPrimitiveReductions, tiStatGetPrimitiveReductions,
                tiStatIncCurStackDepth, tiStatDecCurStackDepth, tiStatGetMaxStackDepth) where

-- interface
tiStatInitial :: TIStats

tiStatIncSteps :: TIStats -> TIStats
tiStatGetSteps :: TIStats -> Int

tiStatIncScReductions :: TIStats -> TIStats
tiStatGetScReductions :: TIStats -> Int

tiStatIncPrimitiveReductions :: TIStats -> TIStats
tiStatGetPrimitiveReductions :: TIStats -> Int

tiStatIncCurStackDepth :: TIStats -> TIStats
tiStatDecCurStackDepth :: Int -> TIStats -> TIStats

tiStatGetMaxStackDepth :: TIStats -> Int

-- implementation
-- (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth)
newtype TIStats = TIStatsImpl (Int, Int, Int, Int, Int)

tiStatInitial  = TIStatsImpl (0, 0, 0, 0, 0)

tiStatIncSteps (TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth))
    = TIStatsImpl (steps + 1, scReductions, primitiveReductions, curStackDepth, maxStackDepth)
tiStatGetSteps (TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth)) = steps

tiStatIncScReductions (TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth))
    = TIStatsImpl (steps, scReductions + 1, primitiveReductions,  curStackDepth, maxStackDepth)
tiStatGetScReductions (TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth)) = scReductions

tiStatIncPrimitiveReductions (TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth))
    = TIStatsImpl (steps, scReductions, primitiveReductions + 1,  curStackDepth, maxStackDepth)
tiStatGetPrimitiveReductions (TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth)) = primitiveReductions

tiStatIncCurStackDepth (TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth))
    = TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth + 1, newMaxStackDepth)
    where 
        newMaxStackDepth = max (curStackDepth + 1) maxStackDepth
tiStatDecCurStackDepth n (TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth))
    = TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth - n, maxStackDepth)
tiStatGetMaxStackDepth (TIStatsImpl (steps, scReductions, primitiveReductions, curStackDepth, maxStackDepth)) = maxStackDepth
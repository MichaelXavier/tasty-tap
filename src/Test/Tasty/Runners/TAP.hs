-- | This is a TAP13 (Test Anything Protocol version 13) compatible
-- test output formatter for tasty. You can read more about tap
-- <https://testanything.org/ here>.
module Test.Tasty.Runners.TAP
    ( tapRunner
    , tapRunner'
    ) where


-------------------------------------------------------------------------------
import           Control.Concurrent.STM
import qualified Data.IntMap.Strict     as IM
import           Data.Monoid
import           System.IO
import           Test.Tasty.Ingredients
import           Test.Tasty.Runners
-------------------------------------------------------------------------------


tapRunner :: Ingredient
tapRunner = tapRunner' stdout

-------------------------------------------------------------------------------
tapRunner' :: Handle -> Ingredient
tapRunner' h = TestReporter opts go
  where opts = []
        go _ _ = (Just (tapCallback h))


-------------------------------------------------------------------------------
tapCallback :: Handle -> StatusMap -> IO (Time -> IO Bool)
tapCallback h smap = do
  hPutStrLn h tapHeader
  hPutStrLn h (tapPlan smap)
  results <- mapM (uncurry (reportStatus h)) (IM.toList smap)
  return (const (return (and results)))


-------------------------------------------------------------------------------
tapHeader :: String
tapHeader = "TAP version 13"


-------------------------------------------------------------------------------
tapPlan :: StatusMap -> String
tapPlan sm | IM.null sm = "Bail out! There were no tests to run."
           | otherwise = "1.." <> show (IM.size sm)


-------------------------------------------------------------------------------
reportStatus :: Handle -> Int -> TVar Status -> IO Bool
reportStatus h zeroIdx statRef = do
    res <- atomically (waitFinished )
    hPutStrLn h (renderResult res)
    return (resultSuccessful res)
  where testNum = zeroIdx + 1
        waitFinished = do stat <- readTVar statRef
                          case stat of
                            Done res -> return res
                            _        -> retry
        renderResult res
          | resultSuccessful res = "ok " <> show testNum <> desc
          | otherwise = "not ok " <> show testNum <> desc
          where desc = case resultDescription res of
                         "" -> ""
                         nonEmpty -> " - " <> nonEmpty

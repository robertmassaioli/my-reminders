module StatisticsHandlers (handleStatistics) where

import qualified AppConfig              as CONF
import           AppHelpers
import           Application
import           Persistence.Statistics
import qualified Snap.Core              as SC
import           SnapHelpers

handleStatistics :: AppHandler ()
handleStatistics = handleMethods
    [ (SC.GET, handleGetStatistics)
    ]

handleGetStatistics :: AppHandler ()
handleGetStatistics = getKeyAndConfirm CONF.rmStatisticsKey $ do
    SC.setTimeout 60 -- The statistics job may take a long time
    statistics <- getStatistics
    writeJson statistics
    respondWith ok


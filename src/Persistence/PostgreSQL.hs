module Persistence.PostgreSQL (
    withConnection
  , insertReturning
) where

import qualified Data.Pool                            as P
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple
import           Snap.Snaplet.PostgresqlSimple        (getPostgresState, pgPool, HasPostgres)
import           Control.Monad.IO.Class

insertReturning :: (FromField a, ToRow q) => Connection -> Query -> q -> IO [[a]]
insertReturning = query

withConnection :: HasPostgres m => (Connection -> IO a) -> m a
withConnection f = do
    postgres <- getPostgresState
    P.withResource (pgPool postgres) $ \conn ->
        liftIO $ withTransaction conn $
            f conn

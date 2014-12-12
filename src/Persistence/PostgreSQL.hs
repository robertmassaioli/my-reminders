module Persistence.PostgreSQL
  ( withConnection
  , insertReturning
  ) where

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple
import Snap.Snaplet.PostgresqlSimple (getPostgresState, HasPostgres, Postgres(..))
import Control.Monad.IO.Class

import qualified Data.Pool as P

insertReturning :: (FromField a, ToRow q) => Connection -> Query -> q -> IO [[a]]
insertReturning = query

withConnection :: HasPostgres m => (Connection -> IO a) -> m a
withConnection f = do
  postgres <- getPostgresState
  let postgresPool = case postgres of PostgresPool pgPool -> pgPool
                                      _ -> error "Postgres connection is not a Pool"
  P.withResource postgresPool $ \conn ->
    liftIO $ withTransaction conn (f conn)

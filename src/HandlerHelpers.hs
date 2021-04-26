module HandlerHelpers
    ( withErr
    , withErrCode
    , withErrs
    , writeError
    , writeErrors
    , pe
    ) where

import           Application
import qualified Control.Arrow            as A
import           Data.String.ToString
import           SnapHelpers

-- pe stands for "pluralize errors"
pe :: Either (Int, String) b -> Either (Int, [String]) b
pe = A.left (A.second return)

-- TODO the names for these error methods are a mess, rename them to be sensibly named
withErr :: ToString a => Int -> String -> Either a b -> Either (Int, String) b
withErr code prefix = A.left (withErrHelper code prefix)

withErrHelper :: ToString a => Int -> String -> a -> (Int, String)
withErrHelper code prefix msg = (code, prefix ++ " " ++ toString msg)

withErrCode :: ToString a => Int -> Either a b -> Either (Int, String) b
withErrCode code (Left msg) = Left (code, toString msg)
withErrCode _    (Right x)  = Right x

withErrs :: Int -> Either a b -> Either (Int, a) b
withErrs code = A.left ((,) code)

writeError :: AppHandler (Either (Int, String) b) -> AppHandler ()
writeError m = do
   res <- m
   case res of
      Right _ -> return ()
      Left err -> uncurry respondWithError err

writeErrors :: AppHandler (Either (Int, [String]) b) -> AppHandler ()
writeErrors m = do
   res <- m
   case res of
     Right _ -> return ()
     Left err -> uncurry respondWithErrors err
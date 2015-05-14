{-# LANGUAGE OverloadedStrings #-}
module CustomSplices (spliceConfig) where

import           Control.Lens ((&), (.~))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Heist.Internal.Types as HIT
import qualified Snap.Snaplet as SS
import qualified Snap.Snaplet.Heist as SSH
import qualified Text.XmlHtml as X

spliceConfig :: H.SpliceConfig (SS.Handler a a)
spliceConfig = mempty 
   & HIT.scInterpretedSplices .~ customSplices

customSplices :: HIT.Splices (HI.Splice (SS.Handler a a))
customSplices = do
   "hasSplice" H.## hasSplice
   "includeFile" H.## includeFile
   "js" H.## jsInclude
   "css" H.## cssInclude
   H.defaultInterpretedSplices

hasSplice :: SSH.SnapletISplice a
hasSplice = do
   potentialTokenName <- fmap (X.getAttribute "name") H.getParamNode
   case potentialTokenName of
      Just tokenName -> do
         tokenSplice <- fmap (HI.lookupSplice tokenName) H.getHS
         case tokenSplice of
            Just _ -> HI.runChildren
            Nothing -> return . comment $ "Could not find the variable '" ++ show tokenName ++ "' in the heist context."
      Nothing -> return . comment $ "Could not find 'name' attribute."

includeFile :: SSH.SnapletISplice a
includeFile = do
   potentialFile <- fmap (X.getAttribute "file") H.getParamNode
   case potentialFile of
      Nothing -> return . comment $ "No content could be loaded"
      Just filePath -> do
         fileContents <- liftIO (readFile . T.unpack $ filePath)
         return . text $ fileContents

jsInclude :: SSH.SnapletISplice a
jsInclude = do
   potentialSrc <- fmap (X.getAttribute "src") H.getParamNode
   case potentialSrc of
      Nothing -> return . comment $ "<js> tag had no 'src' attribute"
      Just src -> return [X.Element 
         { X.elementTag = T.pack "script"
         , X.elementAttrs =
            [ (T.pack "src", src)
            , (T.pack "type", T.pack "text/javascript")
            ]
         , X.elementChildren = []
         }]

-- <link rel="stylesheet" type="text/css" href="mystyle.css">
cssInclude :: SSH.SnapletISplice a
cssInclude = do
   potentialHref <- fmap (X.getAttribute "href") H.getParamNode
   case potentialHref of
      Nothing -> return . comment $ "<css> tag had no 'href' attribute"
      Just href -> return [X.Element 
         { X.elementTag = T.pack "link"
         , X.elementAttrs =
            [ (T.pack "href", href)
            , (T.pack "type", T.pack "text/css")
            , (T.pack "rel", T.pack "stylesheet")
            ]
         , X.elementChildren = []
         }]

comment :: String -> [X.Node]
comment x = [X.Comment (T.pack x)]

text :: String -> [X.Node]
text x = [X.TextNode (T.pack x)]


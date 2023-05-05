{-# LANGUAGE 
      OverloadedStrings
    , DuplicateRecordFields
    , ViewPatterns 
    , NamedFieldPuns 
    , DeriveGeneric 
    , DeriveAnyClass 
    , StandaloneDeriving
    , BlockArguments
    , TypeSynonymInstances 
    , TypeFamilies
    , FlexibleInstances 
    , FlexibleContexts
    , ImpredicativeTypes 
    , LambdaCase 
    #-}

module Main (main) where

import Data.Lightning
import Data.Lightning.Generic
import Control.Plugin
import Control.Client 
import Control.Monad.State
import Control.Lens ( (^?) )
import Data.Aeson 
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Text (Text, unpack, pack) 
import GHC.Generics
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import System.Process (callCommand) 
import System.Random
import Control.Monad.Reader
import Data.Time.Clock.POSIX
import Data.Int
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Maybe
import Network.Wai.Handler.Warp
import Servant hiding (respond) 
import SplitDb

prin m = liftIO $ appendFile "/home/o/.ao/storm" $ show m <> "\n"
lcli :: (MonadReader Plug m, MonadIO m) => PartialCommand -> m (Maybe (Res Value)) 
lcli = lightningCliDebug prin

main :: IO ()
main = plugin m s p

-- | configure the plugin
m = object [
      "dynamic" .= True
    , "options" .= ([]::[Option])
    , "rpcmethods" .= ([
          RpcMethod "split" "invoice" "register invoice for split" Nothing False  
        , RpcMethod "part" "splitid amount_msat [name]" "create part of a split" Nothing False
        , RpcMethod "listsplits" "[id]" "show status  splits" Nothing False
        ])
    ] 


-- | store the database Connection in state, run webserver
s :: InitMonad Connection
s = do 
    Init _ (InitConfig {lightning5dir}) <- asks conf  
    let dblocation = unpack lightning5dir <> "/splits.sqlite3"
    conn <- liftIO $ open dblocation 
    liftIO $ createDb conn
    return conn
                   
-- | data handler
p :: PluginApp Connection
p (Just i, "split", parseArg -> spl) = case spl of 
    Just invoice -> do
        Just d <- lightningCli $ decodepay invoice
        case d of 
            ErrRes m _ -> respond (toJSON m) i
            Res v _ -> do
                xid <- liftIO . fmap abs $ (randomIO :: IO Int32) 
                conn <- get
                liftIO $ insertSplit conn (Split xid invoice)
                respond (object ["splitid".=xid,"info".=v]) i
    _ -> respond "invoice parameter required" i

p (Just i, "part", parsePart -> v) = case v of 
    Just (PP xid msats mn) -> do 
        conn <- get 
        found <- liftIO $ lookupSplit conn xid
        case found of 
            Just (Split _ _) -> do 
                parts <- liftIO $ lookupParts conn xid
                let partLabel = (show xid <> "-" <> show (length parts))
                freei <- lcli $Command "freezeinvoice" Nothing (object [
                      "label".=partLabel
                    , "amount_msat".=msats
                    , "description".= ("split"::Text)     
                    ])
                case freei of 
                    Just (Res b@(fromJSON -> Success (Bolt11 b' l')) _) -> do
                        liftIO $ insertPart conn (Part l' mn (SplitId xid))
                        respond (toJSON b) i
                    _ -> pure () 
            _ -> respond (toJSON $ "valid splitid required, invalid: " <> show xid) i
    _ -> respond "splitid & amount_msat required" i

p (Just i, "listsplits", v) = do 
    conn <- get
    case getSplitid v of
        Nothing -> (liftIO $ allSplits conn) >>= \all ->
            respond (object ["splits" .= all] ) i
        Just sid -> do
            parts <- liftIO $ lookupParts conn sid
            split <- splitStatus sid
            parts' <- mapM partStatus parts
            respond (toJSON (split, parts')) i
    where 
    getSplitid :: Value -> Maybe Int32
    getSplitid v = case v of 
        v@(Array _) -> fmap fromInteger $ v ^? nth 0 . _Integer
        Object v -> parseMaybe (.: "splitid") v
        _ -> Nothing

p (Just i, _, _) = release i
p _ = pure ()

splitStatus sid = do 
    conn <- get
    Just (Split _ b')  <- liftIO $ lookupSplit conn sid
    Just (Res pv _) <- lightningCli $ decodepay b'
    pure (b', pv)    

-- partStatus :: Part -> IO 
partStatus (Part lbl mn _) = do
    conn <- get
    aa <- lightningCli $ getinvoice lbl
    bb <- lightningCli $ gethold lbl 
    pure (aa, bb) 
    
data Bolt1 = Bolt1 {
      bolt11 :: Text 
    }deriving (Generic)
instance FromJSON Bolt1
data Bolt11 = Bolt11 {
      bolt11 :: Text 
    , label :: Text}
    deriving (Generic)
instance FromJSON Bolt11
instance ToJSON Bolt11

gethold :: Text -> PartialCommand
gethold p = Command "listholds" filt (object ["label".=p])
    where filt = Just $ object ["_status".=True]
getinvoice :: Text -> PartialCommand
getinvoice p = Command "listinvoices" filt param
    where filt = Just $ object ["invoices".=[object [
                      "bolt11".=True
                    , "status".=True
                    , "amount_msat".=True]]]
          param = object ["label".=p]

decodepay :: Text -> PartialCommand
decodepay p = Command "decodepay" filt param
    where filt = Just $ object ["amount_msat".=True, "payee".=True, "description".=True]
          param = object ["bolt11".= p]

parseArg :: Value -> Maybe Text
parseArg (Object o) = parseMaybe (.: "invoice") o
parseArg a@(Array _) = a ^? nth 0 . _String

data PP = PP Int32 Msat (Maybe Text) 
parsePart :: Value -> Maybe PP
parsePart (Object o) = do 
    Just si <- parseMaybe (.: "splitid") o
    Just msats <- parseMaybe (.: "amount_msat") o
    Just $ PP si msats (parseMaybe (.: "name") o)  
parsePart a@(Array _) = do 
    (fromInteger -> si) <- a ^? nth 0 . _Integer
    (fromInteger -> msats) <- a ^? nth 1 . _Integer
    Just $ PP si msats (a ^? nth 2 . _String)


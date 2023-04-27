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
--import Control.Exception
import Network.Wai.Handler.Warp
import Servant hiding (respond) 
import AoServer
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
        , RpcMethod "part" "splitid [name]" "create a freeze invoice that is part of a split" Nothing False
        , RpcMethod "listsplits" "[id]" "show status  splits" Nothing False
        ])
    ] 

data Splt = Splt Connection (Chan AoEvent)

-- | store the database Connection in state, run webserver
s :: InitMonad Connection
s = do 
    Init _ (InitConfig {lightning5dir}) <- asks conf  
    let dblocation = unpack lightning5dir <> "/splits.sqlite3"
    conn <- liftIO $ open dblocation 
    liftIO $ createDb conn
    ch <- liftIO$newChan
    pl <- ask
    _ <- liftIO.forkIO $ runSuede pl 
                                  8153
                                  "/home/o/o/Projects/split/frontend/dist"
                                  (bb)
                                  ch
    return conn
    where 
    bb :: Chan AoEvent -> Value -> ReaderT Plug Handler NoContent 
    bb i v = do 
        Just (Res v2 _) <- lightningCli (Command "getinfo" Nothing (object [])) 
        liftIO $ writeChan i $ AoEvent v2 
        return NoContent 
                   
frz = object [ "amount".= ("any"::Text)  ]

-- | data handler
p :: PluginApp Connection
p (Just i, "split", parseArg -> spl) = case spl of 
    Just invoice -> do
        Just d <- lightningCli $ decodepay invoice
        case d of 
            ErrRes m _ -> respond (toJSON m) i
            Res v _ -> do
                xid <- liftIO $ (randomIO :: IO Int32) 
                conn <- get
                liftIO $ insertSplit conn (Split xid invoice)
                respond (object ["splitid".=xid,"info".=v]) i
    _ -> respond "invoice and parts required" i

p (Just i, "part", parseParg -> v) = case v of 
    Just (PP xid mn) -> do 
        conn <- get 
        found <- liftIO $ lookupID conn xid
        case found of 
            Just (Split _ _) -> do 
                Just (Res b@(fromJSON -> Success (Bolt11 b' l')) _) 
                    <- lcli $Command "freezeinvoice" Nothing (frz)
                liftIO $ insertPart conn (Part l' Nothing (SplitId xid))
                respond b i
            _ -> respond (toJSON $ "valid splitid required, invalid: " <> show xid) i
       -- liftIO $ insertPart conn 
    _ -> respond "splitid required" i
    
    -- splitid ++ name 


p (Just i, "listsplits", _) = do 
      (runQuery . runSelectReturningList . select . all_ $ _splits splitdb) >>= \all ->
         respond (object ["splits" .= all] ) i
p (Just i, _, _) = release i
p _ = pure ()



parseAmt :: Value -> Text
parseAmt v = "any" -- undefined

data Bolt1 = Bolt1 {
      bolt11 :: Text 
    }deriving (Generic)
instance FromJSON Bolt1
data Bolt11 = Bolt11 {
      bolt11 :: Text 
    , label :: Text}
    deriving (Generic)
instance FromJSON Bolt11


decodepay :: Text -> PartialCommand
decodepay p = Command "decodepay" filt param
    where filt = Just $ object ["amount_msat".=True, "payee".=True, "description".=True]
          param = object ["bolt11".= p]

parseArg :: Value -> Maybe Text
parseArg (Object o) = parseMaybe (.: "invoice") o
parseArg a@(Array _) = a ^? nth 0 . _String


data PPP = PP Int32 (Maybe Text) 
parseParg (Object o) = do 
    Just si <- parseMaybe ((.: "splitid")) o
    na <- parseMaybe (.: "name") o  
    Just $ PP si na
parseParg a@(Array _) = do 
    (fromInteger -> si) <- a ^? nth 0 . _Integer
    Just $ PP si (a ^? nth 1 . _String)

changed' :: Text -> Text -> Value 
changed' l n = object ["success" .= True, "label" .= l, "status" .= n ]

warning' :: Text -> Value 
warning' l = object ["success" .= False, "warning" .= l ]

getlabel :: Params -> Maybe Text
getlabel v = case v of 
    v@(Array _) -> v ^? nth 0 . _String
    Object v -> parseMaybe (.: "label") v

expireFilter = Just $ object ["invoices" .= [object ["expires_at".=True]]]
data Invs a = Invs {invoices :: [a]} deriving (Generic) 
instance FromJSON (Invs Expire)
instance FromJSON (Invs Ha)
data Expire = Expire { expires_at :: Int } deriving (Generic, Show) 
instance FromJSON Expire 
getExpiry label = do 
    Just (Res (fromJSON -> Success (Invs (Expire ex : _))) _)  
        <- lightningCli (Command "listinvoices" expireFilter (object ["label".=label]))
    pure ex 
data Ha = Ha { payment_hash :: Text } deriving (Generic, Eq, Show) 
instance FromJSON Ha 
getHash label = do 
    Just (Res (fromJSON -> Success (Invs (Ha pha : _))) _) 
        <- lightningCli (Command "listinvoices" hashFilter (object ["label".=label]))
    pure pha
hashFilter = Just $ object ["invoices" .= [object ["payment_hash".=True]]]
htlcFilter = Just $ object ["htlcs" .= [object ["expiry".=True, "payment_hash".=True]]]
data Htlcs = Htlcs {htlcs :: [Htlc']}  deriving (Generic, Show)
instance FromJSON Htlcs
data Htlc' = Htlc' { expiry :: Int, payment_hash :: Text } deriving (Generic, Eq, Show)
instance FromJSON Htlc'
_pha :: Htlc' -> Text
_pha = payment_hash


runQuery :: SqliteM c -> PluginMonad Connection c
runQuery q = do
    conn <- get 
    liftIO $ runBeamSqlite conn $ q

lookupSplit l = runSelectReturningList $ lookup_ (_splits splitdb) (SplitId l) 

--updateSplit split newStatus = runUpdate $ save (_splits splitDb) (split { _status = newStatus})

--deleteSplit lbl = runDelete $ delete (_splits splitdb) (\c ->  (_splitid c) ==. (val_ $ lbl)) 


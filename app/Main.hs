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
    , LambdaCase #-}

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
          RpcMethod "split" "invoice parts" "create several lightning invoices, after all are paid: pay the supplied invoice!" Nothing False  
        , RpcMethod "listsplits" "[id]" "show status  splits" Nothing False
        ])
    ] 

-- | store the connection in state
s :: InitMonad Connection
s = do 
    Init _ (InitConfig {lightning5dir}) <- asks conf  
    let dblocation = unpack lightning5dir <> "/splits.sqlite3"
    liftIO $ callCommand $ "touch " <> dblocation
    conn <- liftIO $ open $ dblocation 
    liftIO $ execute_ conn sqlCreate
    return conn

-- | data handler
p :: PluginApp Connection
p (Just i, "split", parseArg -> spl) = case spl of 
    Just (S invoice parts) -> do
        Just d <- lightningCli $ decodepay invoice
        case d of 
            ErrRes m _ -> respond (toJSON m) i
            Res (parseAmt -> amt) _ -> do
                gen <- liftIO $ initStdGen 
                inv <- mapM (createinvoice amt.pack.show) 
                    $ take (fromInteger parts) 
                    $ (randoms gen :: [Int32] ) 
                prin "todo data insert"
                respond (toJSON inv) i
    _ -> respond "invoice and parts required" i

p (Just i, "listsplits", _) = do 
      (runQuery . runSelectReturningList . select . all_ $ _splits splitdb) >>= \all ->
         respond (object ["splits" .= all] ) i
p (Just i, _, _) = release i
p _ = pure ()



parseAmt :: Value -> Text
parseAmt v = "any" -- undefined

createinvoice :: Text -> Text -> PluginMonad a (Maybe Text)
createinvoice amt lbl = do
    Just (Res (fromJSON -> Success (Bolt11 b11)) _) <- lcli $ Command "invoice" b11 params
    pure.Just $ b11
    where 
    b11 = Just $ object ["bolt11" .= True]
    params = object [
          "label".=lbl
        , "amount_msat".=amt
        , "description".=("split"::Text)
        ] 


data Bolt11 = Bolt11 {
    bolt11 :: Text }
    deriving (Generic)
instance FromJSON Bolt11


decodepay :: Text -> PartialCommand
decodepay p = Command "decodepay" filt param
    where filt = Just $ object ["amount_msat".=True]
          param = object ["bolt11".= p]

data Split = S Text Integer
-- | parseArgs
parseArg :: Value -> Maybe Split  
parseArg (Object o) = do 
    invoice <- parseMaybe (.: "invoice") o
    parts <- parseMaybe (.: "parts") o  
    Just $ S invoice parts
parseArg a@(Array _) = do 
    invoice <-  a ^? nth 0 . _String
    parts <- a ^? nth 1 . _Integer
    Just $ S invoice parts

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

sqlCreate = "CREATE TABLE IF NOT EXISTS splits (splitid TEXT, invoices TEXT)"
data SplitDb f = SplitDb {
    _splits :: f (TableEntity SplitT)
    } deriving (Generic, Database Sqlite) 
data SplitT f = Split {
      _splitid :: Columnar f Int32
    , _invoices :: Columnar f Text
    } deriving (Generic, Beamable) 
type Split2 = SplitT Identity
type SplitId = PrimaryKey SplitT Identity
splitdb :: DatabaseSettings be SplitDb
splitdb = defaultDbSettings

instance ToJSON (SplitT Identity) -- where 
 --   toJSON h = object $ splito  h
 --   where splito h = []

instance Table SplitT where
    data PrimaryKey SplitT f = SplitId (Columnar f Int32) deriving (Generic, Beamable)
    primaryKey = SplitId . _splitid

runQuery :: SqliteM c -> PluginMonad Connection c
runQuery q = do
    conn <- get 
    liftIO $ runBeamSqlite conn $ q

lookupSplit l = runSelectReturningList $ lookup_ (_splits splitdb) (SplitId l) 

-- updateSplit split newStatus = runUpdate $ save (_splits splitDb) (split { _status = newStatus})

-- deleteSplit lbl = runDelete $ delete (_splits splitdb) (\c ->  (_splitid c) ==. (val_ $ lbl)) 


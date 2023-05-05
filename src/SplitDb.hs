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
    #-}

module SplitDb where

import Data.Aeson
import Data.Int
import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Network.Wai.Handler.Warp

splitdb :: DatabaseSettings be SplitDb
splitdb = defaultDbSettings
database :: CheckedDatabaseSettings Sqlite SplitDb
database = defaultMigratableDbSettings

createDb conn = runBeamSqlite conn $ do 
   veri <- verifySchema migrationBackend database 
   _ <- checkSchema migrationBackend database mempty
   case veri of 
       VerificationFailed _ -> autoMigrate migrationBackend database
       VerificationSucceeded -> pure () 

lookupSplit :: Connection -> Int32 -> IO (Maybe (SplitT Identity)) 
lookupSplit conn ld = runBeamSqlite conn .runSelectReturningOne
     $ do 
         lookup_ (_splits splitdb) (SplitId ld)

lookupParts :: Connection -> Int32 -> IO [Part]
lookupParts conn ld = runBeamSqlite conn . runSelectReturningList . select
    $ filter_ (\s ->  _splitref s ==. (val_$ SplitId ld)) 
    $ all_ (_parts splitdb) 

allSplits :: Connection -> IO [Split]
allSplits conn = runBeamSqlite conn . runSelectReturningList . select
    $ all_ (_splits splitdb)

insertSplit :: Connection -> Split -> IO ()
insertSplit conn s = runBeamSqlite conn .runInsert.insert (_splits splitdb)
    $ insertValues [s]

insertPart :: Connection -> Part -> IO () 
insertPart conn p = runBeamSqlite conn .runInsert.insert (_parts splitdb)
    $ insertValues [p]

data SplitDb f = SplitDb {
      _splits :: f (TableEntity SplitT)
    , _parts :: f (TableEntity PartT)
    } deriving (Generic, Database Sqlite) 
data SplitT f = Split {
      _splitid :: Columnar f Int32
    , _bolt11 :: Columnar f Text
    } deriving (Generic, Beamable) 
type Split = SplitT Identity
type SplitId = PrimaryKey SplitT Identity
data PartT f = Part {
      _label :: Columnar f Text
    , _name :: Columnar f (Maybe Text)
    , _splitref :: PrimaryKey SplitT f
    } deriving (Generic, Beamable)
type Part = PartT Identity
type PartId = PrimaryKey PartT Identity
   
instance Table PartT where 
    data PrimaryKey PartT f = PartId (Columnar f Text) 
        deriving (Generic, Beamable)
    primaryKey = PartId . _label

instance Table SplitT where
    data PrimaryKey SplitT f = SplitId (Columnar f Int32) 
        deriving (Generic, Beamable)
    primaryKey = SplitId . _splitid

instance ToJSON (SplitT Identity)
instance ToJSON (PartT Identity)
instance ToJSON (PrimaryKey SplitT Identity)

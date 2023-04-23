{-# LANGUAGE 
      TypeOperators 
    , FlexibleInstances
    , MultiParamTypeClasses
    , DataKinds 
    , OverloadedStrings
    , DeriveGeneric
    , FlexibleContexts
    #-} 


module AoServer where 

import Data.Text (Text) 
import Control.Client
import Control.Plugin
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import System.IO (putStrLn) 
import Control.Concurrent 
import Control.Concurrent.Chan
import Servant.Types.SourceT
import Network.Wai.Handler.Warp 
import Servant 
import qualified Network.HTTP.Media as M
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Reader

data AoEvent = AoEvent Value

data EventStream deriving Typeable

class ToSSE a where
  toSSE :: a -> BL.ByteString

instance Accept EventStream where
  contentType _ = "text" M.// "event-stream"
instance ToSSE a => MimeRender EventStream a where
  mimeRender _ = toSSE

instance ToSSE AoEvent where 
  toSSE (AoEvent v) = BL.concat [ 
      "event: ao\ndata: "
    , encode v
    , "\n\n"]

type InFeed = "events" :> ReqBody '[JSON] Value :> Post '[JSON] NoContent

type OutFeed = "eventfeed" :> StreamGet NoFraming EventStream (SourceIO AoEvent)
-- sse :: (MonadReader Plug m) => Chan AoEvent -> m OutFeed
sse :: Chan a -> ReaderT p Handler (SourceT m a)
sse i = do 
    dup <- liftIO $ dupChan i 
    se <- liftIO . readChan $ dup 
    return $ source [se] 

type Suede = 
    InFeed :<|> 
    OutFeed :<|> 
    Raw
api :: Proxy Suede
api = Proxy

suede dist inn feed = 
    inn feed :<|> 
    sse feed :<|>
    serveDirectoryWebApp dist 

runSuede plug port path cce feed = run port $ serve api $ 
    hoistServer api (`runReaderT` plug) $ suede path cce feed


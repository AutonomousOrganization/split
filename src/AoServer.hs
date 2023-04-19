{-# LANGUAGE 
      TypeOperators 
    , FlexibleInstances
    , MultiParamTypeClasses
    , DataKinds 
    , OverloadedStrings
    , DeriveGeneric
    #-} 


module AoServer where 

import Data.Text (Text) 
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
out :: Chan AoEvent -> Server OutFeed
out i = do 
    dup <- liftIO $ dupChan i 
    se <- liftIO . readChan $ dup 
    return $ source [se] 

type Suede = 
    InFeed :<|> 
    OutFeed :<|> 
    Raw

suede dist inn feed = 
    inn feed :<|> 
    out feed :<|>
    serveDirectoryWebApp dist 

runSuede p pa cce feed = run p $ serve (Proxy :: Proxy Suede) $ suede pa cce feed
        






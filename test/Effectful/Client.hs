module Effectful.Client where

import           Data.Function
import           Data.Proxy
import           Effectful
import           Effectful.Dispatch.Dynamic
import           Effectful.Error.Static
import           Effectful.State.Static.Local
import           Network.HTTP.Client          hiding (Proxy)
import           Servant.API                  hiding ((:>))
import           Servant.Client               hiding (Client)

import           API
import           Effectful.Validation
import           Types.Semantic.Parse         (ValidationErrors)
import           Types.Transport

data Client :: Effect where
  BroadcastTx :: SignedTransaction -> Client m TxId
  GetTx       :: TxId -> Client m SignedTransaction

type instance DispatchOf Client = 'Dynamic

broadcastTx
  :: Client :> es => SignedTransaction -> Eff es TxId
broadcastTx = send . BroadcastTx

getTx :: Client :> es => TxId -> Eff es SignedTransaction
getTx = send . GetTx

runClient
  :: forall es a. (Validation [String] es, IOE :> es)
  => Int -> Manager -> Eff (Client : es) a -> Eff es a
runClient port man = interpret $ \_ -> \case
  BroadcastTx tx -> do
    eres <- runClientM' $ broadcastTx' tx
    case eres of
      Left errs -> refute [show errs]
      Right res -> pure res
  GetTx     txid -> do
    mtx <- runClientM' $ getTx' txid
    case mtx of
      Nothing -> refute ["transaction not found: " <> show txid]
      Just tx -> pure tx
  where
    runClientM' :: forall x. ClientM x -> Eff es x
    runClientM' req = do
      let
        env = mkClientEnv man (BaseUrl Http "localhost" port "")
      liftIO (runClientM req env) >>= \case
        Left err  -> refute [show err]
        Right res -> pure res

broadcastTx' :: SignedTransaction -> ClientM (Either ValidationErrors TxId)
getTx' :: TxId -> ClientM (Maybe SignedTransaction)
broadcastTx' :<|> getTx' = client (Proxy @API)

runScript
  :: Int
  -> Eff '[Client, Error (), State [String], IOE] a
  -> IO (Either [String] a)
runScript port script = do
  manager' <- liftIO $ newManager defaultManagerSettings
  script
    & runClient port manager'
    & runValidationNoCallStack
    & runEff

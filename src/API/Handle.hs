module API.Handle
  ( B.ByteString
  , handleBroadcast
  , handleGet
  , runApp
  , AppM
  ) where

import qualified Data.ByteString.Lazy as B
import           Effectful
import           Effectful.TrialChain
import           Servant              (Handler)

import           Env
import           Types.Transport

type EffectStack = '[TrialChain, IOE]

type AppM = Eff EffectStack

handleBroadcast :: SignedTransaction -> AppM TxId
handleBroadcast tx = do
  liftIO (putStrLn "broadcast got")
  appendTx tx

handleGet :: TxId -> AppM (Maybe SignedTransaction)
handleGet txid = do
  liftIO (putStrLn "it works")
  lookupTx txid

runApp :: TrialChainEnv -> AppM a -> Handler a
runApp env
      = liftIO
      . runEff
      . runTrialChainIO env

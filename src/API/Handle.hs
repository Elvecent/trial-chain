module API.Handle
  ( B.ByteString
  , handleBroadcast
  , handleGet
  , runApp
  , AppM
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy as B
import           Effectful
import           Effectful.TrialChain
import           Servant              (Handler)

import           Env
import qualified Types.Semantic       as S
import           Types.Semantic.Parse
import           Types.Transport

type EffectStack = '[TrialChain, IOE]

type AppM = Eff EffectStack

handleBroadcast :: SignedTransaction -> AppM TxId
handleBroadcast tx = do
  liftIO $ putStrLn "broadcasting transaction"
  (etx, errs) <- runValidation $ parseSignedTransaction tx
  case etx of
    Left _    -> pure undefined
    Right ptx -> appendTx ptx <&> S.transportTxId

handleGet :: TxId -> AppM (Maybe SignedTransaction)
handleGet txid = do
  liftIO $ putStrLn "retrieving transaction"
  parseTxId txid
    & lookupTx
    & mapped . _Just %~ S.transportSignedTransaction

runApp :: TrialChainEnv -> AppM a -> Handler a
runApp env
      = liftIO
      . runEff
      . runTrialChainIO env

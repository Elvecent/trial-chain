module API.Handle
  ( B.ByteString
  , handleBroadcast
  , handleGet
  , runApp
  , AppM
  , ValidationErrors
  ) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import           Effectful
import           Effectful.Concurrent
import           Effectful.TrialChain

import           Env
import qualified Types.Semantic       as S
import           Types.Semantic.Parse
import           Types.Transport

type AppM = Eff '[TrialChain, Concurrent, IOE]

handleBroadcast :: SignedTransaction -> AppM (Either ValidationErrors TxId)
handleBroadcast tx = do
  liftIO $ putStrLn "handling broadcast request"
  (etx, errs :: ValidationErrors) <-
    runValidation $ parseSignedTransaction tx
  unless (null errs) $
    liftIO $ print errs
  case etx of
    Left cs   -> liftIO $ print cs >> (pure $ Left errs)
    Right ptx -> broadcastTx ptx <&> S.transportTxId <&> Right

handleGet :: TxId -> AppM (Maybe SignedTransaction)
handleGet txid = do
  liftIO $ putStrLn "handling get request"
  parseTxId txid
    & lookupTx
    & mapped . _Just %~ S.transportSignedTransaction

runApp :: MonadIO m => TrialChainEnv -> AppM a -> m a
runApp env
      = liftIO
      . runEff
      . runConcurrent
      . runTrialChainIO env

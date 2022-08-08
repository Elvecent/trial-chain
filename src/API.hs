{-|
Module      : API
Copyright   : (c) Cyril Valyavin, 2022
License     : NONE
Maintainer  : kvalyavin@yandex.com
Stability   : experimental
-}

module API
  ( API
  , BroadcastTx
  , GetTx
  , server
  , runApp) where

import           Servant

import           API.Handle
import           Types.Transport

-- | Top-level API description
type API
    =   BroadcastTx
  :<|>  GetTx


-- | Broadcast method announces a signed transaction to the blockchain
type BroadcastTx = "broadcast"
                :> ReqBody '[JSON] SignedTransaction
                :> Post '[JSON] (Either ValidationErrors TxId)

-- | Get method retrieves a transaction by its @TxId@
type GetTx = "get"
          :> Capture "txid" TxId
          :> Get '[JSON] (Maybe SignedTransaction)

server :: ServerT API AppM
server =  handleBroadcast
     :<|> handleGet

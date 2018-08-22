-- |
-- Module      :  Network.Ethereum.Web3
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- An Ethereum node offers a RPC interface. This interface gives Ðapp’s
-- access to the Ethereum blockchain and functionality that the node provides,
-- such as compiling smart contract code. It uses a subset of the JSON-RPC 2.0
-- specification (no support for notifications or named parameters) as serialisation
-- protocol and is available over HTTP and IPC (unix domain sockets on linux/OSX
-- and named pipe’s on Windows).
--
-- Web3 Haskell library currently use JSON-RPC over HTTP to access node functionality.
--

module Network.Ethereum.Web3 (

  -- ** Monad as base of any Ethereum node communication
    Web3
  , runWeb3

  -- ** Basic transaction sending
  , sendTx
  , Call(..)

  -- ** Basic event listening
  , EventAction(..)
  , event
  , event'

  -- ** Primitive data types
  , Address
  , Bytes
  , BytesN
  , IntN
  , UIntN
  , ListN

  -- ** Metric unit system
  , module Network.Ethereum.Unit

  ) where

import           Data.Solidity.Prim.Address       (Address)
import           Data.Solidity.Prim.Bool          ()
import           Data.Solidity.Prim.Bytes         (Bytes, BytesN)
import           Data.Solidity.Prim.Int           (IntN, UIntN)
import           Data.Solidity.Prim.List          (ListN)
import           Data.Solidity.Prim.String        ()
import           Network.Ethereum.Api.Provider    (Web3, runWeb3)
import           Network.Ethereum.Api.Types       (Call (..))
import           Network.Ethereum.Contract.Event  (EventAction (..), event,
                                                   event')
import           Network.Ethereum.Contract.Method (sendTx)
import           Network.Ethereum.Unit

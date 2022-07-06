{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Data.Solidity.Prim.Tuple
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Tuple type abi encoding instances.
--

module Data.Solidity.Prim.Tuple where

import           Data.Proxy                  (Proxy (..))
import           Data.Tuple.Solo             (Solo (..))
import           Generics.SOP                (Generic)

import           Data.Solidity.Abi           (AbiGet, AbiPut, AbiType (..))
import           Data.Solidity.Abi.Generic   ()
import           Data.Solidity.Prim.Tuple.TH (tupleDecs)

instance Generic (Solo a)

instance AbiType a => AbiType (Solo a) where
    isDynamic _ = isDynamic (Proxy :: Proxy a)

instance AbiGet a => AbiGet (Solo a)
instance AbiPut a => AbiPut (Solo a)

$(concat <$> mapM tupleDecs [2..20])

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Block.Arbitrary.Message
       (
       ) where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import qualified Pos.Block.Network.Types as T
import           Pos.Core (HasGenesisHash, HasProtocolConstants)

import           Test.Pos.Block.Arbitrary ()
import           Test.Pos.Core.Chrono ()
import           Test.Pos.Ssc.Arbitrary ()
import           Test.Pos.Update.Arbitrary ()

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

instance Arbitrary T.MsgGetHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgGetBlocks where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasProtocolConstants => Arbitrary T.MsgHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (HasProtocolConstants, HasGenesisHash) => Arbitrary T.MsgBlock where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgStream where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgStreamStart where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgStreamUpdate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( HasProtocolConstants
         , HasGenesisHash
         ) =>
         Arbitrary T.MsgStreamBlock where
    arbitrary = genericArbitrary
    shrink = genericShrink

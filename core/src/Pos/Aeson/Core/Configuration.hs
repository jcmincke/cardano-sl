{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Aeson.Core.Configuration where

import           Data.Aeson (FromJSON, ToJSON, Value (..), decode,
                     genericToEncoding, pairs, parseJSON, toEncoding, (.:))
import           Data.Aeson.Encoding (pairStr)
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HMS
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
--import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Pos.Aeson.Genesis ()
import           Pos.Core.Common (SharedSeed (..))
import           Pos.Core.Configuration.Core (CoreConfiguration (..),
                     GenesisConfiguration (..))
import           Pos.Core.Genesis (GenesisAvvmBalances (..),
                     GenesisDelegation (..), GenesisInitializer (..),
                     GenesisProtocolConstants (..), GenesisSpec (..))
import           Pos.Core.Update (BlockVersionData (..))
import           Prelude
import           Serokell.Aeson.Options (defaultOptions)
instance ToJSON GenesisConfiguration where
    toEncoding (GCSrc gcsFile gcsHash) =
        pairs . pairStr "src"
            . pairs  $ pairStr "file"
                (toEncoding gcsFile) <> pairStr "hash" (toEncoding gcsHash)

    toEncoding (GCSpec value)          =
        genericToEncoding defaultOptions (GCSpec value)

instance FromJSON GenesisConfiguration where
    parseJSON (Object o)
        | HMS.member "src" o  = GCSrc <$> ((o .: "src") >>= (.: "file"))
                                      <*> ((o .: "src") >>= (.: "hash"))
        | HMS.member "spec" o = do
              -- GCSpec Object
              specO <- o .: "spec"

              -- GenesisAvvmBalances
              avvmDistrO <- specO .: "avvmDistr"

              -- SharedSeed
              ftsSeed <- specO .: "ftsSeed"

              -- GenesisDelegation
              heavyDelegationO <- specO .: "heavyDelegation"

              -- BlockVersionData
              blockVersionDataO <- specO .: "blockVersionData"
              scriptVersion <- blockVersionDataO .: "scriptVersion"
              slotDuration <- blockVersionDataO .: "slotDuration"
              maxBlockSize <- blockVersionDataO .: "maxBlockSize"
              maxHeaderSize <- blockVersionDataO .: "maxHeaderSize"
              maxTxSize <- blockVersionDataO .: "maxTxSize"
              maxProposalSize <- blockVersionDataO .: "maxProposalSize"
              mpcThd <- blockVersionDataO .: "mpcThd"
              heavyDelThd <- blockVersionDataO .: "heavyDelThd"
              updateVoteThd <- blockVersionDataO .: "updateVoteThd"
              updateProposalThd <- blockVersionDataO .: "updateProposalThd"
              updateImplicit <- blockVersionDataO .: "updateImplicit"
              softforkRuleO <- blockVersionDataO .: "softforkRule"
              txFeePolicyO <- blockVersionDataO .: "txFeePolicy"
              unlockStakeEpoch <- blockVersionDataO .: "unlockStakeEpoch"

              -- GenesisProtocolConstants
              protocolConstantsO <- specO .: "protocolConstants"
              k <- protocolConstantsO .: "k"
              protocolMagic <- protocolConstantsO .: "protocolMagic"
              vssMaxTTL <- protocolConstantsO .: "vssMaxTTL"
              vssMinTTL <- protocolConstantsO .: "vssMinTTL"
              initializerO <- specO .: "initializer"
              testBalanceO <- initializerO .: "testBalance"
              fakeAvvmBalanceO <- (initializerO .: "fakeAvvmBalance")
              avvmBalanceFactor <- initializerO .: "avvmBalanceFactor"
              useHeavyDlg <- initializerO .: "useHeavyDlg"
              seed <- initializerO .: "seed"

              return . GCSpec $
                  UnsafeGenesisSpec
                      (GenesisAvvmBalances . fromJust $ decode avvmDistrO :: GenesisAvvmBalances)
                      --(GenesisAvvmBalances . fromJust $ decode (LB.fromStrict avvmDistrO) :: GenesisAvvmBalances)
                      (SharedSeed ftsSeed)
                      (UnsafeGenesisDelegation . fromJust $ decode heavyDelegationO)
                      (BlockVersionData
                          scriptVersion
                          slotDuration
                          maxBlockSize
                          maxHeaderSize
                          maxTxSize
                          maxProposalSize
                          mpcThd
                          heavyDelThd
                          updateVoteThd
                          updateProposalThd
                          updateImplicit
                          (fromJust $ decode softforkRuleO)
                          (fromJust $ decode txFeePolicyO)
                          unlockStakeEpoch
                      )
                      (GenesisProtocolConstants k protocolMagic vssMaxTTL vssMinTTL)
                      (GenesisInitializer
                          (fromJust $ decode testBalanceO)
                          (fromJust $ decode fakeAvvmBalanceO)
                          avvmBalanceFactor
                          useHeavyDlg
                          seed)
        | otherwise = error "error"

    parseJSON invalid = typeMismatch "GenesisConfiguration" invalid
--deriveFromJSON defaultOptions ''GenesisConfiguration

--instance FromJSON GenesisConfiguration

deriveJSON defaultOptions ''CoreConfiguration
{-
"{
  \"spec\": {
    \"avvmDistr\": {
      \"VGVzdA==\": 37343863242999412
    },
    \"ftsSeed\": \"RTVTNGZTSDZldE5vdWlYZXpDeUVqS2MzdEc0amEwa0Y=\",
    \"heavyDelegation\": {
      \"df312a55373f2273a3e5db89aaefe43876f8cea2cff43fec11987e48\": {
        \"pskDelegatePk\": \"VGVzdFRlc3Q=\",
        \"pskOmega\": 683004810334740698,
        \"pskCert\": \"bae5422af5405e3803154a4ad986da5d14cf624d6701c5c78a79ec73777f74e13973af83752114d9f18166085997fc81e432cab7fee99a275d8bf138ad04e103\",
        \"pskIssuerPk\": \"VGVzdFRlc3Q=\"
      }
    },
    \"blockVersionData\": {
      \"scriptVersion\": 999,
      \"slotDuration\": 999,
      \"maxBlockSize\": 999,
      \"maxHeaderSize\": 999,
      \"maxTxSize\": 999,
      \"maxProposalSize\": 999,
      \"mpcThd\": 9.9e-14,
      \"heavyDelThd\": 9.9e-14,
      \"updateVoteThd\": 9.9e-14,
      \"updateProposalThd\": 9.9e-14,
      \"updateImplicit\": 99,
      \"softforkRule\": {
        \"initThd\": 9.9e-14,
        \"minThd\": 9.9e-14,
        \"thdDecrement\": 9.9e-14
      },
      \"txFeePolicy\": {
        \"txSizeLinear\": {
          \"a\": 9.99e-7,
          \"b\": 7.7e-8
        }
      },
      \"unlockStakeEpoch\": 99
    },
    \"protocolConstants\": {
      \"k\": 37,
      \"protocolMagic\": 1783847074,
      \"vssMaxTTL\": 1477558317,
      \"vssMinTTL\": 744040476
    },
    \"initializer\": {
      \"testBalance\": {
        \"poors\": 2448641325904532856,
        \"richmen\": 14071205313513960321,
        \"totalBalance\": 10953275486128625216,
        \"richmenShare\": 4.2098713311249885,
        \"useHDAddresses\": true
      },
      \"fakeAvvmBalance\": {
        \"count\": 17853231730478779264,
        \"oneBalance\": 15087947214890024355
      },
      \"avvmBalanceFactor\": 0.366832547637728,
      \"useHeavyDlg\": false,
      \"seed\": 0
    }
  }
}"
-}

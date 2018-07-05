module Test.Pos.Core.Json where

import qualified Cardano.Crypto.Wallet as CC
import qualified Crypto.Sign.Ed25519 as Ed25519
import           Data.Fixed
import qualified Data.HashMap.Strict as HM
import           Data.Time.Units (Millisecond)
import           Hedgehog (Property)
import qualified Hedgehog as H
import           Pos.Aeson.Core.Configuration ()
import           Pos.Binary.Class (Raw (..))
import           Pos.Core.Common (Coeff (..), Coin (..), CoinPortion (..),
                     ScriptVersion, SharedSeed (..), TxFeePolicy (..),
                     TxSizeLinear (..))
import           Pos.Core.Configuration
import           Pos.Core.Delegation (HeavyDlgIndex (..))
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                     GenesisAvvmBalances (..), GenesisDelegation (..),
                     GenesisInitializer (..), GenesisProtocolConstants (..),
                     GenesisSpec (..), TestnetBalanceOptions (..))
import           Pos.Core.ProtocolConstants (VssMaxTTL (..), VssMinTTL (..))
import           Pos.Core.Slotting (EpochIndex (..), FlatSlotId)
import           Pos.Core.Update (BlockVersionData (..), SoftforkRule (..))
import           Pos.Crypto (ProtocolMagic (..))
import           Pos.Crypto.Hashing (abstractHash)
import           Pos.Crypto.Signing (ProxyCert (..), ProxySecretKey (..),
                     PublicKey (..), RedeemPublicKey (..))
import           Serokell.Data.Memory.Units (Byte)
import           Test.Pos.Core.Bi (feedPM)
import           Test.Pos.Core.Gen (genGenesisConfiguration)
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSON)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)
import           Universum

--------------------------------------------------------------------------------
-- GensisConfiguration
--------------------------------------------------------------------------------

golden_GenesisConfiguration_GCSpec :: Property
golden_GenesisConfiguration_GCSpec = goldenTestJSON exampleGenesisConfiguration_GCSpec "test/golden/GenesisConfiguration_GCSpec"

golden_GenesisConfiguration_GCSrc :: Property
golden_GenesisConfiguration_GCSrc = goldenTestJSON exampleGenesisConfiguration_GCSrc "test/golden/GenesisConfiguration_GCSrc"

roundTripGenesisConfiguration :: Property
roundTripGenesisConfiguration = eachOf 1000 (feedPM genGenesisConfiguration) roundTripsAesonShow

exampleGenesisConfiguration_GCSrc :: GenesisConfiguration
exampleGenesisConfiguration_GCSrc =
    GCSrc "dRaMwdYsH3QA3dChe" (abstractHash (Raw "Test"))

exampleGenesisConfiguration_GCSpec :: GenesisConfiguration
exampleGenesisConfiguration_GCSpec =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances
        exampleSharedSeed
        exampleGenesisDelegation
        exampleBlockVersionData
        exampleProtocolConstants
        exampleGenesisInitializer

exampleGenesisAvvmBalances :: GenesisAvvmBalances
exampleGenesisAvvmBalances =
    GenesisAvvmBalances {getGenesisAvvmBalances =
        (HM.fromList [(RedeemPublicKey (Ed25519.PublicKey "Test")
                     , Coin {getCoin = 36524597913081152})
                     ,(RedeemPublicKey (Ed25519.PublicKey "Test")
                     ,Coin {getCoin = 37343863242999412})
                     ]) }

exampleSharedSeed :: SharedSeed
exampleSharedSeed = SharedSeed (getBytes 8 32)

exampleGenesisDelegation :: GenesisDelegation
exampleGenesisDelegation = UnsafeGenesisDelegation {unGenesisDelegation = HM.fromList
    [(abstractHash (PublicKey (CC.XPub {CC.xpubPublicKey = "Test"
    , CC.xpubChaincode = CC.ChainCode "Test"}))
    , UnsafeProxySecretKey {pskOmega = HeavyDlgIndex $ EpochIndex 683004810334740698
    , pskIssuerPk = PublicKey (CC.XPub {CC.xpubPublicKey = "Test"
    , CC.xpubChaincode = CC.ChainCode "Test"})
    , pskDelegatePk = PublicKey (CC.XPub {CC.xpubPublicKey = "Test"
    , CC.xpubChaincode = CC.ChainCode "Test"})
    , pskCert = ProxyCert (fromRight (error "Something went wrong") $ sig)})]}
  where
    sig = CC.xsignature "\186\229B*\245@^8\ETX\NAKJJ\217\134\218]\DC4\207\
                        \bMg\SOH\197\199\138y\236sw\DELt\225\&9s\175\131\
                        \u!\DC4\217\241\129f\bY\151\252\129\228\&\
                        \2\202\183\254\233\154']\139\241\&8\173\EOT\225\ETX"

exampleBlockVersionData :: BlockVersionData
exampleBlockVersionData = BlockVersionData
                              (999 :: ScriptVersion)
                              (999 :: Millisecond)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (99 :: FlatSlotId)
                              sfrule
                              (TxFeePolicyTxSizeLinear tslin)
                              (EpochIndex 99)
    where
        tslin = TxSizeLinear c1' c2'
        c1' = Coeff (MkFixed 999)
        c2' = Coeff (MkFixed 77)
        sfrule = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))

exampleProtocolConstants :: GenesisProtocolConstants
exampleProtocolConstants = GenesisProtocolConstants
    { gpcK = 37
    , gpcProtocolMagic = ProtocolMagic {getProtocolMagic = 1783847074}
    , gpcVssMaxTTL = VssMaxTTL {getVssMaxTTL = 1477558317}
    , gpcVssMinTTL = VssMinTTL {getVssMinTTL = 744040476}}

exampleGenesisInitializer :: GenesisInitializer
exampleGenesisInitializer = GenesisInitializer
    {giTestBalance = TestnetBalanceOptions
        {tboPoors = 2448641325904532856
        , tboRichmen = 14071205313513960321
        , tboTotalBalance = 10953275486128625216
        , tboRichmenShare = 4.2098713311249885
        , tboUseHDAddresses = True}
        , giFakeAvvmBalance = FakeAvvmOptions
            {faoCount = 17853231730478779264
            , faoOneBalance = 15087947214890024355}
            , giAvvmBalanceFactor = CoinPortion
                 {getCoinPortion = 366832547637728}
                 , giUseHeavyDlg = False
                 , giSeed = 0}


tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip

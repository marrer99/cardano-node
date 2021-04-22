{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


-- | Transaction bodies
--
module Cardano.Api.TxBody (

    -- * Transaction bodies
    TxBody(..),
    makeTransactionBody,
    TxBodyContent(..),
    TxBodyError(..),

    -- ** Transitional utils
    makeByronTransaction,

    -- * Transaction Ids
    TxId(..),
    getTxId,

    -- * Transaction inputs
    TxIn(..),
    TxIx(..),
    genesisUTxOPseudoTxIn,

    -- * Transaction outputs
    TxOut(..),
    TxOutDatumHash(..),
    TxOutValue(..),

    -- * Other transaction body types
    TxFee(..),
    TxValidityLowerBound(..),
    TxValidityUpperBound(..),
    TxMetadataInEra(..),
    TxAuxScripts(..),
    TxWithdrawals(..),
    TxCertificates(..),
    TxUpdateProposal(..),
    TxMintValue(..),
    TxWitnessPPDataHash(..),
    Datum(..),

    -- ** Building vs viewing transactions
    BuildTxWith(..),
    BuildTx,
    ViewTx,

    -- * Era-dependent transaction body features
    MultiAssetSupportedInEra(..),
    OnlyAdaSupportedInEra(..),
    TxFeesExplicitInEra(..),
    TxFeesImplicitInEra(..),
    ValidityUpperBoundSupportedInEra(..),
    ValidityNoUpperBoundSupportedInEra(..),
    ValidityLowerBoundSupportedInEra(..),
    TxMetadataSupportedInEra(..),
    AuxScriptsSupportedInEra(..),
    WithdrawalsSupportedInEra(..),
    CertificatesSupportedInEra(..),
    UpdateProposalSupportedInEra(..),
    WitnessPPDataSupportedInEra(..),
    PlutusScriptsSupportedInEra(..),

    -- ** Feature availability functions
    multiAssetSupportedInEra,
    txFeesExplicitInEra,
    validityUpperBoundSupportedInEra,
    validityNoUpperBoundSupportedInEra,
    validityLowerBoundSupportedInEra,
    txMetadataSupportedInEra,
    auxScriptsSupportedInEra,
    withdrawalsSupportedInEra,
    certificatesSupportedInEra,
    updateProposalSupportedInEra,
    plutusScriptsSupportedInEra,

    -- * Internal conversion functions & types
    toShelleyTxId,
    toShelleyTxIn,
    toShelleyTxOut,
    fromShelleyTxId,
    fromShelleyTxIn,
    fromShelleyTxOut,
    fromTxOut,

    -- * Data family instances
    AsType(AsTxId, AsTxBody, AsByronTxBody, AsShelleyTxBody, AsMaryTxBody),

    -- * Conversion functions
    fromByronTxIn,

    -- * Other Tx Body helper functions
    makeTxWitnessPPDataHash,
  ) where

import           Prelude

import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (findIndex, intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           GHC.Generics

import           Control.Monad (guard)

import           Cardano.Binary (Annotated (..), reAnnotate, recoverBytes)
import qualified Cardano.Binary as CBOR
import qualified Shelley.Spec.Ledger.Serialization as CBOR (decodeNullMaybe, encodeNullMaybe)

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hashing as Byron

import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Ledger (hashAuxiliaryData)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.Constraints as Ledger
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as Allegra
import qualified Cardano.Ledger.ShelleyMA.TxBody as Allegra
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardAlonzo, StandardMary,
                   StandardShelley)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)

import qualified Shelley.Spec.Ledger.Address as Shelley
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe)
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Metadata as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Script
import           Cardano.Api.SerialiseBech32
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value


-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Shelley.Hash StandardCrypto Shelley.EraIndependentTxBody)
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)
               -- We use the Shelley representation and convert the Byron one

instance ToJSON TxId where
  toJSON = Aeson.String . serialiseToRawBytesHexText

instance HasTypeProxy TxId where
    data AsType TxId = AsTxId
    proxyToAsType _ = AsTxId

instance SerialiseAsRawBytes TxId where
    serialiseToRawBytes (TxId h) = Crypto.hashToBytes h
    deserialiseFromRawBytes AsTxId bs = TxId <$> Crypto.hashFromBytes bs

toByronTxId :: TxId -> Byron.TxId
toByronTxId (TxId h) =
    Byron.unsafeHashFromBytes (Crypto.hashToBytes h)

toShelleyTxId :: TxId -> Shelley.TxId StandardCrypto
toShelleyTxId (TxId h) =
    Shelley.TxId (SafeHash.unsafeMakeSafeHash (Crypto.castHash h))

fromShelleyTxId :: Shelley.TxId StandardCrypto -> TxId
fromShelleyTxId (Shelley.TxId h) =
    TxId (Crypto.castHash (SafeHash.extractHash h))

-- | Calculate the transaction identifier for a 'TxBody'.
--
getTxId :: forall era. TxBody era -> TxId
getTxId (ByronTxBody tx) =
    TxId
  . fromMaybe impossible
  . Crypto.hashFromBytesShort
  . Byron.abstractHashToShort
  . Byron.hashDecoded
  $ tx
  where
    impossible =
      error "getTxId: byron and shelley hash sizes do not match"

getTxId (ShelleyTxBody era tx _ _ _) =
    case era of
      ShelleyBasedEraShelley -> getTxIdShelley tx
      ShelleyBasedEraAllegra -> getTxIdShelley tx
      ShelleyBasedEraMary    -> getTxIdShelley tx
      ShelleyBasedEraAlonzo  -> getTxIdShelley tx
  where
    getTxIdShelley :: Ledger.Crypto (ShelleyLedgerEra era) ~ StandardCrypto
                   => Ledger.UsesTxBody (ShelleyLedgerEra era)
                   => Ledger.TxBody (ShelleyLedgerEra era) -> TxId
    getTxIdShelley =
        TxId
      . Crypto.castHash
      . (\(Shelley.TxId txhash) -> SafeHash.extractHash txhash)
      . (Shelley.txid @(ShelleyLedgerEra era))


-- ----------------------------------------------------------------------------
-- Transaction inputs
--

data TxIn where
  TxIn :: TxId -> TxIx -> TxIn

deriving instance Eq   TxIn
deriving instance Ord  TxIn
deriving instance Show TxIn

data PlutusScriptsSupportedInEra era where

     PlutusScriptsInAlonzoEra :: PlutusScriptsSupportedInEra AlonzoEra

deriving instance Eq   (PlutusScriptsSupportedInEra era)
deriving instance Ord  (PlutusScriptsSupportedInEra era)
deriving instance Show (PlutusScriptsSupportedInEra era)

plutusScriptsSupportedInEra :: CardanoEra era
                            -> Maybe (PlutusScriptsSupportedInEra era)
plutusScriptsSupportedInEra ByronEra   = Nothing
plutusScriptsSupportedInEra ShelleyEra = Nothing
plutusScriptsSupportedInEra AllegraEra = Nothing
plutusScriptsSupportedInEra MaryEra    = Nothing
plutusScriptsSupportedInEra AlonzoEra  = Just PlutusScriptsInAlonzoEra

instance ToJSON TxIn where
  toJSON txIn = Aeson.String $ renderTxIn txIn

instance ToJSONKey TxIn where
  toJSONKey = toJSONKeyText renderTxIn

renderTxIn :: TxIn -> Text
renderTxIn (TxIn txId (TxIx ix)) =
  serialiseToRawBytesHexText txId <> "#" <> Text.pack (show ix)


newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)
  deriving newtype ToJSON

fromByronTxIn :: Byron.TxIn -> TxIn
fromByronTxIn (Byron.TxInUtxo txId index) =
  let shortBs = Byron.abstractHashToShort txId
      mApiHash = Crypto.hashFromBytesShort shortBs
  in case mApiHash of
       Just apiHash -> TxIn (TxId apiHash) (TxIx . fromIntegral $ toInteger index)
       Nothing -> error $ "Error converting Byron era TxId: " <> show txId

toByronTxIn :: TxIn -> Byron.TxIn
toByronTxIn (TxIn txid (TxIx txix)) =
    Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

toShelleyTxIn :: TxIn -> Shelley.TxIn StandardCrypto
toShelleyTxIn (TxIn txid (TxIx txix)) =
    Shelley.TxIn (toShelleyTxId txid) (fromIntegral txix)

fromShelleyTxIn :: Shelley.TxIn StandardCrypto -> TxIn
fromShelleyTxIn (Shelley.TxIn txid txix) =
    TxIn (fromShelleyTxId txid) (TxIx (fromIntegral txix))


-- ----------------------------------------------------------------------------
-- Transaction outputs
--

data TxOut era
  = TxOut (AddressInEra era) (TxOutValue era) (TxOutDatumHash era)
  deriving Generic

instance IsCardanoEra era => ToJSON (TxOut era) where
  toJSON (TxOut (AddressInEra addrType addr) val _datHash) =
    case addrType of
      ByronAddressInAnyEra ->
        let hexAddr = serialiseToRawBytesHexText addr
        in object [ "address" .= hexAddr
                  , "value" .= toJSON val
                  ]
      ShelleyAddressInEra sbe ->
        case sbe of
          ShelleyBasedEraShelley ->
            object
              [ "address" .= serialiseToBech32 addr
              , "value" .= toJSON val
              ]
          ShelleyBasedEraAllegra ->
            object
              [ "address" .= serialiseToBech32 addr
              , "value" .= toJSON val
              ]
          ShelleyBasedEraMary ->
            object
              [ "address" .= serialiseToBech32 addr
              , "value" .= toJSON val
              ]

          ShelleyBasedEraAlonzo ->
            object [ "address" .= serialiseToBech32 addr
                   , "value" .= toJSON val
                   , "dataHash" .= Aeson.Null --TODO:JORDAN FILL IN
                   ]


deriving instance Eq   (TxOut era)
deriving instance Show (TxOut era)

toByronTxOut :: TxOut ByronEra -> Maybe Byron.TxOut
toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress addr))
                    (TxOutAdaOnly AdaOnlyInByronEra value) _) =
    Byron.TxOut addr <$> toByronLovelace value

toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
                    (TxOutValue era _)_ ) = case era of {}

toByronTxOut (TxOut (AddressInEra (ShelleyAddressInEra era) ShelleyAddress{})
                    _ _) = case era of {}

toShelleyTxOut :: forall era ledgerera.
                 (ShelleyLedgerEra era ~ ledgerera,
                  IsShelleyBasedEra era, Ledger.ShelleyBased ledgerera)
               => TxOut era -> Core.TxOut ledgerera
toShelleyTxOut (TxOut _ (TxOutAdaOnly AdaOnlyInByronEra _) _) =
    case shelleyBasedEra :: ShelleyBasedEra era of {}

toShelleyTxOut (TxOut addr (TxOutAdaOnly AdaOnlyInShelleyEra value) _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut (TxOut addr (TxOutAdaOnly AdaOnlyInAllegraEra value) _) =
    Shelley.TxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut (TxOut addr (TxOutValue MultiAssetInMaryEra value) _) =
    Shelley.TxOut (toShelleyAddr addr) (toMaryValue value)

toShelleyTxOut (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) _mDatHash) =
    Alonzo.TxOut (toShelleyAddr addr) (toMaryValue value) (error "DataHash")

fromShelleyTxOut :: Shelley.TxOut StandardShelley -> TxOut ShelleyEra
fromShelleyTxOut = fromTxOut ShelleyBasedEraShelley

fromTxOut
  :: ShelleyLedgerEra era ~ ledgerera
  => Ledger.Crypto ledgerera ~ StandardCrypto
  => ShelleyBasedEra era
  -> Core.TxOut ledgerera
  -> TxOut era
fromTxOut shelleyBasedEra' ledgerTxOut =
  case shelleyBasedEra' of
    ShelleyBasedEraShelley -> let (Shelley.TxOut addr value) = ledgerTxOut
                              in TxOut (fromShelleyAddr addr)
                                       (TxOutAdaOnly AdaOnlyInShelleyEra
                                                      (fromShelleyLovelace value))
                                                      NoDatumHash
    ShelleyBasedEraAllegra -> let (Shelley.TxOut addr value) = ledgerTxOut
                              in TxOut (fromShelleyAddr addr)
                                       (TxOutAdaOnly AdaOnlyInAllegraEra
                                                     (fromShelleyLovelace value))
                                                      NoDatumHash
    ShelleyBasedEraMary    -> let (Shelley.TxOut addr value) = ledgerTxOut
                              in TxOut (fromShelleyAddr addr)
                                       (TxOutValue MultiAssetInMaryEra
                                                     (fromMaryValue value))
                                                      NoDatumHash

    ShelleyBasedEraAlonzo  -> let (Alonzo.TxOut addr value _mDhash) = ledgerTxOut
                              in TxOut (fromShelleyAddr addr)
                                       (TxOutValue MultiAssetInAlonzoEra
                                                     (fromMaryValue value))
                                                      NoDatumHash --TODO: Update

-- ----------------------------------------------------------------------------
-- Era-dependent transaction body features
--

-- | A representation of whether the era supports multi-asset transactions.
--
-- The Mary and subsequent eras support multi-asset transactions.
--
-- The negation of this is 'OnlyAdaSupportedInEra'.
--
data MultiAssetSupportedInEra era where

     -- | Multi-asset transactions are supported in the 'Mary' era.
     MultiAssetInMaryEra   :: MultiAssetSupportedInEra MaryEra
     MultiAssetInAlonzoEra :: MultiAssetSupportedInEra AlonzoEra

deriving instance Eq   (MultiAssetSupportedInEra era)
deriving instance Show (MultiAssetSupportedInEra era)

instance ToJSON (MultiAssetSupportedInEra era) where
  toJSON = Aeson.String . Text.pack . show

-- | A representation of whether the era supports only ada transactions.
--
-- Prior to the Mary era only ada transactions are supported. Multi-assets are
-- supported from the Mary era onwards.
--
-- This is the negation of 'MultiAssetSupportedInEra'. It exists since we need
-- evidence to be positive.
--
data OnlyAdaSupportedInEra era where

     AdaOnlyInByronEra   :: OnlyAdaSupportedInEra ByronEra
     AdaOnlyInShelleyEra :: OnlyAdaSupportedInEra ShelleyEra
     AdaOnlyInAllegraEra :: OnlyAdaSupportedInEra AllegraEra

deriving instance Eq   (OnlyAdaSupportedInEra era)
deriving instance Show (OnlyAdaSupportedInEra era)

multiAssetSupportedInEra :: CardanoEra era
                         -> Either (OnlyAdaSupportedInEra era)
                                   (MultiAssetSupportedInEra era)
multiAssetSupportedInEra ByronEra   = Left AdaOnlyInByronEra
multiAssetSupportedInEra ShelleyEra = Left AdaOnlyInShelleyEra
multiAssetSupportedInEra AllegraEra = Left AdaOnlyInAllegraEra
multiAssetSupportedInEra MaryEra    = Right MultiAssetInMaryEra
multiAssetSupportedInEra AlonzoEra  = Right MultiAssetInAlonzoEra


-- | A representation of whether the era requires explicitly specified fees in
-- transactions.
--
-- The Byron era tx fees are implicit (as the difference bettween the sum of
-- outputs and sum of inputs), but all later eras the fees are specified in the
-- transaction explicitly.
--
data TxFeesExplicitInEra era where

     TxFeesExplicitInShelleyEra :: TxFeesExplicitInEra ShelleyEra
     TxFeesExplicitInAllegraEra :: TxFeesExplicitInEra AllegraEra
     TxFeesExplicitInMaryEra    :: TxFeesExplicitInEra MaryEra
     TxFeesExplicitInAlonzoEra  :: TxFeesExplicitInEra AlonzoEra

deriving instance Eq   (TxFeesExplicitInEra era)
deriving instance Show (TxFeesExplicitInEra era)

-- | A representation of whether the era requires implicitly specified fees in
-- transactions.
--
-- This is the negation of 'TxFeesExplicitInEra'.
--
data TxFeesImplicitInEra era where
     TxFeesImplicitInByronEra :: TxFeesImplicitInEra ByronEra

deriving instance Eq   (TxFeesImplicitInEra era)
deriving instance Show (TxFeesImplicitInEra era)

txFeesExplicitInEra :: CardanoEra era
                    -> Either (TxFeesImplicitInEra era)
                              (TxFeesExplicitInEra era)
txFeesExplicitInEra ByronEra   = Left  TxFeesImplicitInByronEra
txFeesExplicitInEra ShelleyEra = Right TxFeesExplicitInShelleyEra
txFeesExplicitInEra AllegraEra = Right TxFeesExplicitInAllegraEra
txFeesExplicitInEra MaryEra    = Right TxFeesExplicitInMaryEra
txFeesExplicitInEra AlonzoEra  = Right TxFeesExplicitInAlonzoEra


-- | A representation of whether the era supports transactions with an upper
-- bound on the range of slots in which they are valid.
--
-- The Shelley and subsequent eras support an upper bound on the validity
-- range. In the Shelley era specifically it is actually required. It is
-- optional in later eras.
--
data ValidityUpperBoundSupportedInEra era where

     ValidityUpperBoundInShelleyEra :: ValidityUpperBoundSupportedInEra ShelleyEra
     ValidityUpperBoundInAllegraEra :: ValidityUpperBoundSupportedInEra AllegraEra
     ValidityUpperBoundInMaryEra    :: ValidityUpperBoundSupportedInEra MaryEra
     ValidityUpperBoundInAlonzoEra  :: ValidityUpperBoundSupportedInEra AlonzoEra

deriving instance Eq   (ValidityUpperBoundSupportedInEra era)
deriving instance Show (ValidityUpperBoundSupportedInEra era)

validityUpperBoundSupportedInEra :: CardanoEra era
                                 -> Maybe (ValidityUpperBoundSupportedInEra era)
validityUpperBoundSupportedInEra ByronEra   = Nothing
validityUpperBoundSupportedInEra ShelleyEra = Just ValidityUpperBoundInShelleyEra
validityUpperBoundSupportedInEra AllegraEra = Just ValidityUpperBoundInAllegraEra
validityUpperBoundSupportedInEra MaryEra    = Just ValidityUpperBoundInMaryEra
validityUpperBoundSupportedInEra AlonzoEra  = Just ValidityUpperBoundInAlonzoEra


-- | A representation of whether the era supports transactions having /no/
-- upper bound on the range of slots in which they are valid.
--
-- Note that the 'ShelleyEra' /does not support/ omitting a validity upper
-- bound. It was introduced as a /required/ field in Shelley and then made
-- optional in Allegra and subsequent eras.
--
-- The Byron era supports this by virtue of the fact that it does not support
-- validity ranges at all.
--
data ValidityNoUpperBoundSupportedInEra era where

     ValidityNoUpperBoundInByronEra   :: ValidityNoUpperBoundSupportedInEra ByronEra
     ValidityNoUpperBoundInAllegraEra :: ValidityNoUpperBoundSupportedInEra AllegraEra
     ValidityNoUpperBoundInMaryEra    :: ValidityNoUpperBoundSupportedInEra MaryEra
     ValidityNoUpperBoundInAlonzoEra  :: ValidityNoUpperBoundSupportedInEra AlonzoEra

deriving instance Eq   (ValidityNoUpperBoundSupportedInEra era)
deriving instance Show (ValidityNoUpperBoundSupportedInEra era)

validityNoUpperBoundSupportedInEra :: CardanoEra era
                                   -> Maybe (ValidityNoUpperBoundSupportedInEra era)
validityNoUpperBoundSupportedInEra ByronEra   = Just ValidityNoUpperBoundInByronEra
validityNoUpperBoundSupportedInEra ShelleyEra = Nothing
validityNoUpperBoundSupportedInEra AllegraEra = Just ValidityNoUpperBoundInAllegraEra
validityNoUpperBoundSupportedInEra MaryEra    = Just ValidityNoUpperBoundInMaryEra
validityNoUpperBoundSupportedInEra AlonzoEra  = Just ValidityNoUpperBoundInAlonzoEra


-- | A representation of whether the era supports transactions with a lower
-- bound on the range of slots in which they are valid.
--
-- The Allegra and subsequent eras support an optional lower bound on the
-- validity range. No equivalent of 'ValidityNoUpperBoundSupportedInEra' is
-- needed since all eras support having no lower bound.
--
data ValidityLowerBoundSupportedInEra era where

     ValidityLowerBoundInAllegraEra :: ValidityLowerBoundSupportedInEra AllegraEra
     ValidityLowerBoundInMaryEra    :: ValidityLowerBoundSupportedInEra MaryEra
     ValidityLowerBoundInAlonzoEra  :: ValidityLowerBoundSupportedInEra AlonzoEra

deriving instance Eq   (ValidityLowerBoundSupportedInEra era)
deriving instance Show (ValidityLowerBoundSupportedInEra era)

validityLowerBoundSupportedInEra :: CardanoEra era
                                 -> Maybe (ValidityLowerBoundSupportedInEra era)
validityLowerBoundSupportedInEra ByronEra   = Nothing
validityLowerBoundSupportedInEra ShelleyEra = Nothing
validityLowerBoundSupportedInEra AllegraEra = Just ValidityLowerBoundInAllegraEra
validityLowerBoundSupportedInEra MaryEra    = Just ValidityLowerBoundInMaryEra
validityLowerBoundSupportedInEra AlonzoEra  = Just ValidityLowerBoundInAlonzoEra


-- | A representation of whether the era supports transaction metadata.
--
-- Transaction metadata is supported from the Shelley era onwards.
--
data TxMetadataSupportedInEra era where

     TxMetadataInShelleyEra :: TxMetadataSupportedInEra ShelleyEra
     TxMetadataInAllegraEra :: TxMetadataSupportedInEra AllegraEra
     TxMetadataInMaryEra    :: TxMetadataSupportedInEra MaryEra
     TxMetadataInAlonzoEra  :: TxMetadataSupportedInEra AlonzoEra

deriving instance Eq   (TxMetadataSupportedInEra era)
deriving instance Show (TxMetadataSupportedInEra era)

txMetadataSupportedInEra :: CardanoEra era
                         -> Maybe (TxMetadataSupportedInEra era)
txMetadataSupportedInEra ByronEra   = Nothing
txMetadataSupportedInEra ShelleyEra = Just TxMetadataInShelleyEra
txMetadataSupportedInEra AllegraEra = Just TxMetadataInAllegraEra
txMetadataSupportedInEra MaryEra    = Just TxMetadataInMaryEra
txMetadataSupportedInEra AlonzoEra  = Just TxMetadataInAlonzoEra


-- | A representation of whether the era supports auxiliary scripts in
-- transactions.
--
-- Auxiliary scripts are supported from the Allegra era onwards.
--
data AuxScriptsSupportedInEra era where

     AuxScriptsInAllegraEra :: AuxScriptsSupportedInEra AllegraEra
     AuxScriptsInMaryEra    :: AuxScriptsSupportedInEra MaryEra
     AuxScriptsInAlonzoEra  :: AuxScriptsSupportedInEra AlonzoEra

deriving instance Eq   (AuxScriptsSupportedInEra era)
deriving instance Show (AuxScriptsSupportedInEra era)

auxScriptsSupportedInEra :: CardanoEra era
                         -> Maybe (AuxScriptsSupportedInEra era)
auxScriptsSupportedInEra ByronEra   = Nothing
auxScriptsSupportedInEra ShelleyEra = Nothing
auxScriptsSupportedInEra AllegraEra = Just AuxScriptsInAllegraEra
auxScriptsSupportedInEra MaryEra    = Just AuxScriptsInMaryEra
auxScriptsSupportedInEra AlonzoEra  = Just AuxScriptsInAlonzoEra


-- | A representation of whether the era supports withdrawals from reward
-- accounts.
--
-- The Shelley and subsequent eras support stake addresses, their associated
-- reward accounts and support for withdrawals from them.
--
data WithdrawalsSupportedInEra era where

     WithdrawalsInShelleyEra :: WithdrawalsSupportedInEra ShelleyEra
     WithdrawalsInAllegraEra :: WithdrawalsSupportedInEra AllegraEra
     WithdrawalsInMaryEra    :: WithdrawalsSupportedInEra MaryEra
     WithdrawalsInAlonzoEra  :: WithdrawalsSupportedInEra AlonzoEra

deriving instance Eq   (WithdrawalsSupportedInEra era)
deriving instance Show (WithdrawalsSupportedInEra era)

withdrawalsSupportedInEra :: CardanoEra era
                          -> Maybe (WithdrawalsSupportedInEra era)
withdrawalsSupportedInEra ByronEra   = Nothing
withdrawalsSupportedInEra ShelleyEra = Just WithdrawalsInShelleyEra
withdrawalsSupportedInEra AllegraEra = Just WithdrawalsInAllegraEra
withdrawalsSupportedInEra MaryEra    = Just WithdrawalsInMaryEra
withdrawalsSupportedInEra AlonzoEra  = Just WithdrawalsInAlonzoEra


-- | A representation of whether the era supports 'Certificate's embedded in
-- transactions.
--
-- The Shelley and subsequent eras support such certificates.
--
data CertificatesSupportedInEra era where

     CertificatesInShelleyEra :: CertificatesSupportedInEra ShelleyEra
     CertificatesInAllegraEra :: CertificatesSupportedInEra AllegraEra
     CertificatesInMaryEra    :: CertificatesSupportedInEra MaryEra
     CertificatesInAlonzoEra  :: CertificatesSupportedInEra AlonzoEra

deriving instance Eq   (CertificatesSupportedInEra era)
deriving instance Show (CertificatesSupportedInEra era)

certificatesSupportedInEra :: CardanoEra era
                           -> Maybe (CertificatesSupportedInEra era)
certificatesSupportedInEra ByronEra   = Nothing
certificatesSupportedInEra ShelleyEra = Just CertificatesInShelleyEra
certificatesSupportedInEra AllegraEra = Just CertificatesInAllegraEra
certificatesSupportedInEra MaryEra    = Just CertificatesInMaryEra
certificatesSupportedInEra AlonzoEra  = Just CertificatesInAlonzoEra


-- | A representation of whether the era supports 'UpdateProposal's embedded in
-- transactions.
--
-- The Shelley and subsequent eras support such update proposals. They Byron
-- era has a notion of an update proposal, but it is a standalone chain object
-- and not embedded in a transaction.
--
data UpdateProposalSupportedInEra era where

     UpdateProposalInShelleyEra :: UpdateProposalSupportedInEra ShelleyEra
     UpdateProposalInAllegraEra :: UpdateProposalSupportedInEra AllegraEra
     UpdateProposalInMaryEra    :: UpdateProposalSupportedInEra MaryEra
     UpdateProposalInAlonzoEra  :: UpdateProposalSupportedInEra AlonzoEra

deriving instance Eq   (UpdateProposalSupportedInEra era)
deriving instance Show (UpdateProposalSupportedInEra era)

updateProposalSupportedInEra :: CardanoEra era
                             -> Maybe (UpdateProposalSupportedInEra era)
updateProposalSupportedInEra ByronEra   = Nothing
updateProposalSupportedInEra ShelleyEra = Just UpdateProposalInShelleyEra
updateProposalSupportedInEra AllegraEra = Just UpdateProposalInAllegraEra
updateProposalSupportedInEra MaryEra    = Just UpdateProposalInMaryEra
updateProposalSupportedInEra AlonzoEra  = Just UpdateProposalInAlonzoEra


-- ----------------------------------------------------------------------------
-- Building vs viewing transactions
--

data BuildTx
data ViewTx

data BuildTxWith build a where

     ViewTx      ::      BuildTxWith ViewTx  a
     BuildTxWith :: a -> BuildTxWith BuildTx a

deriving instance Eq   a => Eq   (BuildTxWith build a)
deriving instance Show a => Show (BuildTxWith build a)


-- ----------------------------------------------------------------------------
-- Transaction output values (era-dependent)
--

data TxOutValue era where

     TxOutAdaOnly :: OnlyAdaSupportedInEra era -> Lovelace -> TxOutValue era

     TxOutValue   :: MultiAssetSupportedInEra era -> Value -> TxOutValue era

deriving instance Eq   (TxOutValue era)
deriving instance Show (TxOutValue era)
deriving instance Generic (TxOutValue era)

instance ToJSON (TxOutValue era) where
  toJSON (TxOutAdaOnly _ ll) = toJSON ll
  toJSON (TxOutValue _ val) = toJSON val

-- ----------------------------------------------------------------------------
-- Transaction output data hash (era-dependent)
--

data TxOutDatumHash era where
     NoDatumHash :: TxOutDatumHash era
     DatumHash   :: TxOutDatumsSupportedInEra era
                 -> Hash Datum
                 -> TxOutDatumHash era

-- TODO: Update appropriately
newtype instance Hash Datum = HashDatum ()
  deriving (Show, Eq)

deriving instance Show (TxOutDatumHash era)
deriving instance Eq (TxOutDatumHash era)

data TxOutDatumsSupportedInEra era where
  TxOutDatumsSupportedInAlonzoEra :: TxOutDatumsSupportedInEra AlonzoEra

-- Placeholder for Data era
newtype Datum = Datum ()

deriving instance Eq   (TxOutDatumsSupportedInEra era)
deriving instance Ord  (TxOutDatumsSupportedInEra era)
deriving instance Show (TxOutDatumsSupportedInEra era)
-- -------------------------------------------------------
---------------------
-- Transaction fees
--

data TxFee era where

     TxFeeImplicit :: TxFeesImplicitInEra era -> TxFee era

     TxFeeExplicit :: TxFeesExplicitInEra era -> Lovelace -> TxFee era

deriving instance Eq   (TxFee era)
deriving instance Show (TxFee era)


-- ----------------------------------------------------------------------------
-- Transaction validity range
--

-- | This was formerly known as the TTL.
--
data TxValidityUpperBound era where

     TxValidityNoUpperBound :: ValidityNoUpperBoundSupportedInEra era
                            -> TxValidityUpperBound era

     TxValidityUpperBound   :: ValidityUpperBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityUpperBound era

deriving instance Eq   (TxValidityUpperBound era)
deriving instance Show (TxValidityUpperBound era)


data TxValidityLowerBound era where

     TxValidityNoLowerBound :: TxValidityLowerBound era

     TxValidityLowerBound   :: ValidityLowerBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityLowerBound era

deriving instance Eq   (TxValidityLowerBound era)
deriving instance Show (TxValidityLowerBound era)


-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxMetadataInEra era where

     TxMetadataNone  :: TxMetadataInEra era

     TxMetadataInEra :: TxMetadataSupportedInEra era
                     -> TxMetadata
                     -> TxMetadataInEra era

deriving instance Eq   (TxMetadataInEra era)
deriving instance Show (TxMetadataInEra era)


-- ----------------------------------------------------------------------------
-- Auxiliary scripts (era-dependent)
--

data TxAuxScripts era where

     TxAuxScriptsNone :: TxAuxScripts era

     TxAuxScripts     :: AuxScriptsSupportedInEra era
                      -> [ScriptInEra era]
                      -> TxAuxScripts era

deriving instance Eq   (TxAuxScripts era)
deriving instance Show (TxAuxScripts era)


-- ----------------------------------------------------------------------------
-- Withdrawals within transactions (era-dependent)
--

data TxWithdrawals build era where

     TxWithdrawalsNone :: TxWithdrawals build era

     TxWithdrawals     :: WithdrawalsSupportedInEra era
                       -> [(StakeAddress, Lovelace,
                            BuildTxWith build (Witness WitCtxStake era))]
                       -> TxWithdrawals build era

deriving instance Eq   (TxWithdrawals build era)
deriving instance Show (TxWithdrawals build era)


-- ----------------------------------------------------------------------------
-- Certificates within transactions (era-dependent)
--

data TxCertificates build era where

     TxCertificatesNone :: TxCertificates build era

     TxCertificates     :: CertificatesSupportedInEra era
                        -> [Certificate]
                        -> BuildTxWith build
                             (Map StakeCredential (Witness WitCtxStake era))
                        -> TxCertificates build era

deriving instance Eq   (TxCertificates build era)
deriving instance Show (TxCertificates build era)


-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxUpdateProposal era where

     TxUpdateProposalNone :: TxUpdateProposal era

     TxUpdateProposal     :: UpdateProposalSupportedInEra era
                          -> UpdateProposal
                          -> TxUpdateProposal era

deriving instance Eq   (TxUpdateProposal era)
deriving instance Show (TxUpdateProposal era)


-- ----------------------------------------------------------------------------
-- Value minting within transactions (era-dependent)
--

data TxMintValue build era where

     TxMintNone  :: TxMintValue build era

     TxMintValue :: MultiAssetSupportedInEra era
                 -> Value
                 -> BuildTxWith build (Map PolicyId (Witness WitCtxMint era))
                 -> TxMintValue build era

deriving instance Eq   (TxMintValue build era)
deriving instance Show (TxMintValue build era)


data TxPlutusWitnesses build era where
    TxPlutusWitnessesNone :: TxPlutusWitnesses build era
    TxPlutusWitnesses      :: TxPlutusWitnessesSupportedInEra era
                           -> [BuildTxWith build (Witness WitCtxPlutus era)]
                           -> TxPlutusWitnesses build era

deriving instance Eq   (TxPlutusWitnesses build era)
deriving instance Show (TxPlutusWitnesses build era)

data TxPlutusWitnessesSupportedInEra era where
    TxPlutusWitnessesSupportedInAlonzoEra :: TxPlutusWitnessesSupportedInEra AlonzoEra

deriving instance Eq   (TxPlutusWitnessesSupportedInEra era)
deriving instance Show (TxPlutusWitnessesSupportedInEra era)

-- ----------------------------------------------------------------------------
-- Data necessary to create a hash of the script execution data.
--
data TxWitnessPPDataHash era where
    TxWitnessPPDataHashNone :: TxWitnessPPDataHash era
    TxWitnessPPDataHash     :: WitnessPPDataSupportedInEra era
                            -> StrictMaybe (Alonzo.WitnessPPDataHash StandardCrypto)
                            -> TxWitnessPPDataHash era

data WitnessPPDataSupportedInEra era where

    WitnessPPDataSupportedInAlonzoEra  :: WitnessPPDataSupportedInEra AlonzoEra

_witnessPPDataSupportedInEra :: CardanoEra era -> Maybe (WitnessPPDataSupportedInEra era)
_witnessPPDataSupportedInEra ByronEra   = Nothing
_witnessPPDataSupportedInEra ShelleyEra = Nothing
_witnessPPDataSupportedInEra AllegraEra = Nothing
_witnessPPDataSupportedInEra MaryEra    = Nothing
_witnessPPDataSupportedInEra AlonzoEra  = Just WitnessPPDataSupportedInAlonzoEra


--_toAlonzoDataHash :: DataHash era -> Maybe (Alonzo.DataHash StandardCrypto)
--_toAlonzoDataHash DataHashNone = Nothing
--_toAlonzoDataHash (DataHash DataHashSupportedInAlonzoEra _datum) = Nothing --TODO:JORDAN
--
--_fromAlonzoDataHash :: StrictMaybe (Alonzo.DataHash StandardCrypto) -> DataHash era
--_fromAlonzoDataHash _ = DataHashNone

-- ----------------------------------------------------------------------------
-- Transaction body content
--

data TxBodyContent build era =
     TxBodyContent {
       txIns             :: [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))],
       txOuts            :: [TxOut era],
       txFee             :: TxFee era,
       txValidityRange   :: (TxValidityLowerBound era,
                             TxValidityUpperBound era),
       txMetadata        :: TxMetadataInEra era,
       txAuxScripts      :: TxAuxScripts era,
       txWithdrawals     :: TxWithdrawals  build era,
       txCertificates    :: TxCertificates build era,
       txUpdateProposal  :: TxUpdateProposal era,
       txMintValue       :: TxMintValue build era,
       txPlutusWitnesses :: TxPlutusWitnesses build era,
       txWitnessPPData   :: TxWitnessPPDataHash era
     }


-- ----------------------------------------------------------------------------
-- Transaction bodies
--

data TxBody era where

     ByronTxBody
       :: Annotated Byron.Tx ByteString
       -> TxBody ByronEra

     ShelleyTxBody
       :: ShelleyBasedEra era
       -> Ledger.TxBody (ShelleyLedgerEra era)

          -- We include the scripts along with the tx body, rather than the
          -- witnesses set, since they need to be known when building the body.
       -> [Ledger.Script (ShelleyLedgerEra era)]

          -- The 'Ledger.AuxiliaryData' consists of one or several things,
          -- depending on era:
          -- + transaction metadata  (in Shelley and later)
          -- + auxiliary scripts     (in Allegra and later)
          -- + auxiliary script data (in Allonzo and later)
       -> Maybe (Ledger.AuxiliaryData (ShelleyLedgerEra era))

          -- We include the redeemer pointer map along with the tx body since itt
          -- needs to be known when building the body to contruct the 'WitnessPPDataHash'
       -> Maybe (Map Alonzo.RdmrPtr (Alonzo.Data (ShelleyLedgerEra era), Alonzo.ExUnits))

       -> TxBody era
     -- The 'ShelleyBasedEra' GADT tells us what era we are in.
     -- The 'ShelleyLedgerEra' type family maps that to the era type from the
     -- ledger lib. The 'Ledger.TxBody' type family maps that to a specific
     -- tx body type, which is different for each Shelley-based era.


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Eq (TxBody era) where
    (==) (ByronTxBody txbodyA)
         (ByronTxBody txbodyB) = txbodyA == txbodyB

    (==) (ShelleyTxBody era txbodyA txscriptsA txmetadataA rdmrMapA)
         (ShelleyTxBody _   txbodyB txscriptsB txmetadataB rdmrMapB) =
         case era of
           ShelleyBasedEraShelley -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB
                                  && rdmrMapA    == rdmrMapB

           ShelleyBasedEraAllegra -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB
                                  && rdmrMapA    == rdmrMapB

           ShelleyBasedEraMary    -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB
                                  && rdmrMapA    == rdmrMapB

           ShelleyBasedEraAlonzo  -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB
    (==) ByronTxBody{} (ShelleyTxBody era _ _ _ _ ) = case era of {}


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Show (TxBody era) where
    showsPrec p (ByronTxBody txbody) =
      showParen (p >= 11)
        ( showString "ByronTxBody "
        . showsPrec 11 txbody
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraShelley txbody txscripts txmetadata _) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraShelley "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 txmetadata
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraAllegra txbody txscripts txmetadata _) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAllegra "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 txmetadata
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraMary txbody txscripts txmetadata _) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraMary "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 txmetadata
        )
    showsPrec p (ShelleyTxBody ShelleyBasedEraAlonzo txbody txscripts txmetadata rdmrmap) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAlonzo "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 rdmrmap
        )

instance HasTypeProxy era => HasTypeProxy (TxBody era) where
    data AsType (TxBody era) = AsTxBody (AsType era)
    proxyToAsType _ = AsTxBody (proxyToAsType (Proxy :: Proxy era))

pattern AsByronTxBody :: AsType (TxBody ByronEra)
pattern AsByronTxBody   = AsTxBody AsByronEra
{-# COMPLETE AsByronTxBody #-}

pattern AsShelleyTxBody :: AsType (TxBody ShelleyEra)
pattern AsShelleyTxBody = AsTxBody AsShelleyEra
{-# COMPLETE AsShelleyTxBody #-}

pattern AsMaryTxBody :: AsType (TxBody MaryEra)
pattern AsMaryTxBody = AsTxBody AsMaryEra
{-# COMPLETE AsMaryTxBody #-}

instance IsCardanoEra era => SerialiseAsCBOR (TxBody era) where

    serialiseToCBOR (ByronTxBody txbody) =
      recoverBytes txbody

    serialiseToCBOR (ShelleyTxBody era txbody txscripts txmetadata _isValid) =
      case era of
        -- Use the same serialisation impl, but at different types:
        ShelleyBasedEraShelley -> serialiseShelleyBasedTxBody txbody txscripts txmetadata
        ShelleyBasedEraAllegra -> serialiseShelleyBasedTxBody txbody txscripts txmetadata
        ShelleyBasedEraMary    -> serialiseShelleyBasedTxBody txbody txscripts txmetadata
        ShelleyBasedEraAlonzo  -> error "TODO:Jordan"

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTxBody <$>
            CBOR.decodeFullAnnotatedBytes
              "Byron TxBody"
              CBOR.fromCBORAnnotated
              (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        ShelleyEra -> error "TODO"
          --deserialiseShelleyBasedTxBody (ShelleyTxBody ShelleyBasedEraShelley) bs
        AllegraEra -> error "TODO"
          --deserialiseShelleyBasedTxBody (ShelleyTxBody ShelleyBasedEraAllegra) bs
        MaryEra    -> error "TODO"
          --deserialiseShelleyBasedTxBody (ShelleyTxBody ShelleyBasedEraMary) bs
        AlonzoEra  -> error "TODO"
          --deserialiseShelleyBasedTxBody (ShelleyTxBody ShelleyBasedEraAlonzo) bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
--
serialiseShelleyBasedTxBody :: forall txbody script metadata.
                                (ToCBOR txbody, ToCBOR script, ToCBOR metadata)
                            => txbody
                            -> [script]
                            -> Maybe metadata
                            -> ByteString
serialiseShelleyBasedTxBody txbody txscripts txmetadata =
    CBOR.serializeEncoding' $
        CBOR.encodeListLen 3
     <> CBOR.toCBOR txbody
     <> CBOR.toCBOR txscripts
     <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata

_deserialiseShelleyBasedTxBody :: forall txbody script metadata pair.
                                (FromCBOR (CBOR.Annotator txbody),
                                 FromCBOR (CBOR.Annotator script),
                                 FromCBOR (CBOR.Annotator metadata))
                              => (txbody -> [script] -> Maybe metadata -> pair)
                              -> ByteString
                              -> Either CBOR.DecoderError pair
_deserialiseShelleyBasedTxBody mkTxBody bs =
    CBOR.decodeAnnotator
      "Shelley TxBody"
      decodeAnnotatedTuple
      (LBS.fromStrict bs)
  where
    decodeAnnotatedTuple :: CBOR.Decoder s (CBOR.Annotator pair)
    decodeAnnotatedTuple =  do
      CBOR.decodeListLenOf 3
      txbody     <- fromCBOR
      txscripts  <- fromCBOR
      txmetadata <- CBOR.decodeNullMaybe fromCBOR
      return $ CBOR.Annotator $ \fbs ->
        mkTxBody
          (CBOR.runAnnotator txbody fbs)
          (map (`CBOR.runAnnotator` fbs) txscripts)
          (CBOR.runAnnotator <$> txmetadata <*> pure fbs)

instance IsCardanoEra era => HasTextEnvelope (TxBody era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxUnsignedByron"
        ShelleyEra -> "TxUnsignedShelley"
        AllegraEra -> "TxBodyAllegra"
        MaryEra    -> "TxBodyMary"
        AlonzoEra    -> "TxBodyAlonzo"


-- ----------------------------------------------------------------------------
-- Constructing transaction bodies
--

data TxBodyError era =
       TxBodyEmptyTxIns
     | TxBodyEmptyTxOuts
     | TxBodyOutputNegative Quantity (TxOut era)
     | TxBodyOutputOverflow Quantity (TxOut era)
     | TxBodyMetadataError [(Word64, TxMetadataRangeError)]
     | TxBodyMintAdaError
     deriving Show

instance Error (TxBodyError era) where
    displayError TxBodyEmptyTxIns  = "Transaction body has no inputs"
    displayError TxBodyEmptyTxOuts = "Transaction body has no outputs"
    displayError (TxBodyOutputNegative (Quantity q) txout) =
      "Negative quantity (" ++ show q ++ ") in transaction output: " ++
      show txout
    displayError (TxBodyOutputOverflow (Quantity q) txout) =
      "Quantity too large (" ++ show q ++ " >= 2^64) in transaction output: " ++
      show txout
    displayError (TxBodyMetadataError [(k, err)]) =
      "Error in metadata entry " ++ show k ++ ": " ++ displayError err
    displayError (TxBodyMetadataError errs) =
      "Error in metadata entries: " ++
      intercalate "; "
        [ show k ++ ": " ++ displayError err
        | (k, err) <- errs ]
    displayError TxBodyMintAdaError =
      "Transaction cannot mint ada, only non-ada assets"


makeTransactionBody :: forall era.
                       IsCardanoEra era
                    => TxBodyContent BuildTx era
                    -> Either (TxBodyError era) (TxBody era)
makeTransactionBody =
    case cardanoEraStyle (cardanoEra :: CardanoEra era) of
      LegacyByronEra      -> makeByronTransactionBody
      ShelleyBasedEra era -> makeShelleyTransactionBody era


makeByronTransactionBody :: TxBodyContent BuildTx ByronEra
                         -> Either (TxBodyError ByronEra) (TxBody ByronEra)
makeByronTransactionBody TxBodyContent { txIns, txOuts } = do
    ins'  <- NonEmpty.nonEmpty txIns      ?! TxBodyEmptyTxIns
    let ins'' = NonEmpty.map (toByronTxIn . fst) ins'

    outs'  <- NonEmpty.nonEmpty txOuts    ?! TxBodyEmptyTxOuts
    outs'' <- traverse
                (\out -> toByronTxOut out ?! classifyRangeError out)
                outs'
    return $
      ByronTxBody $
        reAnnotate $
          Annotated
            (Byron.UnsafeTx ins'' outs'' (Byron.mkAttributes ()))
            ()
  where
    classifyRangeError :: TxOut ByronEra -> TxBodyError ByronEra
    classifyRangeError
      txout@(TxOut (AddressInEra ByronAddressInAnyEra ByronAddress{})
                   (TxOutAdaOnly AdaOnlyInByronEra value) _)
      | value < 0        = TxBodyOutputNegative (lovelaceToQuantity value) txout
      | otherwise        = TxBodyOutputOverflow (lovelaceToQuantity value) txout

    classifyRangeError
      (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
             (TxOutValue era _) _) = case era of {}

    classifyRangeError
      (TxOut (AddressInEra (ShelleyAddressInEra era) ShelleyAddress{})
             _ _) = case era of {}

makeShelleyTransactionBody :: ShelleyBasedEra era
                           -> TxBodyContent BuildTx era
                           -> Either (TxBodyError era) (TxBody era)
makeShelleyTransactionBody era@ShelleyBasedEraShelley
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (_, upperBound),
                             txMetadata,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do guard (v >= 0) ?! TxBodyOutputNegative (lovelaceToQuantity v) txout
           guard (v <= maxTxOut) ?! TxBodyOutputOverflow (lovelaceToQuantity v) txout
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Lovelace
      , txout@(TxOut _ (TxOutAdaOnly AdaOnlyInShelleyEra v) _) <- txOuts ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> first TxBodyMetadataError (validateTxMetadata m)

    return $
      ShelleyTxBody era
        (Shelley.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (case upperBound of
             TxValidityNoUpperBound era' -> case era' of {}
             TxValidityUpperBound _ ttl  -> ttl)
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toUpdate era p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData @StandardShelley <$> txAuxData)))
        (map toShelleySimpleScript (collectTxBodySimpleScripts txbodycontent))
        txAuxData
        Nothing
        -- _txnetworkid = SNothing
  where
    txAuxData :: Maybe (Ledger.AuxiliaryData StandardShelley)
    txAuxData
      | Map.null ms = Nothing
      | otherwise   = Just (toShelleyAuxiliaryData ms)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'

makeShelleyTransactionBody era@ShelleyBasedEraAllegra
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do guard (v >= 0) ?! TxBodyOutputNegative (lovelaceToQuantity v) txout
           guard (v <= maxTxOut) ?! TxBodyOutputOverflow (lovelaceToQuantity v) txout
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Lovelace
      , txout@(TxOut _ (TxOutAdaOnly AdaOnlyInAllegraEra v) _) <- txOuts
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError

    return $
      ShelleyTxBody era
        (Allegra.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             Allegra.invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             Allegra.invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toUpdate era p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData @StandardAllegra <$> txAuxData))
          mempty) -- No minting in Allegra, only Mary
        (map toShelleySimpleScript (collectTxBodySimpleScripts txbodycontent))
        txAuxData
        Nothing
        -- _txnetworkid = SNothing
  where
    txAuxData :: Maybe (Ledger.AuxiliaryData StandardAllegra)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just $ toAllegraAuxiliaryData ms ss
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

makeShelleyTransactionBody era@ShelleyBasedEraMary
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do allPositive
           allWithinMaxBound
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Quantity
      , txout@(TxOut _ (TxOutValue MultiAssetInMaryEra v) _) <- txOuts
      , let allPositive       = case [ q | (_,q) <- valueToList v, q < 0 ] of
                                  []  -> Right ()
                                  q:_ -> Left (TxBodyOutputNegative q txout)
            allWithinMaxBound = case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
                                  []  -> Right ()
                                  q:_ -> Left (TxBodyOutputOverflow q txout)
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone        -> return ()
      TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError

    return $
      ShelleyTxBody era
        (Allegra.TxBody
          (Set.fromList (map (toShelleyTxIn . fst) txIns))
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             Allegra.invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             Allegra.invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toShelleyUpdate p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData @StandardMary <$> txAuxData))
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toMaryValue v))
        (map toShelleySimpleScript (collectTxBodySimpleScripts txbodycontent))
        txAuxData
        Nothing
        -- _txnetworkid = SNothing
  where
    txAuxData :: Maybe (Ledger.AuxiliaryData StandardMary)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAllegraAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

makeShelleyTransactionBody era@ShelleyBasedEraAlonzo
                           TxBodyContent { txIns,
                                           txOuts,
                                           txFee,
                                           txValidityRange = (lowerBound, upperBound),
                                           txMetadata,
                                           txAuxScripts,
                                           txWithdrawals,
                                           txCertificates,
                                           txUpdateProposal,
                                           txMintValue,
                                           txWitnessPPData
                                          } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do allPositive
           allWithinMaxBound
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Quantity
      , txout@(TxOut _ (TxOutValue MultiAssetInAlonzoEra v) _mDataHash) <- txOuts
      , let allPositive       = case [ q | (_,q) <- valueToList v, q < 0 ] of
                                  []  -> Right ()
                                  q:_ -> Left (TxBodyOutputNegative q txout)
            allWithinMaxBound = case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
                                  []  -> Right ()
                                  q:_ -> Left (TxBodyOutputOverflow q txout)
      ]
    -- Although we don't have to separate the plutus fees from the spending tx inputs
    --we do so to reduce the tx fee.
    let plutusFees     = [txIn
                         | (txIn, BuildTxWith (KeyWitness KeyWitnessForPlutusFee)) <- txIns]
        spendingInputs = [txIn
                         | (txIn, buildWith) <- txIns
                         , buildWith /= BuildTxWith (KeyWitness KeyWitnessForPlutusFee)] --TODO: Only PLUTUS scripts allowed. We current don't filter them out here
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone      -> return ()
      TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError --TODO: Should check that we have a script witness

    return $
      ShelleyTxBody era
        (Alonzo.TxBody
          (Set.fromList $ map toShelleyTxIn spendingInputs)
          (Set.fromList $ map toShelleyTxIn plutusFees)
          (Seq.fromList (map toShelleyTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone  -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList $ map toShelleyCertificate cs)
          (case txWithdrawals of
             TxWithdrawalsNone  -> Shelley.Wdrl Map.empty
             TxWithdrawals _ ws -> toShelleyWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toShelleyLovelace fee)
          (Allegra.ValidityInterval {
             Allegra.invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             Allegra.invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toUpdate era p))
          (case txMintValue of
             TxMintNone      -> mempty
             TxMintValue _ v _ -> toMaryValue v)
          (case txWitnessPPData of
             TxWitnessPPDataHashNone -> SNothing
             TxWitnessPPDataHash _ mTxWitPPDataHash -> mTxWitPPDataHash)
          (maybeToStrictMaybe $ hashedTxAuxData txAuxData)
        )
        [] -- All Scripts have to do here including redeemers + exunits for plutus scripts
           -- (map toShelleySimpleScript (collectTxBodySimpleScripts txbodycontent))
        txAuxData
        (Just redeemerPtrMap)
        -- _txnetworkid = SNothing
-- TODO: Calc witnessppdata hash and pass the redeemer map onwards
  where
   -- TODO: Should use ValidateAuxiliaryData class when consensus updates their PR
   hashedTxAuxData :: Maybe (Alonzo.AuxiliaryData (Alonzo.AlonzoEra StandardCrypto))
                   -> Maybe (Alonzo.AuxiliaryDataHash StandardCrypto)
   hashedTxAuxData mAuxData = SafeHash.hashAnnotated <$> mAuxData

   txAuxData :: Maybe (Ledger.AuxiliaryData (Alonzo.AlonzoEra StandardCrypto))
   txAuxData
     | Map.null ms
     , null ss   = Nothing
     | otherwise = Just (toAlonzoAuxiliaryData ms ss [])--TODO: Plutus.Data should come from cli
     where
       ms = case txMetadata of
              TxMetadataNone                     -> Map.empty
              TxMetadataInEra _ (TxMetadata ms') -> ms'
       ss = case txAuxScripts of
              TxAuxScriptsNone   -> []
              TxAuxScripts _ ss' -> ss'

   redeemerPtrMap :: Map Alonzo.RdmrPtr (Alonzo.Data StandardAlonzo, Alonzo.ExUnits)
   redeemerPtrMap =
     Map.fromList $
       certifyingRedeemerPtr txCertificates
       <> mintingRedeemerPtr txMintValue
       <> spendingRedeemerPtr txIns
       <> rewardingRedeemerPtr txWithdrawals

makeTxWitnessPPDataHash
  :: forall era ledgerera.
     ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> ProtocolParameters era
  -> Set Alonzo.Language
  -> Map Alonzo.RdmrPtr (Alonzo.Data ledgerera, Alonzo.ExUnits)
    -- Should be: Map Alonzo.RdmrPtr (Alonzo.Data ledgerera, ExUnits)
  -> StrictMaybe (Alonzo.WitnessPPDataHash (Ledger.Crypto ledgerera))
makeTxWitnessPPDataHash sbe@ShelleyBasedEraAlonzo pParams langs _rMap =
  maybeToStrictMaybe
    $ Alonzo.hashWitnessPPData
        (toShelleyPParams sbe  pParams)
        langs
        (error "rMap")
        -- TODO: Consensus needs to update to latest ledger specs
makeTxWitnessPPDataHash _ _ _ _ = SNothing


-- TODO: Better name?
-- | To construct our redeemer pointer map, we need
--the thing the redeemer point will point to, the redeeemer
--and the Plutus script execution units budget.
extractRedeemerPtrIngredients
  :: ShelleyLedgerEra era ~ ledgerera
  => Map a (Witness witxtc era)
  -> [(a, Alonzo.Data ledgerera, Alonzo.ExUnits)]
extractRedeemerPtrIngredients m =
  [ (target, redeemer, fromScriptExecutionUnits exunits)
  | (target, ScriptWitness ScriptWitnessForMinting (PlutusScriptWitness _ _ _ exunits _ (Redeemer redeemer)) )
       <- Map.toList m
  ]


certifyingRedeemerPtr
 :: forall era ledgerera. ShelleyLedgerEra era ~ ledgerera
 => TxCertificates BuildTx era -> [(Alonzo.RdmrPtr, (Alonzo.Data ledgerera, Alonzo.ExUnits))]
certifyingRedeemerPtr TxCertificatesNone = []
certifyingRedeemerPtr (TxCertificates CertificatesInAlonzoEra certs (BuildTxWith sCredMap)) = do
  let ingreds = extractRedeemerPtrIngredients sCredMap
  map (\(sCred, redeemer, exunits) -> (createRdmrPtr certs sCred, (redeemer, exunits))) ingreds
 where
   createRdmrPtr :: [Certificate] -> StakeCredential -> Alonzo.RdmrPtr
   createRdmrPtr certs' sCred =
     case findIndex (onCert sCred) certs' of
       Just index ->
         case intToWord64 index of
             Right w64 -> Alonzo.RdmrPtr Alonzo.Cert w64
             Left err ->
               error $ "certifyingRedeemerPtr: Error occurred while constructing a \
                       \certifying redeemer pointer: " <> err
       Nothing -> error $ "certifyingRedeemerPtr: Script witness not provided\
                          \ for stake credential: " <> show sCred
   onCert :: StakeCredential -> Certificate -> Bool
   onCert sCred (StakeAddressDeregistrationCertificate target) = sCred == target
   onCert sCred (StakeAddressDelegationCertificate target _) = sCred == target
   onCert _ _ = False
certifyingRedeemerPtr _ = []

-- TODO: Definitely want a property test that the constructed redeemers point to the
--correct thing in the final tx.
mintingRedeemerPtr
  :: ShelleyLedgerEra era ~ ledgerera
  => TxMintValue BuildTx era
  -- BuildTxWith build (Map PolicyId (Witness WitCtxMint era))
  -- -> Value
  -> [(Alonzo.RdmrPtr, (Alonzo.Data ledgerera, Alonzo.ExUnits))]
mintingRedeemerPtr TxMintNone = []
mintingRedeemerPtr (TxMintValue MultiAssetInAlonzoEra val (BuildTxWith polWitMap)) = do
  -- get policyId, redeemer and exunits
  let polRedExUnits = extractRedeemerPtrIngredients polWitMap
  map (\(pid, redeemer, exunits) -> (createRdmrPtr val pid, (redeemer, exunits))) polRedExUnits
 where
   selectPolId :: PolicyId -> (AssetId, Quantity) -> Bool
   selectPolId p (AssetId pid' _, _) = p == pid'
   selectPolId _ _ = False

   createRdmrPtr :: Value -> PolicyId -> Alonzo.RdmrPtr
   createRdmrPtr val' pid  =
     case findIndex (selectPolId pid) $ valueToList val' of
         Just index -> do
           case intToWord64 index of
             Right w64 -> Alonzo.RdmrPtr Alonzo.Mint w64
             Left err ->
               error $ "mintingRedeemerPtr: Error occurred while constructing a \
                       \minting redeemer pointer: " <> err
         Nothing -> error $ "mintingRedeemerPtr: Script witness not provided.\
                          \ PolicyId: " <> show pid
mintingRedeemerPtr _ = []

intToWord64 :: Int -> Either String Word64
intToWord64 int = do
       let maxW64 = fromIntegral (maxBound :: Word64) :: Int
       if maxW64 > int
       then Left $ "Word64 bound exceeded " <> show int
       else Right $ fromIntegral int

spendingRedeemerPtr
  :: forall era ledgerera. ShelleyLedgerEra era ~ ledgerera
  => [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
  -> [(Alonzo.RdmrPtr, (Alonzo.Data ledgerera, Alonzo.ExUnits))]
spendingRedeemerPtr txins = do
  let rdmrPtrIngreds = extractRedeemerIngreds txins
  map (\(txin, redeemer, exunits) -> (createRdmrPtr txin txins, (redeemer, exunits))) rdmrPtrIngreds
 where
   extractRedeemerIngreds
     :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
     -> [(TxIn, Alonzo.Data ledgerera, Alonzo.ExUnits)]
   extractRedeemerIngreds txins' =
     [ (txin, redeemer', fromScriptExecutionUnits exunits')
     | (txin, BuildTxWith (ScriptWitness ScriptWitnessForSpending (PlutusScriptWitness _ _ _ exunits' _ (Redeemer redeemer')))) <- txins'
     ]

   createRdmrPtr :: TxIn -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))] -> Alonzo.RdmrPtr
   createRdmrPtr txin' txins' =
    case findIndex (onTxIn txin') txins' of
      Just index ->
        case intToWord64 index of
          Right w64 -> Alonzo.RdmrPtr Alonzo.Spend w64
          Left err ->
            error $ "spendingRedeemerPtr: Error occurred while constructing a \
                    \spending redeemer pointer: " <> err
      Nothing ->
        error $ "spendingRedeemerPtr: A Plutus script witness has not been \
                \provided for txin: " <> show txin'

   onTxIn :: TxIn -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era)) -> Bool
   onTxIn txin (txin',_) = txin == txin'
-- TODO: Left off here: Push datum type through to txout
         -- Checkout ledger specs to see what's happening there
rewardingRedeemerPtr
  :: forall era ledgerera. ShelleyLedgerEra era ~ ledgerera
  => TxWithdrawals BuildTx era
  -> [(Alonzo.RdmrPtr, (Alonzo.Data ledgerera, Alonzo.ExUnits))]
rewardingRedeemerPtr (TxWithdrawals WithdrawalsInAlonzoEra withdrwls) = do
  let rdmrPtrIngreds = extractRedeemerIngreds withdrwls
  map (\(sAddr, redeemer, exunits) -> (createRdmrPtr sAddr withdrwls, (redeemer, exunits))) rdmrPtrIngreds
 where
   extractRedeemerIngreds
     :: [(StakeAddress, Lovelace, BuildTxWith BuildTx (Witness WitCtxStake era))]
     -> [(StakeAddress, Alonzo.Data ledgerera, Alonzo.ExUnits)]
   extractRedeemerIngreds wdrwls =
     [ (sAddr, redeemer', fromScriptExecutionUnits exunits')
     | (sAddr, _, BuildTxWith (ScriptWitness ScriptWitnessForStakeAddr (PlutusScriptWitness _ _ _ exunits' _ (Redeemer redeemer')))) <- wdrwls
     ]

   createRdmrPtr
     :: StakeAddress
     -> [(StakeAddress, Lovelace, BuildTxWith BuildTx (Witness WitCtxStake era))]
     -> Alonzo.RdmrPtr
   createRdmrPtr sAddr withdrawals =
     case findIndex (onStakeAddr sAddr) withdrawals of
       Just index ->
         case intToWord64 index of
           Right w64 -> Alonzo.RdmrPtr Alonzo.Rewrd w64
           Left err ->
             error $ "rewardingRedeemerPtr: Error occurred while constructing a \
                       \reward redeemer pointer: " <> err
       Nothing ->
         error $ "rewardingRedeemerPtr: No script witness supplied for Plutus locked reward withdrawal: " <> show sAddr

   onStakeAddr
     :: StakeAddress
     -> (StakeAddress, Lovelace, BuildTxWith BuildTx (Witness WitCtxStake era))
     -> Bool
   onStakeAddr sA (sA', _, _) = sA == sA'
rewardingRedeemerPtr _ = []

data SimpleScriptInEra era where
     SimpleScriptInEra :: ScriptLanguageInEra lang era
                       -> SimpleScriptVersion lang
                       -> SimpleScript lang
                       -> SimpleScriptInEra era

{-# ANN collectTxBodySimpleScripts ("HLint: ignore Reduce duplication" :: Text) #-}

collectTxBodySimpleScripts :: TxBodyContent BuildTx era
                           -> [SimpleScriptInEra era]
collectTxBodySimpleScripts TxBodyContent {
                             txIns,
                             txWithdrawals,
                             txCertificates,
                             txMintValue
                           } =
    [ script
    | (_, BuildTxWith witness) <- txIns
    , script <- simpleScriptInEra witness ]

 ++ [ script
    | TxWithdrawals _ withdrawals <- [txWithdrawals]
    , (_, _, BuildTxWith witness) <- withdrawals
    , script <- simpleScriptInEra witness ]

 ++ [ script
    | TxCertificates _ _ (BuildTxWith witnesses) <- [txCertificates]
    , witness <- Map.elems witnesses
    , script <- simpleScriptInEra witness ]

 ++ [ script
    | TxMintValue _ _ (BuildTxWith witnesses) <- [txMintValue]
    , witness <- Map.elems witnesses
    , script <- simpleScriptInEra witness ]

  where
    simpleScriptInEra :: Witness witctx era -> [SimpleScriptInEra era]
    simpleScriptInEra (ScriptWitness
                         _ (SimpleScriptWitness langInEra version script)) =
      [SimpleScriptInEra langInEra version script]
    simpleScriptInEra _ = []


toShelleySimpleScript :: SimpleScriptInEra era
                      -> Ledger.Script (ShelleyLedgerEra era)
toShelleySimpleScript (SimpleScriptInEra langInEra version script) =
    toShelleyScript (ScriptInEra langInEra (SimpleScript version script))

toShelleyWithdrawal :: [(StakeAddress, Lovelace, a)] -> Shelley.Wdrl StandardCrypto
toShelleyWithdrawal withdrawals =
    Shelley.Wdrl $
      Map.fromList
        [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
        | (stakeAddr, value, _) <- withdrawals ]

-- | In the Shelley era the auxiliary data consists only of the tx metadata
toShelleyAuxiliaryData :: Map Word64 TxMetadataValue
                       -> Ledger.AuxiliaryData StandardShelley
toShelleyAuxiliaryData m =
    Shelley.Metadata
      (toShelleyMetadata m)

-- | In the Allegra and Mary eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts.
--
toAllegraAuxiliaryData :: forall era ledgeera.
                          ShelleyLedgerEra era ~ ledgeera
                       => Ledger.AuxiliaryData ledgeera ~ Allegra.AuxiliaryData ledgeera
                       => Ledger.AnnotatedData (Ledger.Script ledgeera)
                       => Ord (Ledger.Script ledgeera)
                       => Map Word64 TxMetadataValue
                       -> [ScriptInEra era]
                       -> Ledger.AuxiliaryData ledgeera
toAllegraAuxiliaryData m ss =
    Allegra.AuxiliaryData
      (toShelleyMetadata m)
      (Seq.fromList (map toShelleyScript ss))

toAlonzoAuxiliaryData
  :: ( Ledger.Era ledgerera
     , Ord (Ledger.Script ledgerera)
     , ToCBOR (Ledger.Script ledgerera)
     , Ledger.Script ledgerera ~ Ledger.Script (ShelleyLedgerEra era))
  => Map Word64 TxMetadataValue
  -> [ScriptInEra era]
  -> [Alonzo.Data ledgerera]
  -> Alonzo.AuxiliaryData ledgerera
toAlonzoAuxiliaryData m ss plutusData =
     Alonzo.AuxiliaryData
       (Set.fromList $ map toShelleyScript ss)
       (Set.fromList plutusData)
       (Set.singleton $ Shelley.Metadata $ toShelleyMetadata m)

-- ----------------------------------------------------------------------------
-- Transitional utility functions for making transaction bodies
--

-- | Transitional function to help the CLI move to the updated TxBody API.
--
makeByronTransaction :: [TxIn]
                     -> [TxOut ByronEra]
                     -> Either (TxBodyError ByronEra) (TxBody ByronEra)
makeByronTransaction txIns txOuts =
    makeTransactionBody $
      TxBodyContent {
        txIns             = [ (txin, BuildTxWith (KeyWitness KeyWitnessForSpending))
                            | txin <- txIns ],
        txOuts,
        txFee             = TxFeeImplicit TxFeesImplicitInByronEra,
        txValidityRange   = (TxValidityNoLowerBound,
                             TxValidityNoUpperBound
                               ValidityNoUpperBoundInByronEra),
        txMetadata        = TxMetadataNone,
        txAuxScripts      = TxAuxScriptsNone,
        txWithdrawals     = TxWithdrawalsNone,
        txCertificates    = TxCertificatesNone,
        txUpdateProposal  = TxUpdateProposalNone,
        txMintValue       = TxMintNone,
        txPlutusWitnesses = TxPlutusWitnessesNone,
        txWitnessPPData   = TxWitnessPPDataHashNone
      }
{-# DEPRECATED makeByronTransaction "Use makeTransactionBody" #-}

-- ----------------------------------------------------------------------------
-- Other utilities helpful with making transaction bodies
--

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
--
genesisUTxOPseudoTxIn :: NetworkId -> Hash GenesisUTxOKey -> TxIn
genesisUTxOPseudoTxIn nw (GenesisUTxOKeyHash kh) =
    --TODO: should handle Byron UTxO case too.
    fromShelleyTxIn (Shelley.initialFundsPseudoTxIn addr)
  where
    addr :: Shelley.Addr StandardCrypto
    addr = Shelley.Addr
             (toShelleyNetwork nw)
             (Shelley.KeyHashObj kh)
             Shelley.StakeRefNull

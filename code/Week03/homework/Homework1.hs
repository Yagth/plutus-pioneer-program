{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), Validator,
                                       mkValidatorScript, TxInfo (txInfoValidRange))
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), traceIfFalse, ($), (&&), (||), AdditiveSemigroup ((+)))
import           Utilities            (wrapValidator)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Interval

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = traceIfFalse "Beneficiary one information missing or deadline has passed" (signedByBeneficiary1 && beforeDeadLine) || 
    traceIfFalse "Beneficiary two information missing or deadline is not yet reached" (signedByBeneficiary2 && afterDeadLine)
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx
        
        signedByBeneficiary1 :: Bool
        signedByBeneficiary1 = txSignedBy info $ beneficiary1 _dat

        signedByBeneficiary2 :: Bool
        signedByBeneficiary2 = txSignedBy info $ beneficiary2 _dat

        beforeDeadLine :: Bool
        beforeDeadLine = contains (to $ deadline _dat) (txInfoValidRange info)

        afterDeadLine :: Bool
        afterDeadLine = contains (from $ deadline _dat + 1) (txInfoValidRange  info)

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])

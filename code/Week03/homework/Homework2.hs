{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), Validator,
                                       mkValidatorScript, TxInfo (txInfoValidRange))
import           PlutusTx             (applyCode, compile, liftCode)
import           PlutusTx.Prelude     (Bool (False), (.), traceIfFalse, (&&))
import           Utilities            (wrapValidator)
import Plutus.V1.Ledger.Interval
import Plutus.V2.Ledger.Contexts (txSignedBy)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator _beneficiary _deadline () _ctx = traceIfFalse "Beneficiary information missing" signedByBeneficiary &&
    traceIfFalse "DeadLine not reached yet" afterDeadLine

    where 
        info :: TxInfo
        info = scriptContextTxInfo _ctx

        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info _beneficiary

        afterDeadLine :: Bool
        afterDeadLine = contains (from _deadline) (txInfoValidRange info)

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)

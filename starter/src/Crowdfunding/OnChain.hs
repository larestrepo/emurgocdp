
data Campaign = Campaign
    {
        beneficiary :: PaymentPubKeyHash,
        targetCurrency :: Ada
        deadline :: POSIXTime
    }


newtype Dat = Dat
    {
        contributor :: PaymentPubKeyHash
    }

-- redeem to use the status of the crowdfunding


-- First option
crowdValidation :: Campaign -> Dat -> () -> ScriptContext -> Bool
crowdValidation crp d _ context = (transaction signed by the contributor) && (not campaignEndedSuccesfully) ||
    (transaction signed by benfeiciary) && campaignEndedSuccesfully

-- Second option

simpleType :: CrowdParams -> BuiltInData -> Redeem -> Contexts.ScriptContext -> Bool
simpleType cfParams _ r context = 
    case r of 
        -- Init       -> traceIfFalse "Wrong pubkeyhash"        signedCrowdFunding &&
        --               traceIfFalse "malformed Init Datum"    checkInitDatum
        Contribute -> traceIfFalse "Deadline passed"         $ not deadlinepassed 
        Withdraw   -> traceIfFalse "Deadline passed"         $ not deadlinepassed && 
                      traceIfFalse "signer did not contribute" signerMadeContribution  
        Close      -> traceIfFalse "Deadline not yet passed" deadlinepassed && 
                      ((traceIfFalse "target met, but funds not going to beneficiary"  targetMet && closingFundsToBeneficiary) ||
                      (traceIfFalse "target not met, funds must go back to contributors" (not targetMet) && fundsReturnedToContributors))
    where



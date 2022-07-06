module Sample.Proposal.Shared (proposalTxRef, stakeTxRef, testFunc) where

import Plutarch.Api.V1 (PValidator)
import Plutarch.Lift (PUnsafeLiftDecl (..))
import PlutusLedgerApi.V1 (ScriptContext, ToData, TxId)
import Test.Specification (
  SpecificationTree,
  validatorFailsWith,
  validatorSucceedsWith,
 )

-- | 'TxId' of all the propsoal inputs in the samples.
proposalTxRef :: TxId
proposalTxRef = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"

-- | 'TxId' of all the stake inputs in the samples.
stakeTxRef :: TxId
stakeTxRef = "0ca36f3a357bc69579ab2531aecd1e7d3714d993c7820f40b864be15"

-- | Get the test function given whether a test case is valid.
testFunc ::
  forall {datum :: PType} {redeemer :: PType}.
  ( PUnsafeLiftDecl datum
  , PUnsafeLiftDecl redeemer
  , ToData (PLifted datum)
  , ToData (PLifted redeemer)
  ) =>
  -- | Should the validator pass?
  Bool ->
  String ->
  ClosedTerm PValidator ->
  PLifted datum ->
  PLifted redeemer ->
  ScriptContext ->
  SpecificationTree
testFunc isValid =
  if isValid
    then validatorSucceedsWith
    else validatorFailsWith

module Sample.Proposal.Shared (proposalRef, stakeRef) where

import PlutusLedgerApi.V1 (TxOutRef (..))

proposalRef :: TxOutRef
proposalRef = TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1

stakeRef :: TxOutRef
stakeRef = TxOutRef "0ca36f3a357bc69579ab2531aecd1e7d3714d993c7820f40b864be15" 0

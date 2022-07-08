module Sample.Proposal.Shared (proposalTxRef, stakeTxRef) where

import PlutusLedgerApi.V1 (TxId)

-- | 'TxId' of all the propsoal inputs in the samples.
proposalTxRef :: TxId
proposalTxRef = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"

-- | 'TxId' of all the stake inputs in the samples.
stakeTxRef :: TxId
stakeTxRef = "0ca36f3a357bc69579ab2531aecd1e7d3714d993c7820f40b864be15"

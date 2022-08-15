{- |
Module     : Sample.Proposal.Shared
Maintainer : connor@mlabs.city
Description: Shared constants for proposal samples

Shared constants for proposal samples.
-}
module Sample.Proposal.Shared (proposalTxRef, stakeTxRef, governorTxRef) where

import PlutusLedgerApi.V2 (TxId)

-- | 'TxId' of all the proposal inputs in the samples.
proposalTxRef :: TxId
proposalTxRef = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"

-- | 'TxId' of all the stake inputs in the samples.
stakeTxRef :: TxId
stakeTxRef = "0ca36f3a357bc69579ab2531aecd1e7d3714d993c7820f40b864be15"

governorTxRef :: TxId
governorTxRef = "cb076140e80d240f9c89e478aedbddbe6f4734fecbd0ae3e37404c12e7798c0f"

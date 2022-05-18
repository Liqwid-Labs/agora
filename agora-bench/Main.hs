module Main (main) where

import Agora.AuthorityToken (authorityTokenPolicy)
import Agora.Effect.TreasuryWithdrawal (treasuryWithdrawalValidator)
import Agora.Governor (Governor (..))
import Agora.Governor.Scripts (governorPolicy, governorValidator)
import Agora.Proposal.Scripts (proposalPolicy, proposalValidator)
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Agora.Treasury (treasuryValidator)
import Bench
import Data.Foldable (for_)
import Plutus.V1.Ledger.Api (CurrencySymbol)
import Sample.Shared
import Prelude

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let benchmarks =
        mconcat
          [ -- GATs
            benchmarkSize "authorityTokenPolicy" $ compile $ authorityTokenPolicy authorityToken
          , -- Governor
            benchmarkSize "governorValidator" $ compile $ governorValidator governor
          , benchmarkSize "governorPolicy" $ compile $ governorPolicy governor
          , -- Stake
            benchmarkSize "stakeValidator" $ compile $ stakeValidator stake
          , benchmarkSize "stakePolicy" $ compile $ stakePolicy governor.gtClassRef
          , -- Proposal
            benchmarkSize "proposalValidator" $ compile $ proposalValidator proposal
          , benchmarkSize "proposalPolicy" $ compile $ proposalPolicy govAssetClass
          , -- Treasury
            benchmarkSize "treasuryValidator" $ compile $ treasuryValidator gatCS
          , -- Effect validators
            benchmarkSize "treasuryWithdrawalValidator" $ compile $ treasuryWithdrawalValidator gatCS
          ]

  for_ benchmarks print

gatCS :: CurrencySymbol
gatCS = "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049" -- arbitrary CS

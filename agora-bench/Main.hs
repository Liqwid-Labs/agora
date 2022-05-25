module Main (main) where

import Bench (specificationTreeToBenchmarks)
import Data.Csv (encodeDefaultOrderedByName)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO as I
import Spec.AuthorityToken qualified as AuthorityToken
import Spec.Effect.GovernorMutation qualified as GovernorMutation
import Spec.Effect.TreasuryWithdrawal qualified as TreasuryWithdrawal
import Spec.Governor qualified as Governor
import Spec.Proposal qualified as Proposal
import Spec.Specification (group)
import Spec.Stake qualified as Stake
import Spec.Treasury qualified as Treasury
import Prelude

--------------------------------------------------------------------------------

main :: IO ()
main = do
  I.writeFile "bench.csv" $
    (decodeUtf8 . encodeDefaultOrderedByName) res

  mapM_ (Prelude.putStrLn . (<> "\n") . show) res
  where
    res =
      specificationTreeToBenchmarks $
        group
          "Agora"
          [ group
              "Effects"
              [ group "Treasury Withdrawal Effect" TreasuryWithdrawal.specs
              , group "Governor Mutation Effect" GovernorMutation.specs
              ]
          , group "Stake" Stake.specs
          , group "Proposal" Proposal.specs
          , group "AuthorityToken" AuthorityToken.specs
          , group "Treasury" Treasury.specs
          , group "AuthorityToken" AuthorityToken.specs
          , group "Governor" Governor.specs
          ]

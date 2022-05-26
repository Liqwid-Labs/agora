module Main (main) where

import Bench (specificationTreeToBenchmarks)
import Data.Csv (encodeDefaultOrderedByName)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO as I (writeFile)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)
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

  mapM_ (putStrLn . renderString . layoutPretty defaultLayoutOptions . pretty) res
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

module Main (main) where

import Bench (specificationTreeToBenchmarks)
import Data.Csv (EncodeOptions (encUseCrLf), defaultEncodeOptions, encodeDefaultOrderedByNameWith)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO as I (putStr, writeFile)
import Options (Options (output), parseOptions)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderLazy)
import Spec.AuthorityToken qualified as AuthorityToken
import Spec.Effect.GovernorMutation qualified as GovernorMutation
import Spec.Effect.TreasuryWithdrawal qualified as TreasuryWithdrawal
import Spec.Governor qualified as Governor
import Spec.Proposal qualified as Proposal
import Spec.Stake qualified as Stake
import Spec.Treasury qualified as Treasury
import System.IO (hIsTerminalDevice, stdout)
import Test.Specification (group)
import Prelude

--------------------------------------------------------------------------------

main :: IO ()
main = do
  options <- parseOptions
  isTTY <- hIsTerminalDevice stdout

  mapM_ (`I.writeFile` csv) options.output

  I.putStr $
    if isTTY
      then prettified
      else csv
  where
    encodeOptions =
      defaultEncodeOptions
        { encUseCrLf = False
        }

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

    csv = decodeUtf8 $ encodeDefaultOrderedByNameWith encodeOptions res

    prettified = renderLazy $ layoutPretty defaultLayoutOptions $ pretty res

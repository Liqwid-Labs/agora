{- |
Module: Agora.GT.Policy
Maintainer: jack@mlabs.city
Description: Governance Token (GT) minting policy.

Governance Token (GT) minting policy.
-}
module Agora.GT.Policy (gtMintingPolicy) where

import Plutarch.Api.V1 (PMintingPolicy)

{- | Minting policy for governance tokens (GT).

     Always passes at-present.
-}
gtMintingPolicy :: ClosedTerm PMintingPolicy
gtMintingPolicy =
  plam $ \_redeemer _ctx -> unTermCont $ do
    pure $ popaque (pconstant ())

{- |
Module     : Agora.SafeMoney
Maintainer : emi@haskell.fyi
Description: Phantom-type protected types for handling money in Plutus.

Phantom-type protected types for handling money in Plutus.
-}
module Agora.SafeMoney (
  -- * Types
  MoneyClass,
  PDiscrete,
  Discrete,

  -- * Utility functions
  paddDiscrete,
  pgeqDiscrete,
  pzeroDiscrete,

  -- * Conversions
  pdiscreteValue,
  pvalueDiscrete,

  -- * Example MoneyClasses
  LQ,
  ADA,
) where

import Data.Proxy (Proxy (Proxy))
import Data.String
import GHC.TypeLits (
  KnownSymbol,
  Nat,
  Symbol,
  symbolVal,
 )
import Prelude

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (PValue)
import Plutarch.Builtin ()
import Plutarch.Internal ()
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

import Agora.Utils

--------------------------------------------------------------------------------

-- | Type-level unique identifier for an `AssetClass`
type MoneyClass =
  ( -- AssetClass
    Symbol
  , -- TokenName
    Symbol
  , -- Decimal places
    Nat
  )

-- | A 'PDiscrete' amount of currency tagged on the type level with the 'MoneyClass' it belongs to
newtype PDiscrete (mc :: MoneyClass) (s :: S)
  = PDiscrete (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype (PDiscrete mc) PInteger)

newtype Discrete (mc :: MoneyClass)
  = Discrete Integer
  deriving stock (Show)

-- | Check if one 'PDiscrete' is greater than another.
pgeqDiscrete :: forall (mc :: MoneyClass) (s :: S). Term s (PDiscrete mc :--> PDiscrete mc :--> PBool)
pgeqDiscrete = phoistAcyclic $
  plam $ \x y -> P.do
    PDiscrete x' <- pmatch x
    PDiscrete y' <- pmatch y
    y' #<= x'

-- | Conjure zero discrete unit for any moneyclass
pzeroDiscrete :: forall (mc :: MoneyClass) (s :: S). Term s (PDiscrete mc)
pzeroDiscrete = phoistAcyclic $ pcon (PDiscrete 0)

-- | Add two 'PDiscrete' values of the same 'MoneyClass'.
paddDiscrete :: Term s (PDiscrete mc :--> PDiscrete mc :--> PDiscrete mc)
paddDiscrete = phoistAcyclic $
  -- In the future, this should use plutarch-numeric
  plam $ \x y -> P.do
    PDiscrete x' <- pmatch x
    PDiscrete y' <- pmatch y
    pcon (PDiscrete $ x' + y')

-- | The MoneyClass of LQ.
type LQ :: MoneyClass
type LQ = '("da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24", "LQ", 6)

-- | The MoneyClass of ADA.
type ADA :: MoneyClass
type ADA = '("", "", 6)

--------------------------------------------------------------------------------

-- | Downcast a `PValue` to a `PDiscrete` unit.
pvalueDiscrete ::
  forall (moneyClass :: MoneyClass) (ac :: Symbol) (n :: Symbol) (scale :: Nat) s.
  ( KnownSymbol ac
  , KnownSymbol n
  , moneyClass ~ '(ac, n, scale)
  ) =>
  Term s (PValue :--> PDiscrete moneyClass)
pvalueDiscrete = phoistAcyclic $
  plam $ \f ->
    pcon . PDiscrete $
      passetClassValueOf # pconstant (fromString $ symbolVal $ Proxy @ac)
        # pconstant (fromString $ symbolVal $ Proxy @n)
        # f

{- | Get a `PValue` from a `PDiscrete`.
     __NOTE__: `pdiscreteValue` after `pvalueDiscrete` loses information
-}
pdiscreteValue ::
  forall (moneyClass :: MoneyClass) (ac :: Symbol) (n :: Symbol) (scale :: Nat) s.
  ( KnownSymbol ac
  , KnownSymbol n
  , moneyClass ~ '(ac, n, scale)
  ) =>
  Term s (PDiscrete moneyClass :--> PValue)
pdiscreteValue = phoistAcyclic $
  plam $ \f -> pmatch f $ \case
    PDiscrete p ->
      psingletonValue
        # pconstant (fromString $ symbolVal $ Proxy @ac)
        # pconstant (fromString $ symbolVal $ Proxy @n)
        # p

Effects are changes to a protocol bundled in a proposal, which will be
enacted if said proposal is passed by the DAO. The method by which we
empower effects to make changes to the system, means that effects are
very powerful and could cause harm, if misused. This guide will
provide guidance for constructing these effects.

# Making an Effect

Effects will only be run once and all of the data they require must be
kept in their datums, and not their redeemers. Therefore, it makes
sense for us to start this discussion with datums. It is recommended
to have your effect datums at the Haskell and Plutarch levels. These
levels can be bridged with Plutarch\'s [PConstant and
PLift](https://github.com/Plutonomicon/plutarch/blob/master/docs/Typeclasses/PConstant%20and%20PLift.md).
A `PTryFrom` instance is also required to parse raw \`PData\` from
validator into the specified datum of effect Validator.

## Effect Datum

This section explains basic structure of effect datum with `Treasury
Withdrawal Effect`. Remeber that datums are specific to each effect;
new datum needs to be redesigned and constructed specifically for
other effects.


First, the Haskell-level definition of Datum.

``` haskell
data TreasuryWithdrawalDatum = TreasuryWithdrawalDatum
  { receivers :: [(Credential, Value)]
  , treasuries :: [Credential]
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic)
```

Plutarch-level definition follows:

``` haskell
newtype PTreasuryWithdrawalDatum (s :: S)
  = PTreasuryWithdrawalDatum
      ( Term
          s
          ( PDataRecord
              '[ "receivers" ':= PBuiltinList (PAsData (PTuple PCredential PValue))
               , "treasuries" ':= PBuiltinList (PAsData PCredential)
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PTreasuryWithdrawalDatum
	
instance PUnsafeLiftDecl PTreasuryWithdrawalDatum where
  type PLifted PTreasuryWithdrawalDatum = TreasuryWithdrawalDatum
deriving via
  (DerivePConstantViaData TreasuryWithdrawalDatum PTreasuryWithdrawalDatum)
  instance
    (PConstant TreasuryWithdrawalDatum)	
```

Finally, `PTryFrom PData (Desired Datum type)`:

``` haskell
instance PTryFrom PData PTreasuryWithdrawalDatum where
  type PTryFromExcess PData PTreasuryWithdrawalDatum = Const ()
  ptryFrom' opq cont =
    -- this will need to not use punsafeCoerce...
    cont (punsafeCoerce opq, ())
```

> All informations above is **well** documented in
> [Plutonomicon/plutarch](https://github.com/Plutonomicon/plutarch/tree/master/docs)
> repository. Well, except `PTryFrom`...

## Effect Validator Boilerplate

In Agora, Effects can be built with the given `makeEffect`
function. This function is a simple boilerplate that checks the GAT
is handled correctly. This means that the creator of the effect needs
to only consider the implementation of their effect logic.

``` haskell
makeEffect ::
  forall (datum :: PType).
  (PIsData datum, PTryFrom PData datum) =>
  CurrencySymbol ->  
  -- ^ The Currency Symbol of DAO system
  (forall (s :: S). Term s PCurrencySymbol -> Term s datum -> Term s PTxOutRef -> Term s (PAsData PTxInfo) -> Term s POpaque) ->
  -- ^ Validator logic
  ClosedTerm PValidator
```

An example effect validator would be:

``` haskell
effectValidator :: forall {s :: S}. CurrencySymbol -> Term s PValidator
effectValidator currSymbol = makeEffect currSymbol $
  \_cs (datum' :: Term _ PYourEffectDatum) _txOutRef _txInfo -> P.do
    opaque (pconstant ())
```

Again, conveniently, Script Context is already stripped into `PTxOutRef`
and `PTxInfo`. `PTxInfo` will provide all informations about the
transaction: inputs, outputs, minted tokens, fees, and more. `PTxOutRef`
is the output reference for the effect signer, the one who starts the
effect by burning the GAT. It is easy to overlook it as useless, but, in
fact, It is very useful for

-   finding the address of effect script
-   checking who started the effect
-   accessing other informations

> Utility functions handling `PTxInfo` and `PTxInfo` is provided in
> Agora/Util.hs

## Effect Validator Logic

The most important piece of the validator has still not been
discussed: the validator logic. As explained above, validators ensure
transactions are built correctly and behave as desired.

Normal onchain scripts target to be less strict in order to be useful
in various cases. That is not the case for effects. Effects need to be
specific to correctly filter out all ill-formed transactions. Being
less strict could easily impose vulnerability to entire system.

Whilst the logic of most validators will be specific to those
validators, there are some general points which should be considered
in the construction of every effect. Utility functions in `Agora.Util`
will assist a developer in ensuring that their effect abide by these
considerations. These include:

-   All effect transactions should burn *exactly one* GAT.
-   No transactions should result in funds being paid to an effect.
-   Effects concerning configuration of the governance system should
    not permit funds to be transferred between outputs.
-   Needed informations for effect should be explicitly provided by
    the effect datum. In other words, the redeemer should be
    discarded, or explicitly checked to carry no data.

It'd be impossible to describe step-by-step procedures for writing all
possible effects, however these are some guidelines that you may find
useful:

-   Validators should be specific enough to prevent unwanted
    behaviour; generality in validators leaves more room for accidents
    and exploitation.
-   Consider what data could be provided to validators through the
    datum. More and better information may prove helpful in writing
    more powerful effects.
-   Testing is vital. Consider testing with small unit tests
    throughout development.

> TODO:
> Add "Testing Strategies" section when property testing or
> Apropos get successfully integrated to Agora.
> 
> Add "Benchmarking Strategies" section when benchmarking system get
> implemented.


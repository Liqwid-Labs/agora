Effects describe a modification in the goverance system. Its job is to
overlook transaction and to validate that transaction is doing a correct
job. Thus, if an effect is compromised, the governance system will be
exposed to fraudulent transactions. Agora and this guide will provides
guidance for constructing such effects.

# Making an Effect

All effects are used once, and all required data will be sent via Datum,
not Redeemer. So, at first, one must setup a datum for an effect. It is
recommanded to have both Haskell-level(Plutus style) and plutarch-level
definition of the datum. These two can be bridge with Plutarch\'s
[PConstant and
PLift](https://github.com/Plutonomicon/plutarch/blob/master/docs/Typeclasses/PConstant%20and%20PLift.md).
Also `PTryFrom` instance is also required to parse raw \`PData\` from
validator into the specified datum of effect Validator.

## Effect Datum

First, the Haskell-level definition of Datum--Plutus style.

``` haskell
data TreasuryWithdrawalDatum = TreasuryWithdrawalDatum
  { receivers :: [(Credential, Value)]
  , treasuries :: [Credential]
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic)
```

Plutarch-level definition follows. Plutarch provides deriving strategy
of for `PlutusType` and `PIsData` that conveniently defines bridge
between `PData` and `PTreasurywithdrawaldatum`.

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
```

Following is used to create `PConstant` and `PLift`: a bridge between
Haskell-level `TreasuryWithdrawalDatum` and Plutarch-level
`TreasuryWithdrawalDatum`

``` haskell
instance PUnsafeLiftDecl PTreasuryWithdrawalDatum where
  type PLifted PTreasuryWithdrawalDatum = TreasuryWithdrawalDatum
deriving via
  (DerivePConstantViaData TreasuryWithdrawalDatum PTreasuryWithdrawalDatum)
  instance
    (PConstant TreasuryWithdrawalDatum)
```

Finally, `PTryFrom PData (Desired Datum type)` is required.
~~`punsafeCoerce` might not be the best~~

``` haskell
instance PTryFrom PData PTreasuryWithdrawalDatum where
  type PTryFromExcess PData PTreasuryWithdrawalDatum = Const ()
  ptryFrom' opq cont =
    -- this will need to not use punsafeCoerce...
    cont (punsafeCoerce opq, ())
```

> All informations above is ~~well~~ documented in
> [Plutonomicon/plutarch](https://github.com/Plutonomicon/plutarch/tree/master/docs)
> repository. Well, except `PTryFrom`...

## Effect Validator Boilderplate

In Agora, Effects can be built with given `makeEffect` function. This
function is a simple boilderplate that checks for correct handling of
GAT, leaving effect creator only with the effect logic itself.

``` haskell
makeEffect ::
  forall (datum :: PType).
  (PIsData datum, PTryFrom PData datum) =>
  CurrencySymbol ->
  (forall (s :: S). Term s PCurrencySymbol -> Term s datum -> Term s PTxOutRef -> Term s (PAsData PTxInfo) -> Term s POpaque) ->
  ClosedTerm PValidator
```

Above is the type signiture. currency symbol of governance system and a
function containing effect logic should be given as arguments. For
example

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

## Effect Validator Logics

The most important piece of the validator is still not discussed: the
validator logic. As explained above validators ensures a transaction is
correctly built and will only have the wanted \"effect\". If an effect
fail to do that, it is a vulnerability to entire goverance system.

While most validator logics will have its own rules, there are some
general checks should be made. Utility functions given in
`Agora/Util.hs` will allow to build such general checks with ease. Such
as

-   All effects should burn GAT.
-   Transaction should never pay to effect.
-   For effects that only changes configuration of goverance system,
    they should not allow monetary transactions inside of it.
-   For effects involves other transaction should be given datum, with
    the list of expacted inputs and outputs, and strictly enforce the
    given datum.

It is difficult to describe all step-by-step rules for writing some of
more specific logics. However, there are some standards to follow

-   Validators should be specific enought to prevent possible attacks;
    generality in validators gives more oppertunities to exploit.
-   Consider what informations can be provided to validators through the
    datum. More specific and effective data will allow more specific
    logics.
-   Having simple tests(even a temporary unit test) to check along the
    way is a good idea.

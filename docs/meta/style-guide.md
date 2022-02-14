# Agora docs style guide

This document includes a couple of notes on how Agora documentation should be written and formatted.

## British/American spelling and grammatical differences

The difference between British and American English is wider than a lot of people presume. Authors are permitted to use whichever of the two they learned and therefore feel more comfortable with. The only exception to this is when writing changes that would result in a 'mixing' of styles e.g. having 'color' in a sentence and 'colour' in the next. In this instance please alter your use of the language to maintain consistency.

## Capitalised words

The following words should always in the forms below:

-   Agora
-   Liqwid
-   LiqwidX
-   Nix
-   NixOS
-   Plutus
-   Plutarch

Sensible exceptions naturally exist, including referencing shell commands (`nix-shell`) or code:

```haskell
plutarchTerm :: Term s a 
plutarchTerm = ...
```

## Upper-case terms

The following terms should always be rendered in all capital letters:

-   UTXO
-   EUTXO

## Lower-case words

The following words should always be rendered lower-case (unless used at the beginning of a sentence):

-   governance

## ADA, Ada, ada

Cardano's native token suffers from a frustrating variety of acceptable forms. Reputable sources can be found using all three variants listed in the header. As such, no usage shall be mandated with two caveats:

1.  Whilst the name of the currency is subject to variance, its 'symbol' is unambiguously 'ADA'. Therefore when talking about amounts e.g. 'Trillian deposits 100ADA', use 'ADA'.
2.  Exercise _reasonable consistency_. The use of an 'Ada' at the beginning of the document and an 'ada' 3000 words later is not something worth losing sleep over. Inconsistency within paragraphs or (gasp!) _sentences_ should always be avoided.

## Avoid

The following practices should be avoided:

-   The use of '&' apart from in proper nouns such as AT\&T.

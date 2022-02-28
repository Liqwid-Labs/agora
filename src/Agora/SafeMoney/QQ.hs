{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.SafeMoney.QQ
Maintainer : emi@haskell.fyi
Description: Quasiquoter for SafeMoney types
-}
module Agora.SafeMoney.QQ (discrete) where

import GHC.Real (Ratio ((:%)))
import Language.Haskell.TH qualified as TH (Type)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax (
  Dec (TySynD),
  Exp (AppE, AppTypeE, LitE, VarE),
  Info (TyConI),
  Lit (IntegerL),
  Pat,
  Q,
  TyLit (NumTyLit, StrTyLit),
  Type (AppT, ConT, LitT, PromotedTupleT),
  lookupTypeName,
  reify,
 )
import Text.ParserCombinators.ReadP (readP_to_S, skipSpaces)
import Text.Read (lexP, readPrec_to_P)
import Text.Read.Lex (Lexeme (Ident, Number), Number, numberToFixed, numberToRational)
import Prelude

--------------------------------------------------------------------------------

import Plutarch.Internal (punsafeCoerce)

import Agora.SafeMoney (MoneyClass, PDiscrete)

--------------------------------------------------------------------------------

{- | Generate 'PDiscrete' values tagged by a particular MoneyClass

@
  [discrete| 123.456 ADA |] :: 'Term' s ('PDiscrete' 'ADA')
@
-}
discrete :: QuasiQuoter
discrete = QuasiQuoter discreteExp errorDiscretePat errorDiscreteType errorDiscreteDiscretelaration

discreteConstant :: forall (moneyClass :: MoneyClass) s. Integer -> Term s (PDiscrete moneyClass)
discreteConstant n = punsafeCoerce (pconstant n :: Term s PInteger)

fixedToInteger :: Integer -> (Integer, Integer) -> Integer
fixedToInteger places (i, f) = i * 10 ^ places + f

safeIntegerUpcast :: Integer -> Number -> Either String Integer
safeIntegerUpcast places num =
  case (numberToFixed places num, numberToRational num * 10 ^ places) of
    (Just (i, f), _n :% 1) ->
      Right $ fixedToInteger places (i, f)
    (Just (i, f), _n :% _d) ->
      Left $ "Using more than the available decimal places (" <> show places <> "). Would round to " <> show i <> "." <> show f
    _ -> Left "Some error occurred while getting number"

discreteExp :: String -> Q Exp
discreteExp s = case parseDiscreteRatioExp s of
  Nothing ->
    fail $ "Input malformed. Got: " <> s
  Just (num, mc) -> do
    mcName <-
      lookupTypeName mc >>= \case
        Nothing -> fail $ "MoneyClass with the name " <> show mc <> " is not in scope."
        Just v -> pure v
    reified <- reify mcName
    case reified of
      TyConI (TySynD tyName [] (AppT (AppT (AppT (PromotedTupleT 3) (LitT (StrTyLit _))) (LitT _)) (LitT (NumTyLit n)))) ->
        case safeIntegerUpcast n num of
          Right i ->
            pure $ AppE (AppTypeE (VarE 'discreteConstant) (ConT tyName)) (LitE (IntegerL i))
          Left e -> fail e
      ty' -> fail $ "Could not reify type, got: " <> show ty'

parseDiscreteRatioExp :: String -> Maybe (Number, String)
parseDiscreteRatioExp s =
  let p = skipSpaces *> ((,) <$> readPrec_to_P lexP 0 <* skipSpaces <*> readPrec_to_P lexP 0) <* skipSpaces
   in case readP_to_S p s of
        [((Number n, Ident i), "")] -> Just (n, i)
        _ -> Nothing

errorDiscretePat :: String -> Q Pat
errorDiscretePat _ = fail "Cannot use 'discrete' in a pattern context."

errorDiscreteType :: String -> Q TH.Type
errorDiscreteType _ = fail "Cannot use 'discrete' in a type context."

errorDiscreteDiscretelaration :: String -> Q [Dec]
errorDiscreteDiscretelaration _ = fail "Cannot use 'discrete' in a declaration context."

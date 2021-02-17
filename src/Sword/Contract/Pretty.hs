module Sword.Contract.Pretty
  ( prettyContract,
    prettyContractN,
  )
where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import Numeric.Natural (Natural)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Sword.Contract
import Sword.Time

-- | Prettyprint a 'Contract' using a particular column width.
prettyContractN :: Int -> Contract word -> Text
prettyContractN = prettyContract' . withColumn
  where
    withColumn columnWidth =
      LayoutOptions
        { layoutPageWidth = AvailablePerLine columnWidth 1
        }

-- | Prettyprint a 'Contract' using 'defaultLayoutOptions'.
prettyContract :: Contract word -> Text
prettyContract = prettyContract' defaultLayoutOptions

-- | Prettyprint a 'Contract' given some 'LayoutOptions'.
prettyContract' :: LayoutOptions -> Contract word -> Text
prettyContract' options = renderStrict . layoutPretty options . prettyC

prettyC :: Contract word -> Doc ann
prettyC = \case
  Zero ->
    prettyText "zero"

  Transfer (Asset asset) (Party party) ->
    prettyFun "transfer" [prettyText asset, prettyText party]

  Scale factor contract ->
    prettyFun "scale" [prettyExpr factor, prettyC contract]

  Both contract1 contract2 ->
    prettyFun "both" [prettyC contract1, prettyC contract2]

  Delay time contract ->
    prettyFun "delay" [prettyTime time, prettyC contract]

  IfWithin cond time thenContract elseContract ->
    prettyText "if"
      <+> prettyExpr cond
      <+> prettyText "within"
      <+> prettyTime time
      <+> prettyText "then"
      <+> prettyC thenContract
      <+> prettyText "else"
      <+> prettyC elseContract

prettyTime :: SwordDiffTime -> Doc ann
prettyTime = hsep . punctuate comma . List.map f . Map.toDescList . unSwordDiffTime
  where
    f :: (TimeUnit, Natural) -> Doc ann
    f (unit, qty) = prettyNatural qty <+> prettyText (timeUnitText <> plural)
      where
        plural = if qty == 1 then "" else "s"
        timeUnitText = case unit of
          Seconds -> "second"
          Minutes -> "minute"
          Hours -> "hour"
          Days -> "day"
          Weeks -> "week"

prettyExpr :: Expr word -> Doc ann
prettyExpr e = case e of
  Const _word -> prettyText "CONSTANT"
  Bool True -> prettyText "true"
  Bool False -> prettyText "false"
  Get _oracle -> prettyText "ORACLE"
  Min e1 e2 -> prettyFun "min" (prettyExpr <$> [e1, e2])
  Max e1 e2 -> prettyFun "max" (prettyExpr <$> [e1, e2])
  Add e1 e2 -> prettyBinOp e e1 e2
  Sub e1 e2 -> prettyBinOp e e1 e2
  Mul e1 e2 -> prettyBinOp e e1 e2
  Div e1 e2 -> prettyBinOp e e1 e2
  Eq e1 e2 -> prettyBinOp e e1 e2
  Lt e1 e2 -> prettyBinOp e e1 e2
  Gt e1 e2 -> prettyBinOp e e1 e2
  Geq e1 e2 -> prettyBinOp e e1 e2
  Leq e1 e2 -> prettyBinOp e e1 e2
  And e1 e2 -> prettyBinOp e e1 e2
  Or e1 e2 -> prettyBinOp e e1 e2

prettyBinOp :: Expr word -> Expr word -> Expr word -> Doc ann
prettyBinOp parentExpr leftExpr rightExpr =
  prettyOp leftExpr True
    <+> pretty op
    <+> prettyOp rightExpr False
  where
    prettyOp :: Expr word -> Bool -> Doc ann
    prettyOp e isLeft
      | precedence e < precedence parentExpr = prettyExpr e
      | precedence e == precedence parentExpr && isLeftAssociative e isLeft = prettyExpr e
      | otherwise = parens (prettyExpr e)

    isLeftAssociative :: Expr word -> Bool -> Bool
    isLeftAssociative e isLeft =
      isLeft && isAssociative e && isAssociative parentExpr

    op :: Text
    op = case parentExpr of
      Add {} -> "+"
      Sub {} -> "-"
      Mul {} -> "*"
      Div {} -> "/"
      Eq {} -> "="
      Lt {} -> "<"
      Gt {} -> ">"
      Geq {} -> ">="
      Leq {} -> "<="
      And {} -> "&&"
      Or {} -> "||"

precedence :: Expr word -> Int
precedence = \case
  Const {} -> 0
  Bool {} -> 0
  Get {} -> 0
  Min {} -> 0
  Max {} -> 0
  Not {} -> 0
  Mul {} -> 1
  Div {} -> 1
  Add {} -> 2
  Sub {} -> 2
  Eq {} -> 3
  Lt {} -> 4
  Gt {} -> 4
  Leq {} -> 4
  Geq {} -> 4
  And {} -> 5
  Or {} -> 6
  If {} -> 7

isAssociative :: Expr word -> Bool
isAssociative = \case
  Mul {} -> True
  Add {} -> True
  _ -> False

-- FIXME: Make this look actually pretty.
prettyFun :: Text -> [Doc ann] -> Doc ann
prettyFun name args = prettyText name <> parens (commas args)

commas :: [Doc ann] -> Doc ann
commas = sep . punctuate comma

prettyNatural :: Natural -> Doc ann
prettyNatural = pretty @Natural

prettyText :: Text -> Doc ann
prettyText = pretty @Text

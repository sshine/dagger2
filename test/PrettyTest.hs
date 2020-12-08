module PrettyTest where

import Sword.Contract
import Sword.Contract.Pretty
import Test.Tasty.Hspec

spec_prettyContract_transfer_wraps :: Spec
spec_prettyContract_transfer_wraps =
  describe "prettyContract" $ do
    it "prints on a single line when it fits exactly" $
      prettyContractN 20 (Transfer (Asset "USD") (Party "Simon"))
        `shouldBe` "transfer(USD, Simon)"

    it "prints across lines with indentation when it doesn't fit on one line" $
      prettyContractN 19 (Transfer (Asset "USD") (Party "Simon"))
        `shouldBe` "transfer(\n  USD,\n  Simon)"
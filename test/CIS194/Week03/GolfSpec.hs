module CIS194.Week03.GolfSpec where

import CIS194.Week03.Golf
import Test.Hspec

spec :: Spec
spec = do
  describe "skips" $ do
    it "convert a list into a list of lists" $ do
      skips "ABCD"   `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1]      `shouldBe` [[1]]
      skips [True, False] `shouldBe` [[True, False], [False]]

  describe "localMaxima" $
    it "finds a local maxima of a given list" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      localMaxima [2,3,4,1,5] `shouldBe` [4]
      localMaxima [1,2,3,4,5] `shouldBe` []

  describe "histogram" $
    it "renders a histogram for a given list" $ do
      histogram [1,1,1,5]                `shouldBe` unlines [ " *        "
                                                            , " *        "
                                                            , " *   *    "
                                                            , "=========="
                                                            , "0123456789"
                                                            ]
      histogram  [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` unlines [ "    *     "
                                                            , "    *     "
                                                            , "    * *   "
                                                            , " ******  *"
                                                            , "=========="
                                                            , "0123456789"
                                                            ]


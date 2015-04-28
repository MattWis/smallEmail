{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import Control.Monad (liftM)
import qualified Web.Scotty as S
import ParseEmail


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "subject" $ do
    it "parses the subject" $ do
      liftM getSubject email1 `shouldReturn` subject1
      liftM getSubject email2 `shouldReturn` subject2
      liftM getSubject email3 `shouldReturn` subject3

email1 = readFile "email1.txt"
email2 = readFile "email2.txt"
email3 = readFile "email3.txt"

subject1 = "Google Account: access for less secure apps has been enabled"
subject2 = "I'll be blowing up your inbox"
subject3 = "Attachment tests"

getSubject :: String -> String
getSubject text = case parseEmail text of
                    Left error -> ""
                    Right email -> case subject email of
                                     Left error -> ""
                                     Right sub -> sub

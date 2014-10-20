{-# LANGUAGE TupleSections, OverloadedStrings #-}

import System.IO
import Network.HaskellNet.IMAP.SSL (connectIMAPSSLWithSettings, defaultSettingsIMAPSSL)
import Network.HaskellNet.SSL (sslMaxLineLength)
import Network.HaskellNet.IMAP (login, select, search, list, logout, fetchByString, SearchQuery(ALLs))
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.IMAP.Types (UID)
import Data.Text.Internal.Lazy
import Data.Text (split)
import Data.String (fromString)
import Network.Mail.Client.Gmail (sendGmail)
import Network.Mail.Mime
import qualified Data.ByteString.Char8 as B
import Data.Char
import Control.Monad (liftM)
import Data.List (isInfixOf)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Text.ParserCombinators.Parsec

import Debug.Trace
traceThis x = trace (show x) x

-- Double credentials because the incoming and outgoing libs have different
-- types
username = "olinemailbot@gmail.com"
password = "haskell!"

name = "olinemailbot" :: Text
passwordT = "haskell!" :: Text

mailList = Address Nothing "olinemailbot@gmail.com"
to = Address Nothing "luke.s.metz@gmail.com"

main = getNewEmailsForever 3

getNewEmailsForever num = do
  (numMsgs, emails) <- getEmailsAfter num
  print emails
  print numMsgs

  {-_ <- mapM_ (\(x, y) -> sendGmail name passwordT mailList [to] [] [] (fromString x) (fromString y) []) emails-}

  getNewEmailsForever numMsgs


{-getEmailsAfter :: Int -> IO (Int, [(String, String)])-}
getEmailsAfter numKnown = do
  conn <- connectIMAPSSLWithSettings "imap.gmail.com" cfg
  login conn username password
  mboxes <- list conn
  select conn "INBOX"
  msgs <- search conn [ALLs]
  let newMessages = drop numKnown msgs
  list <- fetchEmail conn (head newMessages)
  logout conn
  return (length msgs, list)
  where cfg = defaultSettingsIMAPSSL { sslMaxLineLength = 100000 }

{-fetchEmails :: IMAPConnection -> [UID] -> IO [(String, String)]-}
{-fetchEmails conn [] = return $ Right [[]]-}
fetchEmail conn message = do
  body <- fetchByString conn message "BODY[]"
  return (parseEmail . snd . head $ body)

data Email = Email Header Content deriving (Eq, Show)
type Header = [String]
data Content = Content ContentType ContentBody deriving (Eq, Show)
type ContentType = String
data ContentBody = Multipart [Content] | Text [String] | OtherType [String] deriving (Eq, Show)

{-parseEmail :: String -> Either ParseError [String]-}
parseEmail = parse (emailFormat Nothing) "(unknown)"

emailFormat boundary = do
  header <- manyTill line $ try (string "Content-Type: ")
  contentType <- manyTill anyChar $ string "; "
  body <- emailContent contentType boundary
  return body


emailContent contentType boundary =
  if "multipart" `isInfixOf` contentType
  then do
    manyTill anyChar $ try (string "boundary=")
    thisBoundary <- manyTill anyChar eol
    newBoundary <- return $ maybe [thisBoundary] (thisBoundary :) boundary
    eol
    body <- multipart $ Just newBoundary
    return $ Content contentType body
  else do
    content <- notBoundaryLines boundary
    return $ Content contentType $ OtherType content

multipart boundary = do
  contents <- manyTill (emailFormat boundary) eof
  return $ Multipart contents

line = manyTill anyChar eol

notBoundaryLines boundary = do
  curLine <- line
  if maybeInfix curLine boundary
  then return []
  else notBoundaryLines boundary >>= (\lines -> return $ curLine : lines)

maybeInfix :: String -> Maybe [String] -> Bool
maybeInfix string = maybe False ((any . flip isInfixOf) string)

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

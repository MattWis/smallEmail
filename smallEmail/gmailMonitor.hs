{-# LANGUAGE TupleSections, OverloadedStrings #-}

import Network.HaskellNet.IMAP.SSL (connectIMAPSSLWithSettings, defaultSettingsIMAPSSL)
import Network.HaskellNet.SSL (sslMaxLineLength)
import Network.HaskellNet.IMAP (login, select, search, list, logout, fetchByString, SearchQuery(ALLs))
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.IMAP.Types (UID)
import Network.Mail.Client.Gmail (sendGmail)
import Network.Mail.Mime (Address(Address))

import Data.Text.Internal.Lazy
import Data.Text (split)
import Data.String (fromString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Control.Monad (liftM)

import Text.ParserCombinators.Parsec (parse, manyTill, anyChar, try, string, eof, (<?>), (<|>))
import Text.Parsec.Prim (ParsecT)
import Text.Parsec.Error (ParseError)
import Data.Functor.Identity (Identity)

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

getNewEmailsForever :: Int -> IO ()
getNewEmailsForever num = do
  (numMsgs, emails) <- getEmailsAfter num
  print emails
  print numMsgs

  {-_ <- mapM_ (\(x, y) -> sendGmail name passwordT mailList [to] [] [] (fromString x) (fromString y) []) emails-}

  getNewEmailsForever numMsgs


getEmailsAfter :: Int -> IO (Int, [Either ParseError Email])
getEmailsAfter numKnown = do
  conn <- connectIMAPSSLWithSettings "imap.gmail.com" cfg
  login conn username password
  mboxes <- list conn
  select conn "INBOX"
  msgs <- search conn [ALLs]
  let newMessages = drop numKnown msgs
  list <- mapM (fetchEmail conn) newMessages
  logout conn
  return (length msgs, list)
  where cfg = defaultSettingsIMAPSSL { sslMaxLineLength = 100000 }

fetchEmail :: IMAPConnection -> UID -> IO (Either ParseError Email)
fetchEmail conn message = do
  body <- fetchByString conn message "BODY[]"
  return (parseEmail . snd . head $ body)

data Email = Email Header Content deriving (Eq, Show)
type Header = [String]
data Content = Content ContentType ContentBody deriving (Eq, Show)
type ContentType = String
data ContentBody = Multipart [Content] | Singlepart [String] deriving (Eq, Show)

parseEmail :: String -> Either ParseError Email
parseEmail = parse emailFormat "(unknown)"

emailFormat :: ParsecT [Char] u Identity Email
emailFormat = do
  header <- manyTill line $ try (string "Content-Type: ")
  contentType <- manyTill anyChar $ string "; "
  body <- emailContent contentType Nothing
  return $ Email header body

contentFormat :: Maybe [String] -> ParsecT [Char] u Identity Content
contentFormat boundary = do
  header <- manyTill line $ try (string "Content-Type: ")
  contentType <- manyTill anyChar $ string "; "
  body <- emailContent contentType boundary
  return body


emailContent :: ContentType -> Maybe [String] -> ParsecT [Char] u Identity Content
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
    return $ Content contentType $ Singlepart content

multipart :: Maybe [String] -> ParsecT [Char] u Identity ContentBody
multipart boundary = do
  contents <- manyTill (contentFormat boundary) eof
  return $ Multipart contents

line :: ParsecT [Char] u Identity [Char]
line = manyTill anyChar eol

notBoundaryLines :: Maybe [String] -> ParsecT [Char] u Identity [[Char]]
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

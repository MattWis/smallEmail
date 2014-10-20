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
import Data.ByteString (ByteString)
import Control.Monad.IO.Class

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
  {-print emails-}
  print numMsgs

  {-_ <- mapM_ (\(x, y) -> sendGmail name passwordT mailList [to] [] [] (fromString x) (fromString y) []) emails-}

  getNewEmailsForever numMsgs


getEmailsAfter :: Int -> IO (Int, [(String, String)])
getEmailsAfter numKnown = do
  conn <- connectIMAPSSLWithSettings "imap.gmail.com" cfg
  login conn username password
  mboxes <- list conn
  select conn "INBOX"
  msgs <- search conn [ALLs]
  let newMessages = drop numKnown msgs
  list <-  fetchEmails conn newMessages
  logout conn
  return (length msgs, list)
  where cfg = defaultSettingsIMAPSSL { sslMaxLineLength = 100000 }

fetchEmails :: IMAPConnection -> [UID] -> IO [(String, String)]
fetchEmails conn [] = return $ []
fetchEmails conn messages = do
    body <- fetchByString conn (head messages) "BODY[]"
    envelope <- fetchByString conn (head messages) "ENVELOPE"
    others <- fetchEmails conn (tail messages)
    (liftIO . putStrLn . snd . head) body
    return ((getSubject envelope, snd . head $ body) : others)

getSubject :: [(String, String)] -> String
getSubject = filter (/= '"') . show . (!! 3) . split (\x -> x == '"') . fromString . snd . head . tail


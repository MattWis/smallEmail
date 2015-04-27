{-# LANGUAGE TupleSections, OverloadedStrings, NoImplicitPrelude, GADTs #-}

import ClassyPrelude
import Network.HaskellNet.IMAP.SSL (connectIMAPSSLWithSettings, defaultSettingsIMAPSSL)
import Network.HaskellNet.SSL (sslMaxLineLength)
import Network.HaskellNet.IMAP (login, select, search, list, logout, fetchByString, SearchQuery(ALLs))
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.IMAP.Types (UID)
import Network.Mail.Client.Gmail (sendGmail)
import Network.Mail.Mime (Address(Address))

import Data.Text (split)
import qualified Data.Text.Internal.Lazy as L
import Data.String (fromString)
import Control.Monad (liftM)
import Control.Concurrent (threadDelay)

import Text.Parsec.Error (ParseError)

import ParseEmail

-- Double credentials because the incoming and outgoing libs have different
-- types
username = "olinemailbot@gmail.com"
password = "haskell!"

name = "olinemailbot" :: L.Text
pass = "haskell!" :: L.Text

mailList = Address Nothing "olinemailbot@gmail.com"
to = Address Nothing "mattwis86@gmail.com"

main = getNewEmailsForever 2

getNewEmailsForever :: Int -> IO ()
getNewEmailsForever num = do
  (numMsgs, emails) <- getEmailsAfter num
  {-print (mapM (liftM (getAttachments . flatten)) emails)-}
  print numMsgs
  print (map (liftM subject) emails)
  print (map (liftM (getPart "text/plain")) emails)

  {-_ <- mapM forwardGmail emails-}
  threadDelay 10000000 --10 seconds

  getNewEmailsForever numMsgs

forwardGmail :: Either ParseError Email -> IO()
forwardGmail (Left x) = do (return ())
forwardGmail (Right email) = case subject email of
                             Left x -> do (return ())
                             Right sub -> let html = getPart "text/plain" email
                                          in  send (fromString sub) (fromString html)
  where send sub html = sendGmail name pass mailList [to] [] [] sub html [] 10000000

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
  return (parseEmail $ (traceShowId . extractBody) body)


extractBody body = case headMay body of
    Just safeBody -> snd safeBody
    Nothing -> ""

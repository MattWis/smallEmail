{-# LANGUAGE TupleSections, OverloadedStrings, NoImplicitPrelude, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

import Import (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings, Key(..))

import ClassyPrelude
import Network.HaskellNet.IMAP.SSL (connectIMAPSSLWithSettings, defaultSettingsIMAPSSL)
import Network.HaskellNet.SSL (sslMaxLineLength)
import Network.HaskellNet.IMAP (login, select, search, list, logout, fetchByString, SearchQuery(ALLs))
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.IMAP.Types (UID)
import Network.Mail.Client.Gmail (sendGmail)
import Network.Mail.Mime (Address(Address))

import Database.Persist.Sqlite (insert, runMigration, runSqlite)
import Text.Parsec.Error (ParseError)

import ParseEmail

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
AttachedFile
    extension Text
    headers Text
    fileData Text
|]

-- Double credentials because the incoming and outgoing libs have different
-- types
username = "olinemailbot@gmail.com"
password = "haskell!"

name = "olinemailbot" :: Text
passwordT = "haskell!" :: Text

mailList = Address Nothing "olinemailbot@gmail.com"
to = Address Nothing "luke.s.metz@gmail.com"

main = getNewEmailsForever 2

getNewEmailsForever :: Int -> IO ()
getNewEmailsForever num = do
  (numMsgs, emails) <- getEmailsAfter num
  attachments <- return . (liftM concat) $ mapM (liftM (getAttachments . flatten)) emails

  print attachments
  print numMsgs

  keys <- infectIO $! insertAttachments attachments
  print keys

  {-_ <- mapM_ (\(x, y) -> sendGmail name passwordT mailList [to] [] [] (fromString x) (fromString y) []) emails-}

  getNewEmailsForever numMsgs

infectIO :: Either b (IO a) -> IO (Either b a)
infectIO (Right x) = (liftM Right) x
infectIO (Left y) = return (Left y)

insertAttachments :: Either ParseError [Attachment] -> Either ParseError (IO [Key AttachedFile])
insertAttachments = (liftM . (liftM sequence) . map) insertAttachment

insertAttachment :: Attachment -> IO (Key AttachedFile)
insertAttachment (Attachment extension headers fileData) =
  runSqlite "../smallEmail.sqlite3" $! do
    runMigration migrateAll
    insert (AttachedFile (pack extension) (pack $ intercalate "\n" headers) (pack fileData))

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
  return (parseEmail $ extractBody body)


extractBody body = case headMay body of
    Just safeBody -> snd safeBody
    Nothing -> ""

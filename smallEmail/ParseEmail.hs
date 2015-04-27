{-# LANGUAGE TupleSections, OverloadedStrings, NoImplicitPrelude #-}

module ParseEmail
  ( parseEmail
  , flatten
  , getAttachments
  , Email()
  ) where

import ClassyPrelude hiding (try, (<|>))
import Prelude (tail)

import Text.ParserCombinators.Parsec (parse, manyTill, anyChar, try, string, eof, (<?>), (<|>))
import Text.Parsec.Prim (ParsecT)
import Text.Parsec.Error (ParseError)
import Data.Functor.Identity (Identity)

data Email = Email [String] Content deriving (Eq, Show)
data Content = Multipart String [Content]
             | Singlepart String [String] deriving (Eq, Show)
data Attachment = Attachment {
  extension :: String,
  headers :: [String],
  fileData :: String
} deriving (Eq, Ord, Show)


flatten :: Email -> [Content]
flatten (Email header content) = flattenContent content

flattenContent :: Content -> [Content]
flattenContent content = case content of
  Multipart _ contents -> concatMap flattenContent contents
  Singlepart _ _ -> [content]

getAttachments :: [Content] -> [Attachment]
getAttachments = mapMaybe convertToAttachment

convertToAttachment :: Content -> Maybe Attachment
convertToAttachment content = case content of
  Multipart contentType contents -> Nothing
  Singlepart contentType headersAndData -> case headMay headersAndData of
    Nothing -> Nothing
    Just firstLine -> if not ("name" `isInfixOf` firstLine)
      then Nothing
      else let fileData = tail $ dropWhile (/= "") headersAndData
               headers = takeWhile (/= "") headersAndData
           in Just $ Attachment contentType headers (concat fileData)


parseEmail :: String -> Either ParseError Email
parseEmail = parse emailFormat "(unknown)"

emailFormat :: ParsecT [Char] u Identity Email
emailFormat = do
  (header, contentType) <- getHeaders
  body <- emailContent contentType Nothing
  return $ Email header body

contentFormat :: Maybe [String] -> ParsecT [Char] u Identity Content
contentFormat boundary = do
  (header, contentType) <- getHeaders
  body <- emailContent contentType boundary
  return body

getHeaders :: ParsecT [Char] u Identity ([[Char]], [Char])
getHeaders = do
  header <- manyTill line $ try (string "Content-Type: ")
  contentType <- manyTill anyChar $ string "; "
  return (header, contentType)


emailContent :: String -> Maybe [String] -> ParsecT [Char] u Identity Content
emailContent contentType boundary =
  if "multipart" `isInfixOf` contentType
  then do
    manyTill anyChar $ try (string "boundary=")
    thisBoundary <- manyTill anyChar eol
    newBoundary <- return $ maybe [thisBoundary] (thisBoundary :) boundary
    eol
    body <- multipart $ Just newBoundary
    return $ Multipart contentType body
  else do
    content <- notBoundaryLines boundary
    return $ Singlepart contentType content

multipart :: Maybe [String] -> ParsecT [Char] u Identity [Content]
multipart boundary = do
  contents <- manyTill (contentFormat boundary) eof
  return contents

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

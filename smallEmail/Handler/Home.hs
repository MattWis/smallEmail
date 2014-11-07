{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )
import Database.Persist.Sqlite (runMigration, runSqlite)
import Debug.Trace (trace)
-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost inputForm
    let submission = Nothing :: Maybe (Text, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Small Email"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost inputForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    _ <- ($) runSqlite "smallEmail.sqlite3" $ do
      runMigration migrateAll
      case submission of
        Just (email, list) -> insert $ traceThis (EmailEntry email list)
        Nothing -> insert $ traceThis (EmailEntry "" "") -- Should never happen

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Small Email"
        $(widgetFile "homepage")

traceThis :: (Show a) => a -> a
traceThis a = trace (show a) a

emailPrompt :: FieldSettings site
emailPrompt = "What's your email address?"

listPrompt :: FieldSettings site
listPrompt = "What list do you want to follow?"

inputForm :: Form (Text, Text)
inputForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (withSmallInput emailPrompt) Nothing
    <*> areq textField (withSmallInput listPrompt) Nothing

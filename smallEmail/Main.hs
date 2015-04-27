{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import qualified Web.Scotty as S
import qualified Text.Blaze.Html.Renderer.Text as B
import Text.Blaze.Html5 (html, (!), input, p, form)
import qualified Text.Blaze.Html5.Attributes as A
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Sqlite
import Data.Text (Text)
import Data.Time (getCurrentTime, UTCTime)
import System.IO.Unsafe (unsafePerformIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Link
    email Text
    list Text
    createdAt UTCTime
    deriving Show
|]

main = S.scotty 3000 $ do
  S.get "/" $ do
    blaze renderRoot
  S.post "/signup" $ do
    email <- S.param "Email"
    list <- S.param "List"
    runSqlite "smallEmail.sqlite3" $ do
      runMigration migrateAll
      insert $ Link email list (unsafePerformIO getCurrentTime)
    S.html "Thank you for the submission!"

blaze = S.html . B.renderHtml

renderRoot = html $ do
      p "Submit your email address and a mailman list to never have to worry \
        \about large attachments again!"
      form ! A.action "/signup" ! A.method "post" $ do
        input ! A.type_ "email" ! A.name "Email" ! A.placeholder "Email"
        input ! A.type_ "email" ! A.name "List" ! A.placeholder "List"

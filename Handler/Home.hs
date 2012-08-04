{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Graphics.Rendering.Cairo
import qualified Data.Text as T

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

getImageGenR :: Handler RepHtml
getImageGenR = do
  (formWidget, formEnctype) <- generateFormPost cardTextForm
  defaultLayout $ do setTitle "Card text input"
                     $(widgetFile "cardInput")

postImageGenR :: Handler RepHtml
postImageGenR = do
  ((result,formWidget),formEnctype) <- runFormPost cardTextForm
  let cardText :: Text
      cardText = case result of
                   FormSuccess ct -> ct
                   _ -> "No text entered."

      action :: Render ()
      action = do setSourceRGBA 1 0 0 1
                  setFontSize 20 
                  moveTo 0 50
                  showText $ T.unpack cardText

  surf <- liftIO $ createImageSurface FormatARGB32 300 300
  liftIO $ renderWith surf action
  liftIO $ surfaceWriteToPNG surf "static/crapfest.png"

  let iroute = StaticRoute ["crapfest.png"] []

  defaultLayout $ do setTitle "Your card"
                     $(widgetFile "cardResult")

cardTextForm :: Form Text
cardTextForm = renderDivs $ 
    areq textField "Card text" Nothing

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

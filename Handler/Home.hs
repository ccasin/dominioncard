{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Font
import Graphics.Rendering.Pango.Cairo
import Graphics.Rendering.Pango.Layout
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
  let cardText :: String
      cardText = case result of
                   FormSuccess ct -> T.unpack ct
                   _ -> ""

      action :: Render ()
      action = do moveTo 40 37
                  fd <- liftIO $ fontDescriptionFromString "OptimusPrinceps 40"
                  pctx <- liftIO $ cairoCreateContext Nothing
                  lay <- liftIO $ layoutText pctx cardText
                  liftIO $ layoutSetWidth lay $ Just $ 507
                  liftIO $ layoutSetAlignment lay AlignCenter
                  liftIO $ layoutSetFontDescription lay $ Just fd
                  showLayout lay

  surf <- liftIO $ imageSurfaceCreateFromPNG "static/action.png"
  liftIO $ renderWith surf action
  liftIO $ surfaceWriteToPNG surf "static/crapfest.png"

  let iroute = StaticRoute ["crapfest.png"] []

  defaultLayout $ do setTitle "Your card"
                     $(widgetFile "cardResult")

cardTextForm :: Form Text
cardTextForm = renderDivs $ 
    areq textField "Card title" (Just "Title")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

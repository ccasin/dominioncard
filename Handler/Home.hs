{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Font
import Graphics.Rendering.Pango.Cairo
import Graphics.Rendering.Pango.Layout
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Cairo
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

defaultImage :: FilePath
defaultImage = "static/default.jpg"

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
                   FormSuccess (ct,_) -> T.unpack ct
                   _ -> "default"

      cardImagePath :: IO FilePath
      cardImagePath =
        case result of
          FormSuccess (_, Just (FileInfo {fileName,fileContent})) ->
            let fn = "settings/" ++ (T.unpack fileName) in
            do B.writeFile fn fileContent
               return fn
          _ -> return defaultImage

      drawUserImage :: Render ()
      drawUserImage =
          do cip <- liftIO cardImagePath
             pb <- liftIO $ pixbufNewFromFileAtScale cip 510 370 False
             setSourcePixbuf pb 38 95
             paint

      drawCardOverlay :: Render ()
      drawCardOverlay =
        do overlaySurf <- liftIO $ imageSurfaceCreateFromPNG "static/action.png"
           setSourceSurface overlaySurf 0 0
           rectangle 0 0 579 892
           fill

      makeTitleLayout :: IO PangoLayout
      makeTitleLayout = 
        do fd   <- fontDescriptionFromString "OptimusPrinceps 40"
           pctx <- cairoCreateContext Nothing
           lay  <- layoutText pctx cardText
           layoutSetWidth lay $ Just $ 507
           layoutSetAlignment lay AlignCenter
           layoutSetFontDescription lay $ Just fd
           return lay


      drawTitle :: Render ()
      drawTitle = do moveTo 40 37
                     lay <- liftIO makeTitleLayout
                     showLayout lay

  -- surf <- liftIO $ imageSurfaceCreateFromPNG "static/action.png"
  -- liftIO $ renderWith surf action
  cardSurf <- liftIO $ createImageSurface FormatARGB32 579 892
  liftIO $ renderWith cardSurf drawUserImage
  liftIO $ renderWith cardSurf drawCardOverlay
  liftIO $ renderWith cardSurf drawTitle
  liftIO $ surfaceWriteToPNG cardSurf "static/crapfest.png"

  let iroute = StaticRoute ["crapfest.png"] []

  defaultLayout $ do setTitle "Your card"
                     $(widgetFile "cardResult")

cardTextForm :: Form (Text, Maybe FileInfo)
cardTextForm = renderDivs $ (,)
    <$> areq textField "Card title" (Just "Title")
    <*> fileAFormOpt "Choose an image"

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

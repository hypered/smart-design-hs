{-|
Module: Smart.Html.Render
Description: Functions to render a Canvas to Html or Text.

-}
module Smart.Html.Render
  ( renderCanvas
  , renderCanvasFullScroll
  , renderCanvasText
  , smartDesignHead
  ) where

import qualified Data.Text                     as T
import qualified Smart.Html.Dsl                as Dsl
import qualified Text.Blaze.Html.Renderer.Pretty
                                               as R
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Render a Smart canvas as a complete HTML document, as Text.
renderCanvasText :: Dsl.HtmlCanvas -> T.Text
renderCanvasText = T.pack . R.renderHtml . renderCanvas

-- | Render a Smart canvas as a complete HTML document.
renderCanvas :: Dsl.HtmlCanvas -> H.Html
renderCanvas canvas = do
  H.docType
  H.html ! A.class_ "u-maximize-height" ! A.dir "ltr" ! A.lang "en" $ do
    smartDesignHead
    H.body ! A.class_ "u-maximize-height" $ H.toMarkup canvas >> js

-- | This must be combined with a "u-scroll-vertical" on the body, and no
-- "u-scroll-wrapper" on the main.
renderCanvasFullScroll :: Dsl.HtmlCanvas -> H.Html
renderCanvasFullScroll canvas = do
  H.docType
  H.html ! A.class_ "u-maximize-height" ! A.dir "ltr" ! A.lang "en" $ do
    smartDesignHead
    H.body
      !  A.class_ "u-maximize-height u-overflow-hidden"
      $  H.toMarkup canvas
      >> js


-- | Markup for the Smart CSS head etc.
smartDesignHead :: H.Html
smartDesignHead =
  H.head
    $  charset
    >> viewport
    >> title
    >> description
    >> maincss
    >> protocss
    >> custscss
 where
  charset = H.meta ! A.charset "utf-8"
  viewport =
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  title = H.title "Smart prototype" -- TODO Make it a parameter.
  description =                     -- TODO Make it a parameter.
    H.meta
      ! A.name "description"
      ! A.content
          "Curiosity is an ever-evolving prototype system to think, discuss, and communicate the future of Smart Belgium's developments."
  maincss  = stylesheet "main.css"
  protocss = stylesheet "prototype.css"
  custscss = stylesheet "styleguide-customizations.css"
  stylesheet file =
    H.link ! A.rel "stylesheet" ! A.href ("/static/css/" <> file)

-- | Markup for the Smart JS scripts.
js = do
  H.script ! A.src "/static/js/bundle-client.js" $ mempty

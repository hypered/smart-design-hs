{-# LANGUAGE OverloadedStrings #-}

module Smart.Html.Errors
  ( NotFound(..)
  , Error(..)
  ) where

import           Smart.Html.Shared.Html.Icons
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


--------------------------------------------------------------------------------
data NotFound = NotFound

instance H.ToMarkup NotFound where
  -- TODO Move the `main` element elsewhere.
  toMarkup NotFound =
    H.toMarkup (Error 404 "Page not found"
      "Sorry, we couldn't find the page you are looking for.")


--------------------------------------------------------------------------------
data Error = Error Int Text Text -- code, title, subtitle

instance H.ToMarkup Error where
  -- TODO Move the `main` element elsewhere.
  toMarkup (Error code title subtitle) =
      H.div ! A.class_ "c-error-page" $ do
        H.span ! A.class_ "c-error-page__code" $ H.toHtml $ (show code :: Text)
        H.h4 ! A.class_ "c-error-page__title" $ H.toHtml title
        H.p ! A.class_ "c-error-page__desc" $ H.toHtml subtitle
        H.a ! A.class_ "c-button c-button--secondary"
            ! A.href "/" $
          H.span ! A.class_ "c-button__content" $ do
            H.div ! A.class_ "o-svg-icon o-svg-icon-arrow-left  " $
              H.toMarkup svgIconArrowLeft
            H.span ! A.class_ "c-button__label" $ "Go back home"

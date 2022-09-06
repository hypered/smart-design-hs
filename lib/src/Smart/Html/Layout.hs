{-|
Module: Smart.Html.Layout
Description: Functions to organize the main page components.

-}
module Smart.Html.Layout
  ( mainContent
  , mainContentSideMenu
  , withSideMenu
  , withSideMenuFullScroll
  ) where

import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
mainContent top content =
  H.main ! A.class_ "u-scroll-wrapper u-maximize-width" $ do
    top
    H.div ! A.class_ "u-scroll-wrapper-body" $ content

mainContentSideMenu menu top content =
  withSideMenu menu $ do
    top
    H.div ! A.class_ "u-scroll-wrapper-body" $ content

withSideMenu menu content =
  H.main ! A.class_ "u-maximize-width u-scroll-wrapper" $ do
    withSideMenu' menu content

withSideMenuFullScroll menu content =
  H.main ! A.class_ "u-maximize-width" $ do
    withSideMenu' menu content

withSideMenu' menu content =
  H.div ! A.class_ "c-app-layout-inner" $ do
    H.div ! A.class_ "c-app-layout-inner__sidebar" $ H.toMarkup menu
    H.div
      ! A.class_ "c-app-layout-inner__content"
      $ H.div
      ! A.class_ "u-scroll-wrapper"
      $ content

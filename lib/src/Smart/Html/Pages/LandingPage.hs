{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Smart.Html.Pages.LandingPage
  ( landingPage
  , navigation
  ) where

import           Smart.Html.Alert
import           Smart.Html.Button
import           Smart.Html.Dsl                 ( HtmlCanvas )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Navbar
import           Smart.Html.Shared.Html.Icons
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


landingPage :: HtmlCanvas
landingPage = Dsl.SingletonCanvas $ do
  H.toMarkup navigation
  landing
  Misc.landingFooter "https://github.com/hypered/smart-design-hs"

navigation :: NavbarWebsite
navigation = NavbarWebsite
  [Entry "Documentation" (Link "/documentation"), Entry "Login" (Link "/login"), Entry "Sign up" (Link "/signup")]

landing = H.main ! A.class_ "o-container o-container--flex" $ do
  Misc.landingHero "Curiosity, a working prototype to redefine our software"
    $ do
        H.p $ do
          "This site is a running instance of Curiosity, an always \
          \work-in-progress system to think, discuss, and communicate \
          \the future of our developments."
        H.toMarkup $ Alert
          AlertWarning
          iconWarning
          "This site is up for demonstration purpose only. Data are \
          \frequently permanently erased. Please use this site only \
          \if you understand what this means."
          NoButton
  H.div ! A.class_ "o-grid" $ do
    Misc.landingPanel "Open source" $ H.p $ do
      "This project is open source "
      "and available "
      H.a ! A.href "https://github.com/hypered/smart-design-hs" $ "on GitHub"
      "."
    Misc.landingPanel "Experimental" $ H.p $ do
      "Curiosity is being developed to inform further developments, and "
      "is not intended to be a production system."

-- TODO Move this little helper definitions to Icons.
iconWarning = Just $ OSvgIconDiv @"warning" svgIconWarning

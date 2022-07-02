module Examples.Pages.MainHeader
  ( mainHeader
  , mainHeaderAlt
  , mainHeaderWebsite
  ) where

import           Examples.Navbar                ( exampleNavbar
                                                , exampleNavbarAlt
                                                , exampleNavbarWebsite
                                                )
import           Smart.Html.Dsl
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


-- | The main navigation bar, as used in an application page.
mainHeader :: HtmlCanvas
mainHeader = SingletonCanvas . div' $ H.toMarkup exampleNavbar
  where div' = H.div ! A.class_ "c-app-layout"


-- | The main navigation bar, as used in an application page.
mainHeaderAlt :: HtmlCanvas
mainHeaderAlt = SingletonCanvas . div' $ H.toMarkup exampleNavbarAlt
  where div' = H.div ! A.class_ "c-app-layout"


-- | The main navigation bar, as used in a website page.
mainHeaderWebsite :: HtmlCanvas
mainHeaderWebsite = SingletonCanvas $ H.toMarkup exampleNavbarWebsite

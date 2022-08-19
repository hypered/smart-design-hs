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
import           Smart.Html.Navbar              ( Action(..)
                                                , Entry(..)
                                                , SubEntry(..)
                                                , navbarWebsite'
                                                , toNavbarDesktop
                                                )
import           Smart.Html.Shared.Html.Icons
import           Text.Blaze                     ( customAttribute )
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
-- mainHeaderWebsite = SingletonCanvas $ H.toMarkup exampleNavbarWebsite
mainHeaderWebsite = SingletonCanvas $ navigation'

navigation' :: H.Html
navigation' = navbarWebsite' $ H.nav ! A.class_ "c-design-system-nav" $ do
  H.button
    ! A.class_
        "c-button c-button--borderless c-button--icon c-design-system-nav-open"
    ! A.type_ "button"
    ! A.id "c-design-system-nav-open"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-menu" $ H.toMarkup svgIconMenu
        H.div ! A.class_ "u-sr-accessible" $ "Open menu"
  H.button
    ! A.class_
        "c-button c-button--borderless c-button--icon c-design-system-nav-close"
    ! A.type_ "button"
    ! A.id "c-design-system-nav-close"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-close" $ H.toMarkup svgIconClose
        H.div ! A.class_ "u-sr-accessible" $ "Close menu"
  H.div ! A.class_ "c-design-system-nav__mobile" $ H.ul $ do
    H.li $ do
      H.span "Design"
      H.ul $ do
        H.li $ H.a ! A.href "/design/how-it-works.html" $ "Design workflow"
        H.li
          $ H.a
          ! A.href "/design/in-practice/copywriting.html"
          $ "Design system in practice"
    H.li $ do
      H.span "Development"
      H.ul $ do
        H.li
          $ H.a
          ! A.href "/development/getting-started.html"
          $ "Getting started"
        H.li $ H.a ! A.href "/development/example-pages.html" $ "Example pages"
        H.li $ H.a ! A.href "/prototypes/index.html" $ "Prototypes"
        H.li
          $ H.a
          ! A.href "/development/package-and-repo-links.html"
          $ "Package and repo links"
        H.li
          $ H.a
          ! A.href "/development/browser-support.html"
          $ "Browser support"
        H.li
          $ H.a
          ! A.href "/development/writing-css/architecture.html"
          $ "CSS architecture"
        H.li
          $ H.a
          ! A.href "/development/writing-css/component-structure.html"
          $ "CSS component structure"
    H.li
      $ H.a
      ! A.href "/development/component-documentation.html"
      $ "Components"
    H.li $ H.a ! A.href "/blog/index.html" $ "Blog"
    H.li $ H.a ! A.href "/changelog.html" $ "Changelog"
  toNavbarDesktop entries

entries =
  [ Entry "Design" $ SubEntries
    [ SubEntry "Design workflow" "/design/how-it-works.html" False
    , SubEntry "Design system in practice"
               "/design/in-practice/copywriting.html"
               False
    ]
  , Entry "Development" $ SubEntries
    [ SubEntry "Getting started" "/development/getting-started.html" False
    , SubEntry "Example pages"   "/development/example-pages.html"   False
    , SubEntry "Prototypes"      "/prototypes/index.html"            False
    , SubEntry "Package and repo links"
               "/development/package-and-repo-links.html"
               False
    , Divider
    , SubEntry "Browser support" "/development/browser-support.html" False
    , Divider
    , SubEntry "CSS architecture"
               "/development/writing-css/architecture.html"
               False
    , SubEntry "CSS component structure"
               "/development/writing-css/component-structure.html"
               False
    ]
  , Entry "Components" (Link "/development/component-documentation.html")
  , Entry "Blog"       (Link "/blog/index.html")
  , Entry "Changelog"  (Link "/changelog.html")
  ]

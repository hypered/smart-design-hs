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
import           Smart.Html.Navbar              ( navbarWebsite' )
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
  H.div
    ! A.class_ "c-design-system-nav__desktop"
    $ H.ul
    ! A.class_ "c-pill-navigation"
    $ do
        H.li
          ! A.class_
              "c-pill-navigation__item c-pill-navigation__item--has-child-menu"
          $ do
              H.button
                ! A.type_ "button"
                ! customAttribute "data-menu"           "menu1"
                ! customAttribute "data-menu-samewidth" "true"
                $ "Design"
              H.ul ! A.class_ "c-menu c-menu--large" ! A.id "menu1" $ do
                H.li
                  ! A.class_ "c-menu__item"
                  $ H.a
                  ! A.class_ "c-menu__label"
                  ! A.href "/design/how-it-works.html"
                  $ "Design workflow"
                H.li
                  ! A.class_ "c-menu__item"
                  $ H.a
                  ! A.class_ "c-menu__label"
                  ! A.href "/design/in-practice/copywriting.html"
                  $ "Design system in practice"
        H.li
          ! A.class_
              "c-pill-navigation__item c-pill-navigation__item--has-child-menu"
          $ do
              H.button
                ! A.type_ "button"
                ! customAttribute "data-menu"           "menu2"
                ! customAttribute "data-menu-samewidth" "true"
                $ "Development"
              H.ul ! A.class_ "c-menu c-menu--large" ! A.id "menu2" $ do
                H.li
                  ! A.class_ "c-menu__item"
                  $ H.a
                  ! A.class_ "c-menu__label"
                  ! A.href "/development/getting-started.html"
                  $ "Getting started"
                H.li
                  ! A.class_ "c-menu__item"
                  $ H.a
                  ! A.class_ "c-menu__label"
                  ! A.href "/development/example-pages.html"
                  $ "Example pages"
                H.li
                  ! A.class_ "c-menu__item"
                  $ H.a
                  ! A.class_ "c-menu__label"
                  ! A.href "/prototypes/index.html"
                  $ "Prototypes"
                H.li
                  ! A.class_ "c-menu__item"
                  $ H.a
                  ! A.class_ "c-menu__label"
                  ! A.href "/development/package-and-repo-links.html"
                  $ "Package and repo links"
                H.li
                  ! A.class_ "c-menu__divider"
                  ! A.role "presentational"
                  $ mempty
                H.li
                  ! A.class_ "c-menu__item"
                  $ H.a
                  ! A.class_ "c-menu__label"
                  ! A.href "/development/browser-support.html"
                  $ "Browser support"
                H.li
                  ! A.class_ "c-menu__divider"
                  ! A.role "presentational"
                  $ mempty
                H.li
                  ! A.class_ "c-menu__item"
                  $ H.a
                  ! A.class_ "c-menu__label"
                  ! A.href "/development/writing-css/architecture.html"
                  $ "CSS architecture"
                H.li
                  ! A.class_ "c-menu__item"
                  $ H.a
                  ! A.class_ "c-menu__label"
                  ! A.href "/development/writing-css/component-structure.html"
                  $ "CSS component structure"
        H.li
          ! A.class_ "c-pill-navigation__item"
          $ H.a
          ! A.href "/development/component-documentation.html"
          $ "Components"
        H.li
          ! A.class_ "c-pill-navigation__item"
          $ H.a
          ! A.href "/blog/index.html"
          $ "Blog"
        H.li
          ! A.class_ "c-pill-navigation__item"
          $ H.a
          ! A.href "/changelog.html"
          $ "Changelog"

module Smart.Html.Navbar
  ( Navbar(..)
  , NavbarWebsite(..)
  , Action(..)
  , Entry(..)
  , RightEntry(..)
  , SubEntry(..)
  , mkNavbar
  , mkNavbarWebsite
  , toNavbar
  , navbar
  , navbarWebsite
  , navbarWebsite'
  ) where

import           Smart.Html.Avatar
import           Smart.Html.Brand
import           Smart.Html.Shared.Html.Icons
import           Smart.Html.Shared.Types        ( Link
                                                , Title
                                                )
import           Text.Blaze                     ( customAttribute )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- | A navigation bar for the application.
data Navbar = Navbar [Entry] [RightEntry]

instance H.ToMarkup Navbar where
  toMarkup = mkNavbar

-- | A navigation bar for the website.
newtype NavbarWebsite = NavbarWebsite [Entry]

instance H.ToMarkup NavbarWebsite where
  toMarkup = mkNavbarWebsite

mkNavbar (Navbar entries items) = navbar entries items

mkNavbarWebsite (NavbarWebsite entries) = navbarWebsite entries

-- A textual entry has a name, then either a link or subentries.
data Entry = Entry Title Action

-- This represents either a link, subentries (associated to an entry), or a
-- search bar.
data Action = Link Link | SubEntries [SubEntry]

-- The SearchEntry and UserEntry don't have a c-pill-navigation__item around
-- them, otherwise, this could be part of the Entry data type instead of a
-- separate one.
-- UserEntry can use a button, but the rendering is not correctly aligned,
-- maybe this could be fixed in CSS.
data RightEntry = HelpEntry [SubEntry] | SearchEntry | UserEntry [SubEntry] AvatarImage

-- A subentry is just a triple (name, link, is-external-link), or horizontal
-- dividing rule, or a "signed-in as" information item.
data SubEntry = SubEntry Title Link Bool | Divider | SignedInAs Text

toNavbar tree = mapM_ toplevel (zip tree [1 ..])

toplevel :: (Entry, Int) -> Html
toplevel (Entry a (Link lnk), _) =
  H.li
    ! A.class_ "c-pill-navigation__item"
    $ H.a
    ! A.href (H.toValue lnk)
    $ H.toHtml a
toplevel (Entry a (SubEntries bs), n) =
  H.li
    ! A.class_ "c-pill-navigation__item c-pill-navigation__item--has-child-menu"
    $ do
        H.button
          ! A.type_ "button"
          ! customAttribute "data-menu" (H.toValue $ "subMenu-" ++ show n)
          ! customAttribute "data-menu-samewidth" "true"
          $ H.toHtml a
        H.ul
          ! A.class_ "c-menu c-menu--large"
          ! A.id (H.toValue $ "subMenu-" ++ show n)
          $ mapM_ sublevel bs

toplevel' :: RightEntry -> Html
toplevel' e = case e of
  HelpEntry bs ->
    H.nav
      $ H.ul
      ! A.class_ "c-pill-navigation"
      $ H.li
      ! A.class_
          "c-pill-navigation__item c-pill-navigation__item--has-child-menu"
      $ do
          H.button
            ! A.type_ "button"
            ! customAttribute "data-menu" "helpMenu"
            $ do
                H.div
                  ! A.class_ "o-svg-icon o-svg-icon-circle-help  "
                  $ H.toMarkup svgIconCircleHelp
                H.span ! A.class_ "u-sr-accessible" $ "Help"
          H.ul ! A.class_ "c-menu c-menu--large" ! A.id "helpMenu" $ mapM_
            sublevel
            bs
  SearchEntry -> H.div ! A.class_ "c-input-with-icon" $ do
    H.div ! A.class_ "o-svg-icon o-svg-icon-search  " $ H.toMarkup svgIconSearch
    H.input ! A.class_ "c-input" ! A.type_ "text" ! A.placeholder "Search ..."
  UserEntry bs avatarImage -> do
    H.a
      ! A.class_ "c-user-navigation"
      ! A.href "#"
      ! customAttribute "data-menu" "userMenu"
      $ H.toMarkup
      $ Avatar avatarImage Regular AvNoAdditionalContent
    H.ul ! A.class_ "c-menu c-menu--large" ! A.id "userMenu" $ mapM_ sublevel bs

sublevel :: SubEntry -> Html
sublevel (SubEntry b lnk externalLink) =
  H.li
    ! A.class_ "c-menu__item"
    $ H.a
    ! A.class_ "c-menu__label"
    ! A.href (H.toValue lnk)
    $ if externalLink
        then do
          H.span $ H.toHtml b
          H.div ! A.class_ "o-svg-icon o-svg-icon-external-link  " $ H.toMarkup
            svgIconExternalLink
        else H.toHtml b

sublevel Divider =
  H.li ! A.class_ "c-menu__divider" ! A.role "presentational" $ ""

sublevel (SignedInAs name) =
  H.li ! A.class_ "c-menu__info" $ H.div ! A.class_ "c-avatar-and-text" $ do
    H.div ! A.class_ "c-avatar c-avatar--img" $ H.toMarkup svgIconUser
    H.div ! A.class_ "c-avatar-and-text__text" $ do
      H.p "Signed in as"
      H.strong $ H.toHtml name

navbar tree items =
  H.header
    $ H.div
    ! A.class_ "c-navbar c-navbar--bordered-bottom c-navbar--fixed"
    $ toolbar
        [ H.toMarkup
          $ BrandXSmall "/" "https://design.smart.coop/images/logo.svg" "Smart"
        , H.nav $ H.ul ! A.class_ "c-pill-navigation" $ toNavbar tree
        ]
        (map toplevel' items)

navbarWebsite tree =
  navbarWebsite' $ H.nav $ H.ul ! A.class_ "c-pill-navigation" $ toNavbar tree

navbarWebsite' content =
  H.header
    $ H.div
    ! A.class_ "o-container"
    $ H.div
    ! A.class_ "c-navbar c-navbar--bordered-bottom c-navbar--main"
    $ toolbar
        [ H.toMarkup
            $ BrandSmall "/" "https://design.smart.coop/images/logo.svg" "Smart"
        ]
        [content]

toolbar leftItems rightItems = H.div ! A.class_ "c-toolbar" $ do
  H.div ! A.class_ "c-toolbar__left" $ mapM_ item leftItems
  H.div ! A.class_ "c-toolbar__right" $ mapM_ item rightItems
  where item = H.div ! A.class_ "c-toolbar__item"

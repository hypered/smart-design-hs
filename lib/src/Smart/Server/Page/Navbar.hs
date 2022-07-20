{-# LANGUAGE OverloadedStrings #-}
module Smart.Server.Page.Navbar
  ( IsNavbarContent(..)
  ) where

import           Text.Blaze.Html5

class IsNavbarContent a where

  -- | Indicate how we can render something as HTML.
  navbarMarkup :: a -> Html


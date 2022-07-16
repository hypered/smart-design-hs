module Examples.Navbar
  ( navbars
  , exampleNavbar
  , exampleNavbarAlt
  , exampleNavbarWebsite
  ) where

import           Smart.Html.Avatar
import           Smart.Html.Navbar

navbars :: [Navbar]
navbars = [exampleNavbar]

exampleNavbar :: Navbar
exampleNavbar = Navbar
  [ Entry "Activities" (Link "#")
  , Entry
    "Management"
    (SubEntries
      [ SubEntry "Nav item" "#" False
      , SubEntry "Nav item" "#" False
      , SubEntry "Nav item" "#" False
      ]
    )
  , Entry
    "Documents"
    (SubEntries [SubEntry "Nav item" "#" False, SubEntry "Nav item" "#" False])
  , Entry "Members" (Link "#")
  , Entry "Archive" (Link "#")
  ]
  [helpEntry, SearchEntry, userEntry]

exampleNavbarAlt :: Navbar
exampleNavbarAlt = Navbar [] [helpEntry, SearchEntry, userEntry]

exampleNavbarWebsite :: NavbarWebsite
exampleNavbarWebsite = NavbarWebsite
  [ Entry
    "Design"
    (SubEntries
      [ SubEntry "Nav item" "#" False
      , SubEntry "Nav item" "#" False
      , SubEntry "Nav item" "#" False
      ]
    )
  , Entry
    "Development"
    (SubEntries [SubEntry "Nav item" "#" False, SubEntry "Nav item" "#" False])
  , Entry "Components" (Link "#")
  , Entry "Blog"       (Link "#")
  , Entry "Changelog"  (Link "#")
  ]

helpEntry = HelpEntry helpEntries

helpEntries =
  [ SubEntry "About this page" "#" False
  , Divider
  , SubEntry "Documentation" "#" True
  , SubEntry "Report a bug"  "#" False
  ]

userEntry = UserEntry userEntries
  (AvatarImage "https://design.smart.coop/images/avatars/1.jpg")

userEntries =
  [SubEntry "My profile" "#" False, Divider, SubEntry "Sign out" "#" False]

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

helpEntry = HelpEntry helpEntries

helpEntries =
  [ SubEntry "About this page" "#" False
  , Divider
  , SubEntry "Documentation" "#" True
  , SubEntry "Report a bug"  "#" False
  ]

userEntry = UserEntry
  userEntries
  (AvatarImage "/static/images/avatars/1.jpg")

userEntries =
  [SubEntry "My profile" "#" False, Divider, SubEntry "Sign out" "#" False]

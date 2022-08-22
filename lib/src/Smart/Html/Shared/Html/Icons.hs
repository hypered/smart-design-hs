{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Smart.Html.Shared.Html.Icons
  ( Icon(..)
  , OSvgIconDiv(..)
  , IconDiv(..)
  , svgIconChevronLeft
  , svgIconChevronRight
  , svgIconTag
  , svgIconCheck
  , svgIconEdit
  , svgIconDelete
  , svgIconOptionsHorizontal
  , svgIconExternalLink
  , svgIconSearch
  , svgIconArrowLeft
  , svgIconArrowRight
  , svgIconClose
  , svgIconSave
  , svgIconAdd
  , svgIconBills
  , svgIconUser
  , svgIconWarning
  -- * Icons with circles surrounding them.
  , svgIconCircleAdd
  , svgIconCircleCheck
  , svgIconCircleError
  , svgIconCircleHelp
  , svgIconCircleInformation
  , svgIconCirclePercent
  -- * Contact related icons
  , svgIconEmail
  , svgIconPhone
  -- * File related icons 
  , svgIconFileAttachment
  , svgIconDocument
  , svgIconEyeOff
  , svgIconEye
  , svgIconGitHub
  , svgIconMenu
  ) where


import qualified Data.Text                     as T
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Text.Blaze.Svg11              as S
import qualified Text.Blaze.Svg11.Attributes   as SA

-- | An Svg icon wrapped up in a div with @o-svg-icon@ and @o-svg-icon-<specific>@ classes.
newtype OSvgIconDiv (iconType :: Symbol) = OSvgIconDiv Icon

instance KnownSymbol iconType => H.ToMarkup (OSvgIconDiv iconType) where
  toMarkup (OSvgIconDiv icon) =
    let iconSpecificClass =
          mappend "o-svg-icon-" . T.pack $ symbolVal (Proxy @iconType)
        iconDivClass = H.textValue $ "o-svg-icon " <> iconSpecificClass
    in  H.div ! A.class_ iconDivClass $ H.toMarkup icon

{- | An icon enclosed in a div that just contains an icon specific class, of the form @<specificName>__icon@.
For example, for file-attachment icons, we can use @IconDiv \@"c-file-attachment" svgIconFileAttachment@; the resulting div will contain

@
<div class="c-file-attachment__icon" ..> icon html </div> 
@ 
-}
newtype IconDiv (iconType :: Symbol) = IconDiv Icon

instance KnownSymbol iconType => H.ToMarkup (IconDiv iconType) where
  toMarkup (IconDiv icon) =
    let divClass =
          H.textValue $ (T.pack . symbolVal $ Proxy @iconType) <> "__icon"
    in  H.div ! A.class_ divClass $ H.toMarkup icon

-- | Newtype wrapper over all icons for safety.
-- FIXME: rename to svg icon.
newtype Icon = Icon H.Html
             deriving H.ToMarkup via H.Html

--------------------------------------------------------------------------------
svgIconCircleHelp :: Icon
svgIconCircleHelp = mkSvgIconStdFill
  [ "M12 4C9.87827 4 7.84344 4.84285 6.34315 6.34315C4.84285 7.84344 4 9.87827 4 12C4 14.1217 4.84285 16.1566 6.34315 17.6569C7.84344 19.1571 9.87827 20 12 20C14.1217 20 16.1566 19.1571 17.6569 17.6569C19.1571 16.1566 20 14.1217 20 12C20 9.87827 19.1571 7.84344 17.6569 6.34315C16.1566 4.84285 14.1217 4 12 4ZM2 12C2 6.477 6.477 2 12 2C17.523 2 22 6.477 22 12C22 17.523 17.523 22 12 22C6.477 22 2 17.523 2 12Z"
  , "M12 14C11.7348 14 11.4804 13.8946 11.2929 13.7071C11.1054 13.5196 11 13.2652 11 13V12C11 11.7348 11.1054 11.4804 11.2929 11.2929C11.4804 11.1054 11.7348 11 12 11C12.2652 11 12.5196 11.1054 12.7071 11.2929C12.8946 11.4804 13 11.7348 13 12V13C13 13.2652 12.8946 13.5196 12.7071 13.7071C12.5196 13.8946 12.2652 14 12 14ZM10.5 16.5C10.5 16.1022 10.658 15.7206 10.9393 15.4393C11.2206 15.158 11.6022 15 12 15C12.3978 15 12.7794 15.158 13.0607 15.4393C13.342 15.7206 13.5 16.1022 13.5 16.5C13.5 16.8978 13.342 17.2794 13.0607 17.5607C12.7794 17.842 12.3978 18 12 18C11.6022 18 11.2206 17.842 10.9393 17.5607C10.658 17.2794 10.5 16.8978 10.5 16.5Z"
  , "M12.3899 7.811C11.4329 7.766 10.6299 8.301 10.4859 9.164C10.4356 9.41907 10.2878 9.6445 10.0741 9.79249C9.86029 9.94047 9.59729 9.99938 9.34081 9.95672C9.08434 9.91406 8.85456 9.77319 8.7002 9.56397C8.54584 9.35476 8.47903 9.09364 8.51394 8.836C8.86994 6.7 10.8169 5.734 12.4849 5.814C13.3389 5.854 14.2179 6.161 14.8939 6.793C15.5869 7.44 15.9999 8.368 15.9999 9.5C15.9999 10.791 15.4919 11.749 14.6169 12.332C13.8139 12.867 12.8289 13 11.9999 13C11.7347 13 11.4804 12.8946 11.2928 12.7071C11.1053 12.5196 10.9999 12.2652 10.9999 12C10.9999 11.7348 11.1053 11.4804 11.2928 11.2929C11.4804 11.1054 11.7347 11 11.9999 11C12.6699 11 13.1859 10.883 13.5079 10.668C13.7579 10.501 13.9999 10.208 13.9999 9.5C13.9999 8.882 13.7879 8.497 13.5279 8.254C13.2509 7.995 12.8479 7.834 12.3899 7.811Z"
  ]

svgIconCircleInformation :: Icon
svgIconCircleInformation = mkSvgIconStdFill
  [ "M12 4C7.58172 4 4 7.58172 4 12C4 16.4183 7.58172 20 12 20C16.4183 20 20 16.4183 20 12C20 7.58172 16.4183 4 12 4ZM2 12C2 6.47715 6.47715 2 12 2C17.5228 2 22 6.47715 22 12C22 17.5228 17.5228 22 12 22C6.47715 22 2 17.5228 2 12Z"
  , "M12 10C12.5523 10 13 10.4477 13 11V17C13 17.5523 12.5523 18 12 18C11.4477 18 11 17.5523 11 17V11C11 10.4477 11.4477 10 12 10Z"
  , "M13.5 7.5C13.5 8.32843 12.8284 9 12 9C11.1716 9 10.5 8.32843 10.5 7.5C10.5 6.67157 11.1716 6 12 6C12.8284 6 13.5 6.67157 13.5 7.5Z"
  ]

svgIconCirclePercent :: Icon
svgIconCirclePercent = mkSvgIconStdFill
  [ "M4 12C4 7.58172 7.58172 4 12 4C16.4183 4 20 7.58172 20 12C20 16.4183 16.4183 20 12 20C7.58172 20 4 16.4183 4 12ZM12 2C6.47715 2 2 6.47715 2 12C2 17.5228 6.47715 22 12 22C17.5228 22 22 17.5228 22 12C22 6.47715 17.5228 2 12 2ZM15.7071 9.70711C16.0976 9.31658 16.0976 8.68342 15.7071 8.29289C15.3166 7.90237 14.6834 7.90237 14.2929 8.29289L8.29289 14.2929C7.90237 14.6834 7.90237 15.3166 8.29289 15.7071C8.68342 16.0976 9.31658 16.0976 9.70711 15.7071L15.7071 9.70711ZM14.5 16C15.3284 16 16 15.3284 16 14.5C16 13.6716 15.3284 13 14.5 13C13.6716 13 13 13.6716 13 14.5C13 15.3284 13.6716 16 14.5 16ZM11 9.5C11 10.3284 10.3284 11 9.5 11C8.67157 11 8 10.3284 8 9.5C8 8.67157 8.67157 8 9.5 8C10.3284 8 11 8.67157 11 9.5Z"
  ]

svgIconExternalLink :: Icon
svgIconExternalLink =
  mkSvgIconStdFill
    . pure
    $ "M14 5C13.7348 5 13.4804 4.89464 13.2929 4.70711C13.1054 4.51957 13 4.26522 13 4C13 3.73478 13.1054 3.48043 13.2929 3.29289C13.4804 3.10536 13.7348 3 14 3H20C20.2652 3 20.5196 3.10536 20.7071 3.29289C20.8946 3.48043 21 3.73478 21 4V10C21 10.2652 20.8946 10.5196 20.7071 10.7071C20.5196 10.8946 20.2652 11 20 11C19.7348 11 19.4804 10.8946 19.2929 10.7071C19.1054 10.5196 19 10.2652 19 10V6.414L9.707 15.707C9.5184 15.8892 9.2658 15.99 9.0036 15.9877C8.7414 15.9854 8.49059 15.8802 8.30518 15.6948C8.11977 15.5094 8.0146 15.2586 8.01233 14.9964C8.01005 14.7342 8.11084 14.4816 8.293 14.293L17.586 5H14ZM3 7C3 6.46957 3.21071 5.96086 3.58579 5.58579C3.96086 5.21071 4.46957 5 5 5H10C10.2652 5 10.5196 5.10536 10.7071 5.29289C10.8946 5.48043 11 5.73478 11 6C11 6.26522 10.8946 6.51957 10.7071 6.70711C10.5196 6.89464 10.2652 7 10 7H5V19H17V14C17 13.7348 17.1054 13.4804 17.2929 13.2929C17.4804 13.1054 17.7348 13 18 13C18.2652 13 18.5196 13.1054 18.7071 13.2929C18.8946 13.4804 19 13.7348 19 14V19C19 19.5304 18.7893 20.0391 18.4142 20.4142C18.0391 20.7893 17.5304 21 17 21H5C4.46957 21 3.96086 20.7893 3.58579 20.4142C3.21071 20.0391 3 19.5304 3 19V7Z"

svgIconSearch :: Icon
svgIconSearch =
  mkSvgIconStdFill
    . pure
    $ "M10 4C6.68629 4 4 6.68629 4 10C4 13.3137 6.68629 16 10 16C13.3137 16 16 13.3137 16 10C16 6.68629 13.3137 4 10 4ZM2 10C2 5.58172 5.58172 2 10 2C14.4183 2 18 5.58172 18 10C18 11.8487 17.3729 13.551 16.3199 14.9056L21.7071 20.2929C22.0976 20.6834 22.0976 21.3166 21.7071 21.7071C21.3166 22.0976 20.6834 22.0976 20.2929 21.7071L14.9056 16.3199C13.551 17.3729 11.8487 18 10 18C5.58172 18 2 14.4183 2 10Z"

svgIconArrowLeft :: Icon
svgIconArrowLeft =
  mkSvgIconStdFill
    . pure
    $ "M11.7071 5.29289C12.0976 5.68342 12.0976 6.31658 11.7071 6.70711L7.41421 11H19C19.5523 11 20 11.4477 20 12C20 12.5523 19.5523 13 19 13H7.41421L11.7071 17.2929C12.0976 17.6834 12.0976 18.3166 11.7071 18.7071C11.3166 19.0976 10.6834 19.0976 10.2929 18.7071L4.29289 12.7071C4.10536 12.5196 4 12.2652 4 12C4 11.7348 4.10536 11.4804 4.29289 11.2929L10.2929 5.29289C10.6834 4.90237 11.3166 4.90237 11.7071 5.29289Z"

svgIconArrowRight :: Icon
svgIconArrowRight =
  mkSvgIconStdFill
    . pure
    $ "M12.2929 5.29289C12.6834 4.90237 13.3166 4.90237 13.7071 5.29289L19.7071 11.2929C19.8946 11.4804 20 11.7348 20 12C20 12.2652 19.8946 12.5196 19.7071 12.7071L13.7071 18.7071C13.3166 19.0976 12.6834 19.0976 12.2929 18.7071C11.9024 18.3166 11.9024 17.6834 12.2929 17.2929L16.5858 13L5 13C4.44772 13 4 12.5523 4 12C4 11.4477 4.44772 11 5 11L16.5858 11L12.2929 6.70711C11.9024 6.31658 11.9024 5.68342 12.2929 5.29289Z"

svgIconClose :: Icon
svgIconClose =
  mkSvgIconStdFill
    . pure
    $ "M5.29289 5.2929C5.68342 4.90237 6.31658 4.90237 6.70711 5.2929L12 10.5858L17.2929 5.2929C17.6834 4.90237 18.3166 4.90237 18.7071 5.2929C19.0976 5.68342 19.0976 6.31659 18.7071 6.70711L13.4142 12L18.7071 17.2929C19.0976 17.6834 19.0976 18.3166 18.7071 18.7071C18.3166 19.0976 17.6834 19.0976 17.2929 18.7071L12 13.4142L6.70711 18.7071C6.31658 19.0976 5.68342 19.0976 5.29289 18.7071C4.90237 18.3166 4.90237 17.6834 5.29289 17.2929L10.5858 12L5.29289 6.70711C4.90237 6.31659 4.90237 5.68342 5.29289 5.2929Z"

svgIconSave :: Icon
svgIconSave =
  mkSvgIconStdFill
    . pure
    $ "M3 5C3 3.89543 3.89543 3 5 3H9H15H16.5858C17.1162 3 17.6249 3.21071 18 3.58579L20.7071 6.29289C20.8946 6.48043 21 6.73478 21 7V19C21 20.1046 20.1046 21 19 21H15H9H5C3.89543 21 3 20.1046 3 19V5ZM9 19H15V13H9V19ZM17 19H19V7.41421L17 5.41421V7C17 8.10457 16.1046 9 15 9H9C7.89543 9 7 8.10457 7 7V5H5V19H7V13C7 11.8954 7.89543 11 9 11H15C16.1046 11 17 11.8954 17 13V19ZM9 5V7H15V5H9Z"

svgIconAdd :: Icon
svgIconAdd =
  mkSvgIconStdFill
    . pure
    $ "M12 4C12.5523 4 13 4.44771 13 5V11H19C19.5523 11 20 11.4477 20 12C20 12.5523 19.5523 13 19 13H13V19C13 19.5523 12.5523 20 12 20C11.4477 20 11 19.5523 11 19V13H5C4.44772 13 4 12.5523 4 12C4 11.4477 4.44772 11 5 11H11V5C11 4.44771 11.4477 4 12 4Z"

svgIconDocument :: Icon
svgIconDocument = mkSvgIconStdFill
  [ "M6 2C4.89543 2 4 2.89543 4 4V20C4 21.1046 4.89543 22 6 22H18C19.1046 22 20 21.1046 20 20V8C20 7.73478 19.8946 7.48043 19.7071 7.29289L14.7071 2.29289C14.5196 2.10536 14.2652 2 14 2H6ZM12 4V9C12 9.55228 12.4477 10 13 10H18V20H6V4L12 4ZM17.5858 8H14V4.41421L17.5858 8Z"
  , "M8 13C8 13.5523 8.44772 14 9 14H15C15.5523 14 16 13.5523 16 13C16 12.4477 15.5523 12 15 12H9C8.44772 12 8 12.4477 8 13Z"
  , "M8 17C8 16.4477 8.44772 16 9 16H15C15.5523 16 16 16.4477 16 17C16 17.5523 15.5523 18 15 18H9C8.44772 18 8 17.5523 8 17Z"
  ]
svgIconBills :: Icon
svgIconBills =
  Icon
    $ S.svg
    ! SA.width "24"
    ! SA.height "24"
    ! SA.viewbox "0 0 24 24"
    ! SA.fill "none"
    $ S.path
    ! SA.fillRule "evenodd"
    ! SA.clipRule "evenodd"
    ! SA.d
        "M2 6C2 4.89543 2.89543 4 4 4H18C19.1046 4 20 4.89543 20 6V8C21.1046 8 22 8.89543 22 10V18C22 19.1046 21.1046 20 20 20H6C4.89543 20 4 19.1046 4 18V16C2.89543 16 2 15.1046 2 14V6ZM18 16C19.1046 16 20 15.1046 20 14V18H6V16H18ZM18 6L4 6V14H18V6ZM10 10C10 9.44772 10.4477 9 11 9C11.5523 9 12 9.44772 12 10C12 10.5523 11.5523 11 11 11C10.4477 11 10 10.5523 10 10ZM11 7C9.34315 7 8 8.34315 8 10C8 11.6569 9.34315 13 11 13C12.6569 13 14 11.6569 14 10C14 8.34315 12.6569 7 11 7Z"
    ! SA.fill "#595959"

svgIconTag :: Icon
svgIconTag = mkSvgIconStdFill
  [ "M2 3C2 2.44772 2.44772 2 3 2H11C11.2652 2 11.5196 2.10536 11.7071 2.29289L21.7071 12.2929C22.0976 12.6834 22.0976 13.3166 21.7071 13.7071L13.7071 21.7071C13.3166 22.0976 12.6834 22.0976 12.2929 21.7071L2.29289 11.7071C2.10536 11.5196 2 11.2652 2 11V3ZM4 4V10.5858L13 19.5858L19.5858 13L10.5858 4H4Z"
  , "M9 7.5C9 8.32843 8.32843 9 7.5 9C6.67157 9 6 8.32843 6 7.5C6 6.67157 6.67157 6 7.5 6C8.32843 6 9 6.67157 9 7.5Z"
  ]
svgIconOptionsHorizontal :: Icon
svgIconOptionsHorizontal = mkSvgIconStdFill
  [ "M12 14C13.1046 14 14 13.1046 14 12C14 10.8954 13.1046 10 12 10C10.8954 10 10 10.8954 10 12C10 13.1046 10.8954 14 12 14Z"
  , "M6 14C7.10457 14 8 13.1046 8 12C8 10.8954 7.10457 10 6 10C4.89543 10 4 10.8954 4 12C4 13.1046 4.89543 14 6 14Z"
  , "M18 14C19.1046 14 20 13.1046 20 12C20 10.8954 19.1046 10 18 10C16.8954 10 16 10.8954 16 12C16 13.1046 16.8954 14 18 14Z"
  ]

svgIconCheck :: Icon
svgIconCheck =
  mkSvgIconStdFill
    . pure
    $ "M20.6644 5.25259C21.0772 5.61951 21.1143 6.25158 20.7474 6.66436L10.0808 18.6644C9.89099 18.8779 9.61898 19 9.33334 19C9.04771 19 8.7757 18.8779 8.58593 18.6644L3.2526 12.6644C2.88568 12.2516 2.92286 11.6195 3.33565 11.2526C3.74843 10.8857 4.3805 10.9229 4.74742 11.3356L9.33334 16.4948L19.2526 5.33564C19.6195 4.92285 20.2516 4.88567 20.6644 5.25259Z"

svgIconEdit :: Icon
svgIconEdit =
  mkSvgIconStdFill
    . pure
    $ "M16.2929 2.29289C16.6834 1.90237 17.3166 1.90237 17.7071 2.29289L21.7071 6.29289C22.0976 6.68342 22.0976 7.31658 21.7071 7.70711L8.70711 20.7071C8.51957 20.8946 8.26522 21 8 21H4C3.44772 21 3 20.5523 3 20V16C3 15.7348 3.10536 15.4804 3.29289 15.2929L13.2927 5.2931L16.2929 2.29289ZM14 7.41421L5 16.4142V19H7.58579L16.5858 10L14 7.41421ZM18 8.58579L19.5858 7L17 4.41421L15.4142 6L18 8.58579Z"

svgIconDelete :: Icon
svgIconDelete =
  mkSvgIconStdFill
    . pure
    $ "M7 4C7 2.89543 7.89543 2 9 2H15C16.1046 2 17 2.89543 17 4V6H18.9897C18.9959 5.99994 19.0021 5.99994 19.0083 6H21C21.5523 6 22 6.44772 22 7C22 7.55228 21.5523 8 21 8H19.9311L19.0638 20.1425C18.989 21.1891 18.1182 22 17.0689 22H6.93112C5.88184 22 5.01096 21.1891 4.9362 20.1425L4.06888 8H3C2.44772 8 2 7.55228 2 7C2 6.44772 2.44772 6 3 6H4.99174C4.99795 5.99994 5.00414 5.99994 5.01032 6H7V4ZM9 6H15V4H9V6ZM6.07398 8L6.93112 20H17.0689L17.926 8H6.07398ZM10 10C10.5523 10 11 10.4477 11 11V17C11 17.5523 10.5523 18 10 18C9.44772 18 9 17.5523 9 17V11C9 10.4477 9.44772 10 10 10ZM14 10C14.5523 10 15 10.4477 15 11V17C15 17.5523 14.5523 18 14 18C13.4477 18 13 17.5523 13 17V11C13 10.4477 13.4477 10 14 10Z"

svgIconChevronRight :: Icon
svgIconChevronRight = mkSvgIconStdFill
  [ "M9.29289 18.7071C8.90237 18.3166 8.90237 17.6834 9.29289 17.2929L14.5858 12L9.29289 6.70711C8.90237 6.31658 8.90237 5.68342 9.29289 5.29289C9.68342 4.90237 10.3166 4.90237 10.7071 5.29289L16.7071 11.2929C17.0976 11.6834 17.0976 12.3166 16.7071 12.7071L10.7071 18.7071C10.3166 19.0976 9.68342 19.0976 9.29289 18.7071Z"
  ]

svgIconChevronLeft :: Icon
svgIconChevronLeft = mkSvgIconStdFill
  [ "M14.7071 5.29289C15.0976 5.68342 15.0976 6.31658 14.7071 6.70711L9.41421 12L14.7071 17.2929C15.0976 17.6834 15.0976 18.3166 14.7071 18.7071C14.3166 19.0976 13.6834 19.0976 13.2929 18.7071L7.29289 12.7071C6.90237 12.3166 6.90237 11.6834 7.29289 11.2929L13.2929 5.29289C13.6834 4.90237 14.3166 4.90237 14.7071 5.29289Z"
  ]

svgIconUser :: Icon
svgIconUser = mkSvgIconStdFill
  [ "M12 4C9.79086 4 8 5.79086 8 8C8 10.2091 9.79086 12 12 12C14.2091 12 16 10.2091 16 8C16 5.79086 14.2091 4 12 4ZM6 8C6 4.68629 8.68629 2 12 2C15.3137 2 18 4.68629 18 8C18 11.3137 15.3137 14 12 14C8.68629 14 6 11.3137 6 8ZM8 18C6.34315 18 5 19.3431 5 21C5 21.5523 4.55228 22 4 22C3.44772 22 3 21.5523 3 21C3 18.2386 5.23858 16 8 16H16C18.7614 16 21 18.2386 21 21C21 21.5523 20.5523 22 20 22C19.4477 22 19 21.5523 19 21C19 19.3431 17.6569 18 16 18H8Z"
  ]

svgIconWarning :: Icon
svgIconWarning = mkSvgIconStdFill
  [ "M12.0002 14C11.4479 14 11.0002 13.5523 11.0002 13V10C11.0002 9.44772 11.4479 9 12.0002 9C12.5525 9 13.0002 9.44772 13.0002 10V13C13.0002 13.5523 12.5525 14 12.0002 14Z"
  , "M10.5002 16.5C10.5002 15.6716 11.1717 15 12.0002 15C12.8286 15 13.5002 15.6716 13.5002 16.5C13.5002 17.3284 12.8286 18 12.0002 18C11.1717 18 10.5002 17.3284 10.5002 16.5Z"
  , "M10.2303 3.2156C10.9802 1.79093 13.0202 1.79092 13.77 3.2156L22.1136 19.0685C22.8146 20.4003 21.8488 22 20.3438 22H3.65653C2.15151 22 1.18574 20.4003 1.8867 19.0685L10.2303 3.2156ZM20.3438 20L12.0002 4.14709L3.65653 20L20.3438 20Z"
  ]

svgIconCircleAdd :: Icon
svgIconCircleAdd = mkSvgIconStdFill
  [ "M12 3.99999C7.58172 3.99999 4 7.58171 4 12C4 16.4183 7.58172 20 12 20C16.4183 20 20 16.4183 20 12C20 7.58171 16.4183 3.99999 12 3.99999ZM2 12C2 6.47714 6.47715 1.99999 12 1.99999C17.5228 1.99999 22 6.47714 22 12C22 17.5228 17.5228 22 12 22C6.47715 22 2 17.5228 2 12ZM12 6.99999C12.5523 6.99999 13 7.44771 13 7.99999V11H16C16.5523 11 17 11.4477 17 12C17 12.5523 16.5523 13 16 13H13V16C13 16.5523 12.5523 17 12 17C11.4477 17 11 16.5523 11 16V13H8C7.44772 13 7 12.5523 7 12C7 11.4477 7.44772 11 8 11H11V7.99999C11 7.44771 11.4477 6.99999 12 6.99999Z"
  ]

svgIconCircleCheck :: Icon
svgIconCircleCheck = mkSvgIconStdFill
  [ "M12 4C9.87827 4 7.84344 4.84285 6.34315 6.34315C4.84285 7.84344 4 9.87827 4 12C4 14.1217 4.84285 16.1566 6.34315 17.6569C7.84344 19.1571 9.87827 20 12 20C14.1217 20 16.1566 19.1571 17.6569 17.6569C19.1571 16.1566 20 14.1217 20 12C20 9.87827 19.1571 7.84344 17.6569 6.34315C16.1566 4.84285 14.1217 4 12 4ZM2 12C2 6.477 6.477 2 12 2C17.523 2 22 6.477 22 12C22 17.523 17.523 22 12 22C6.477 22 2 17.523 2 12ZM16.664 8.753C16.862 8.92918 16.9819 9.17675 16.9975 9.44132C17.0131 9.70589 16.923 9.96582 16.747 10.164L11.414 16.164C11.3202 16.2696 11.2051 16.3541 11.0762 16.412C10.9474 16.4698 10.8077 16.4997 10.6665 16.4997C10.5253 16.4997 10.3856 16.4698 10.2568 16.412C10.1279 16.3541 10.0128 16.2696 9.919 16.164L7.253 13.164C7.08712 12.9645 7.0053 12.7083 7.02482 12.4495C7.04434 12.1907 7.16368 11.9497 7.35762 11.7773C7.55156 11.6049 7.80492 11.5147 8.06418 11.5256C8.32344 11.5366 8.56828 11.6479 8.747 11.836L10.667 13.995L15.253 8.835C15.4293 8.63716 15.677 8.51739 15.9415 8.50202C16.2061 8.48664 16.4659 8.57691 16.664 8.753Z"
  ]

svgIconCircleError :: Icon
svgIconCircleError = mkSvgIconStdFill
  [ "M12 4C9.87827 4 7.84344 4.84285 6.34315 6.34315C4.84285 7.84344 4 9.87827 4 12C4 14.1217 4.84285 16.1566 6.34315 17.6569C7.84344 19.1571 9.87827 20 12 20C14.1217 20 16.1566 19.1571 17.6569 17.6569C19.1571 16.1566 20 14.1217 20 12C20 9.87827 19.1571 7.84344 17.6569 6.34315C16.1566 4.84285 14.1217 4 12 4ZM2 12C2 6.477 6.477 2 12 2C17.523 2 22 6.477 22 12C22 17.523 17.523 22 12 22C6.477 22 2 17.523 2 12ZM7.793 7.793C7.98053 7.60553 8.23484 7.50021 8.5 7.50021C8.76516 7.50021 9.01947 7.60553 9.207 7.793L12 10.586L14.793 7.793C14.8852 7.69749 14.9956 7.62131 15.1176 7.5689C15.2396 7.51649 15.3708 7.4889 15.5036 7.48775C15.6364 7.4866 15.7681 7.5119 15.891 7.56218C16.0139 7.61246 16.1255 7.68671 16.2194 7.7806C16.3133 7.8745 16.3875 7.98615 16.4378 8.10905C16.4881 8.23194 16.5134 8.36362 16.5123 8.4964C16.5111 8.62918 16.4835 8.7604 16.4311 8.8824C16.3787 9.00441 16.3025 9.11475 16.207 9.207L13.414 12L16.207 14.793C16.3892 14.9816 16.49 15.2342 16.4877 15.4964C16.4854 15.7586 16.3802 16.0094 16.1948 16.1948C16.0094 16.3802 15.7586 16.4854 15.4964 16.4877C15.2342 16.49 14.9816 16.3892 14.793 16.207L12 13.414L9.207 16.207C9.0184 16.3892 8.7658 16.49 8.5036 16.4877C8.2414 16.4854 7.99059 16.3802 7.80518 16.1948C7.61977 16.0094 7.5146 15.7586 7.51233 15.4964C7.51005 15.2342 7.61084 14.9816 7.793 14.793L10.586 12L7.793 9.207C7.60553 9.01947 7.50021 8.76516 7.50021 8.5C7.50021 8.23484 7.60553 7.98053 7.793 7.793Z"
  ]

svgIconEmail :: Icon
svgIconEmail = mkSvgIconStdFill
  [ "M2 6C2 4.89543 2.89543 4 4 4H20C21.1046 4 22 4.89543 22 6V18C22 19.1046 21.1046 20 20 20H4C2.89543 20 2 19.1046 2 18V6ZM5.51859 6L12 11.6712L18.4814 6H5.51859ZM20 7.32877L12.6585 13.7526C12.2815 14.0825 11.7185 14.0825 11.3415 13.7526L4 7.32877V18H20V7.32877Z"
  ]

svgIconPhone :: Icon
svgIconPhone = mkSvgIconStdFill
  [ "M3.83319 4H8.32313L9.77034 7.61803L7.44546 9.16795C7.16727 9.35342 7.00016 9.66565 7.00016 10C7.0027 10.0936 7.00017 10.001 7.00017 10.001L7.00017 10.002L7.00017 10.0043L7.0002 10.0093L7.00035 10.0217C7.0005 10.0309 7.00075 10.0421 7.00117 10.0552C7.00202 10.0814 7.00355 10.1153 7.00629 10.1564C7.01176 10.2385 7.02208 10.3494 7.04147 10.4852C7.08022 10.7565 7.15544 11.1281 7.30148 11.5662C7.59472 12.4459 8.1709 13.5849 9.29306 14.7071C10.4152 15.8293 11.5542 16.4054 12.4339 16.6987C12.8721 16.8447 13.2437 16.9199 13.515 16.9587C13.6507 16.9781 13.7617 16.9884 13.8438 16.9939C13.8849 16.9966 13.9188 16.9981 13.945 16.999C13.9581 16.9994 13.9693 16.9997 13.9785 16.9998L13.9908 17L13.9959 17L13.9981 17L13.9992 17C13.9992 17 14.1108 16.9939 14.0002 17C14.3789 17 14.7252 16.786 14.8946 16.4472L15.5643 15.1078L20.0002 15.8471V20.167C17.8891 20.4723 12.1876 20.7732 7.70727 16.2929C3.22697 11.8126 3.52788 6.1111 3.83319 4ZM9.07381 10.4861L10.8797 9.28213C11.6667 8.75751 11.9785 7.75338 11.6273 6.87525L10.1801 3.25722C9.87636 2.4979 9.14094 2 8.32313 2H3.78094C2.87243 2 2.01725 2.63116 1.86811 3.6169C1.5288 5.8595 1.06695 12.481 6.29306 17.7071C11.5192 22.9332 18.1407 22.4714 20.3833 22.1321C21.369 21.9829 22.0002 21.1277 22.0002 20.2192V15.8471C22.0002 14.8694 21.2933 14.0351 20.329 13.8743L15.8931 13.135C15.027 12.9907 14.1681 13.4281 13.7754 14.2134L13.4289 14.9064C13.32 14.8796 13.1985 14.8453 13.0664 14.8013C12.4461 14.5946 11.5851 14.1707 10.7073 13.2929C9.82943 12.4151 9.40561 11.5541 9.19885 10.9338C9.14298 10.7662 9.10275 10.6154 9.07381 10.4861Z"
  ]

svgIconFileAttachment :: Icon
svgIconFileAttachment = mkSvgIconStdFill
  [ "M6 2C4.89543 2 4 2.89543 4 4V20C4 21.1046 4.89543 22 6 22H18C19.1046 22 20 21.1046 20 20V8C20 7.73478 19.8946 7.48043 19.7071 7.29289L14.7071"
    <> " 2.29289C14.5196 2.10536 14.2652 2 14 2H6ZM12 4V9C12 9.55228 12.4477 10 13 10H18V20H6V4L12 4ZM17.5858 8H14V4.41421L17.5858 8Z"
  , "M8 13C8 13.5523 8.44772 14 9 14H15C15.5523 14 16 13.5523 16 13C16 12.4477 15.5523 12 15 12H9C8.44772 12 8 12.4477 8 13Z"
  , "M8 17C8 16.4477 8.44772 16 9 16H15C15.5523 16 16 16.4477 16 17C16 17.5523 15.5523 18 15 18H9C8.44772 18 8 17.5523 8 17Z"
  ]

svgIconEyeOff :: Icon
svgIconEyeOff =
  Icon
    $ S.svg
    ! SA.width "24"
    ! SA.height "24"
    ! SA.viewbox "0 0 24 24"
    ! SA.fill "none"
    $ do
        S.g ! SA.clipPath "url(#clip0)" $ do
          S.path
            ! SA.fillRule "evenodd"
            ! SA.clipRule "evenodd"
            ! SA.d
                "M1.70001 0.3L23.7 22.3C24.1 22.7 24.1 23.3 23.7 23.7C23.5 23.9 23.2 24 23 24C22.8 24 22.5 23.9 22.3 23.7L17.8 19.2C16.1 20.4 14.1 21 12 21C4.50001 21 0.300012 12.8 0.100012 12.4C-0.0999878 12.1 -0.0999878 11.8 0.100012 11.5C1.30001 9.4 2.80001 7.6 4.60001 6L0.300012 1.7C-0.0999878 1.3 -0.0999878 0.7 0.300012 0.3C0.700012 -0.1 1.30001 -0.1 1.70001 0.3ZM12.5 13.9L10 11.4C10 11.5652 9.96971 11.7 9.94246 11.8212C9.92022 11.9202 9.90001 12.0101 9.90001 12.1C9.90001 12.6 10.1 13.1 10.5 13.5C10.9 13.8 11.4 14 11.9 14C12.1 14 12.3 14 12.5 13.9ZM2.10001 12C3.10001 13.6 6.60001 19 12 19C13.5 19 15 18.6 16.3 17.9L14 15.6C13.4 15.9 12.7 16.2 12 16.2H11.9C10.9 16.2 9.90001 15.8 9.20001 15.1C8.40001 14.4 7.90001 13.4 7.90001 12.3C7.90001 11.5 8.10001 10.7 8.50001 10L6.00001 7.4C4.40001 8.7 3.10001 10.2 2.10001 12Z"
            ! SA.fill "#7A7A7A"
          S.path
            ! SA.d
                "M12 5C11.4 5 10.7 5.1 10.1 5.2C9.60001 5.3 9.00001 5.1 8.90001 4.5C8.80001 3.9 9.10001 3.4 9.70001 3.3C10.4 3.1 11.2 3 12 3C18.8027 3 22.8905 9.74605 23.741 11.1498C23.8282 11.2936 23.8814 11.3814 23.9 11.4C24.1 11.7 24.1 12 23.9 12.3C23.2 13.5 22.5 14.7 21.6 15.7C21.4 16 21.1 16.1 20.8 16.1C20.6 16.1 20.4 16.1 20.2 15.9C19.8 15.6 19.7 14.9 20.1 14.5C20.8 13.8 21.4 12.9 21.9 12C20.9 10.4 17.4 5 12 5Z"
            ! SA.fill "#7A7A7A"
        S.defs
          $ S.clippath
          ! A.id "clip0"
          $ S.rect
          ! SA.width "24"
          ! SA.height "24"
          ! SA.fill "white"

svgIconEye :: Icon
svgIconEye =
  Icon
    $ S.svg
    ! SA.width "24"
    ! SA.height "24"
    ! SA.viewbox "0 0 24 24"
    ! SA.fill "none"
    $ do
        S.path
          ! SA.fillRule "evenodd"
          ! SA.clipRule "evenodd"
          ! SA.d
              "M12 8C9.79087 8 8.00001 9.79086 8.00001 12C8.00001 14.2091 9.79087 16 12 16C14.2091 16 16 14.2091 16 12C16 9.79086 14.2091 8 12 8ZM10 12C10 10.8954 10.8954 10 12 10C13.1046 10 14 10.8954 14 12C14 13.1046 13.1046 14 12 14C10.8954 14 10 13.1046 10 12Z"
          ! SA.fill "#7A7A7A"
        S.path
          ! SA.fillRule "evenodd"
          ! SA.clipRule "evenodd"
          ! SA.d
              "M23.8941 11.5521C23.8943 11.5525 23.8944 11.5528 23 12C23.8944 12.4472 23.8943 12.4475 23.8941 12.4479L23.8925 12.4511L23.889 12.458L23.8777 12.4802C23.8681 12.4987 23.8547 12.5247 23.8373 12.5576C23.8025 12.6233 23.752 12.7168 23.686 12.834C23.5542 13.0684 23.3602 13.3985 23.1057 13.7925C22.5979 14.5787 21.8432 15.6294 20.8545 16.6839C18.8955 18.7736 15.8995 21 12 21C8.1005 21 5.10449 18.7736 3.14547 16.6839C2.15684 15.6294 1.40207 14.5787 0.894343 13.7925C0.639857 13.3985 0.445799 13.0684 0.313979 12.834C0.248031 12.7168 0.197547 12.6233 0.162761 12.5576C0.145364 12.5247 0.131882 12.4987 0.122345 12.4802L0.110997 12.458L0.107546 12.4511L0.106377 12.4488C0.106194 12.4484 0.10558 12.4472 1.00001 12C0.10558 11.5528 0.105748 11.5525 0.105932 11.5521L0.107546 11.5489L0.110997 11.542L0.122345 11.5198C0.131882 11.5013 0.145364 11.4753 0.162761 11.4424C0.197547 11.3767 0.248031 11.2832 0.313979 11.166C0.445799 10.9316 0.639857 10.6015 0.894343 10.2075C1.40207 9.42131 2.15684 8.3706 3.14547 7.31606C5.10449 5.22644 8.1005 3 12 3C15.8995 3 18.8955 5.22644 20.8545 7.31606C21.8432 8.3706 22.5979 9.42131 23.1057 10.2075C23.3602 10.6015 23.5542 10.9316 23.686 11.166C23.752 11.2832 23.8025 11.3767 23.8373 11.4424C23.8547 11.4753 23.8681 11.5013 23.8777 11.5198L23.889 11.542L23.8925 11.5489L23.8941 11.5521ZM2.57442 12.7075C2.39493 12.4296 2.25004 12.1889 2.14075 12C2.25004 11.8111 2.39493 11.5704 2.57442 11.2925C3.03544 10.5787 3.71817 9.6294 4.60454 8.68394C6.39553 6.77356 8.89952 5 12 5C15.1005 5 17.6045 6.77356 19.3955 8.68394C20.2818 9.6294 20.9646 10.5787 21.4256 11.2925C21.6051 11.5704 21.75 11.8111 21.8593 12C21.75 12.1889 21.6051 12.4296 21.4256 12.7075C20.9646 13.4213 20.2818 14.3706 19.3955 15.3161C17.6045 17.2264 15.1005 19 12 19C8.89952 19 6.39553 17.2264 4.60454 15.3161C3.71817 14.3706 3.03544 13.4213 2.57442 12.7075Z"
          ! SA.fill "#7A7A7A"
        S.path
          ! SA.d
              "M23 12L23.8941 11.5521C24.0348 11.8336 24.0348 12.1664 23.8941 12.4479L23 12Z"
          ! SA.fill "#7A7A7A"
        S.path
          ! SA.d
              "M0.105932 11.5521L1.00001 12L0.10558 12.4472C-0.0351837 12.1657 -0.0348316 11.8336 0.105932 11.5521Z"
          ! SA.fill "#7A7A7A"

svgIconGitHub :: Icon
svgIconGitHub =
  Icon
    $ S.svg
    ! SA.width "24"
    ! SA.height "24"
    ! SA.viewbox "0 0 24 24"
    ! SA.fill "none"
    $ S.path
    ! SA.fillRule "evenodd"
    ! SA.clipRule "evenodd"
    ! SA.d
        "M12 2C6.475 2 2 6.475 2 12C2 16.425 4.8625 20.1625 8.8375 21.4875C9.3375 21.575 9.525 21.275 9.525 21.0125C9.525 20.775 9.5125 19.9875 9.5125 19.15C7 19.6125 6.35 18.5375 6.15 17.975C6.0375 17.6875 5.55 16.8 5.125 16.5625C4.775 16.375 4.275 15.9125 5.1125 15.9C5.9 15.8875 6.4625 16.625 6.65 16.925C7.55 18.4375 8.9875 18.0125 9.5625 17.75C9.65 17.1 9.9125 16.6625 10.2 16.4125C7.975 16.1625 5.65 15.3 5.65 11.475C5.65 10.3875 6.0375 9.4875 6.675 8.7875C6.575 8.5375 6.225 7.5125 6.775 6.1375C6.775 6.1375 7.6125 5.875 9.525 7.1625C10.325 6.9375 11.175 6.825 12.025 6.825C12.875 6.825 13.725 6.9375 14.525 7.1625C16.4375 5.8625 17.275 6.1375 17.275 6.1375C17.825 7.5125 17.475 8.5375 17.375 8.7875C18.0125 9.4875 18.4 10.375 18.4 11.475C18.4 15.3125 16.0625 16.1625 13.8375 16.4125C14.2 16.725 14.5125 17.325 14.5125 18.2625C14.5125 19.6 14.5 20.675 14.5 21.0125C14.5 21.275 14.6875 21.5875 15.1875 21.4875C17.1727 20.8173 18.8977 19.5415 20.1198 17.8395C21.3419 16.1376 21.9995 14.0953 22 12C22 6.475 17.525 2 12 2Z"
    ! SA.fill "#595959"

svgIconMenu :: Icon
svgIconMenu = mkSvgIconStdFill
  [ "M4 7C4 6.44772 4.44772 6 5 6H19C19.5523 6 20 6.44772 20 7C20 7.55228 19.5523 8 19 8H5C4.44772 8 4 7.55228 4 7ZM4 12C4 11.4477 4.44772 11 5 11H19C19.5523 11 20 11.4477 20 12C20 12.5523 19.5523 13 19 13H5C4.44772 13 4 12.5523 4 12ZM4 17C4 16.4477 4.44772 16 5 16H19C19.5523 16 20 16.4477 20 17C20 17.5523 19.5523 18 19 18H5C4.44772 18 4 17.5523 4 17Z"
  ]

mkSvgIcon pathDAndFills =
  Icon
    . (S.svg ! SA.width "24" ! SA.height "24" ! SA.viewbox "0 0 24 24" ! SA.fill
        "none"
      )
    .   mconcat
    $   mkPathD
    <$> pathDAndFills
  where mkPathD (d, fill) = S.path ! SA.d d ! SA.fill fill

mkSvgIconStdFill = mkSvgIcon . fmap (, "#595959")

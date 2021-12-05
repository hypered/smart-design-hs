{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Smart.Html.Shared.Html.Icons
  ( Icon(..)
  , OSvgIconDiv(..)
  , svgIconChevronLeft
  , svgIconChevronRight
  , svgIconTag
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
  -- * Icons with circles surrounding them.
  , svgIconCircleCheck
  , svgIconCircleInformation
  , svgIconCircleHelp
  -- * Contact related icons
  , svgIconEmail
  , svgIconPhone
  -- * File related icons 
  , svgIconFileAttachment
  , svgIconDocument
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
    in  (H.div ! A.class_ iconDivClass) $ H.toMarkup icon

-- | Newtype wrapper over all icons for safety.
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

svgIconCircleCheck :: Icon
svgIconCircleCheck = mkSvgIconStdFill
  [ "M12 4C9.87827 4 7.84344 4.84285 6.34315 6.34315C4.84285 7.84344 4 9.87827 4 12C4 14.1217 4.84285 16.1566 6.34315 17.6569C7.84344 19.1571 9.87827 20 12 20C14.1217 20 16.1566 19.1571 17.6569 17.6569C19.1571 16.1566 20 14.1217 20 12C20 9.87827 19.1571 7.84344 17.6569 6.34315C16.1566 4.84285 14.1217 4 12 4ZM2 12C2 6.477 6.477 2 12 2C17.523 2 22 6.477 22 12C22 17.523 17.523 22 12 22C6.477 22 2 17.523 2 12ZM16.664 8.753C16.862 8.92918 16.9819 9.17675 16.9975 9.44132C17.0131 9.70589 16.923 9.96582 16.747 10.164L11.414 16.164C11.3202 16.2696 11.2051 16.3541 11.0762 16.412C10.9474 16.4698 10.8077 16.4997 10.6665 16.4997C10.5253 16.4997 10.3856 16.4698 10.2568 16.412C10.1279 16.3541 10.0128 16.2696 9.919 16.164L7.253 13.164C7.08712 12.9645 7.0053 12.7083 7.02482 12.4495C7.04434 12.1907 7.16368 11.9497 7.35762 11.7773C7.55156 11.6049 7.80492 11.5147 8.06418 11.5256C8.32344 11.5366 8.56828 11.6479 8.747 11.836L10.667 13.995L15.253 8.835C15.4293 8.63716 15.677 8.51739 15.9415 8.50202C16.2061 8.48664 16.4659 8.57691 16.664 8.753Z"
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

module Examples.Brand
  ( brands
  ) where

import           Smart.Html.Brand

brands :: [Brand]
brands =
  [ Brand "#" "/static/images/logo.svg" "Smart"
  , BrandSmall "#" "/static/images/logo.svg" "Smart"
  , BrandXSmall "#" "/static/images/logo.svg" "Smart"
  ]

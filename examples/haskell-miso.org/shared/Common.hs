{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
module Common where

-- | Imports
import Network.URI
import Servant.API
import Data.Proxy
import Miso

-- | We can pretty much share everything
-- Model, action, view, router, links, events map, decoders are all shareable

-- | Model
data Model = Model
  { uri :: URI
  } deriving (Show, Eq)

-- | Event Actions
data Action
  = Alert
  | ChangeURI URI
  | HandleURI URI
  | NoOp
  deriving (Show, Eq)

-- | Router
type Routes = About
  :<|> Contact
  :<|> News
  :<|> Home

-- | Routes
type About = "about" :> View Action
type Contact = "contact" :> View Action
type News = "news" :> View Action
type Home = View Action

-- | Convert client route type to a server web handler type
type family ViewTransform view r where
   ViewTransform (a :<|> b) r = ViewTransform a r :<|> ViewTransform b r
   ViewTransform (a :> b) r = ViewTransform b r
   ViewTransform view r = r

-- | Handlers
handlers = about
  :<|> contact
  :<|> news
  :<|> home

-- | Views
about :: Model -> View Action
about = template

contact :: Model -> View Action
contact = template

news :: Model -> View Action
news = template

home :: Model -> View Action
home = template

nav :: View Action
nav = div_ [] [
    button_ [ onClick goHome ] [ text "home" ]
  , button_ [ onClick goAbout ] [ text "about" ]
  , button_ [ onClick goNews ] [ text "news" ]
  , button_ [ onClick goContact ] [ text "contact" ]
  ]

template :: Model -> View Action
template Model{..} =
  div_ [] [
    nav
  , p_ [] [ text (show uri) ]
  , button_ [ onClick Alert ] [ text "alert" ]
  ]

the404 :: View Action
the404 =
  div_ [] [
    text "the 404 :("
  , button_ [ ] [ text "go home" ]
  ]

-- | Links
goAbout, goHome, goContact, goNews :: Action
( goHome, goAbout, goContact, goNews ) =
    ( goto routes homeProxy
    , goto routes aboutProxy
    , goto routes contactProxy
    , goto routes newsProxy
    )

goto a b = ChangeURI (safeLink a b)
homeProxy = Proxy :: Proxy Home
aboutProxy = Proxy :: Proxy About
contactProxy = Proxy :: Proxy Contact
newsProxy = Proxy :: Proxy News
routes = Proxy :: Proxy Routes

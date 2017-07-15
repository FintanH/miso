{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           Common
import           Data.Proxy
import           GHC.TypeLits
import qualified Lucid                    as L
import           Miso
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Lucid
import           Servant.Server
import           Servant.Utils.Enter
import           Lucid.Base

main :: IO ()
main = run 3000 $ serve app (serverHandlers :<|> serveDirectory "static")
  where
    app = Proxy :: Proxy API

-- | Wrapper for setting html doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

-- | Convert client side routes into server-side web handlers
type ServerRoutes =
  ViewTransform Routes (Get '[HTML] (Wrapper (View Action)))

type API = ServerRoutes :<|> Raw

instance L.ToHtml a => L.ToHtml (Wrapper a) where
  toHtmlRaw = L.toHtml
  toHtml (Wrapper x) =
    L.doctypehtml_ $ do
        L.head_ $ do
          bulmaCSS
          L.with (L.script_ mempty) [makeAttribute "src" "static/all.js"]
        L.body_ (L.toHtml x)
          where
            bulmaCSS =
              L.with (L.link_ mempty) [
                  L.rel_ "stylesheet"
                , L.type_ "text/css"
                , L.href_ bulmaRef ]

bulmaRef :: MisoString
bulmaRef = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"

serverHandlers = aboutHandler
  :<|> contactHandler
  :<|> newsHandler
  :<|> homeHandler
     where
       aboutHandler = do
         pure . Wrapper . about $ Model {
           uri = safeLink routes aboutProxy
         }
       contactHandler = do
         pure . Wrapper . contact $ Model {
           uri = safeLink routes contactProxy
         }
       newsHandler = do
         pure . Wrapper . news $ Model {
           uri = safeLink routes newsProxy
         }
       homeHandler = do
         pure . Wrapper . home $ Model {
           uri = safeLink routes homeProxy
         }

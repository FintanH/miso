{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           Common
import           Data.Proxy
import qualified Lucid                    as L
import           Lucid.Base
import           Miso
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Lucid
import qualified System.IO                as IO

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 3002..."
  run 3002 $ serve (Proxy @ API) (serveDirectory "static" :<|> serverHandlers)

-- | Wrapper for setting html doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

-- | Convert client side routes into server-side web handlers
type ServerRoutes =
  ViewTransform Routes (Get '[HTML] (Wrapper (View Action)))

-- | API type
type API = ("static" :> Raw) :<|> ServerRoutes

instance L.ToHtml a => L.ToHtml (Wrapper a) where
  toHtmlRaw = L.toHtml
  toHtml (Wrapper x) =
    L.doctypehtml_ $ do
        L.head_ $ do
          bulmaCSS
          L.with (L.script_ mempty) [makeAttribute "src" "static/all.js"]
          L.script_ analytics
        L.body_ (L.toHtml x)
          where
            bulmaCSS =
              L.with (L.link_ mempty) [
                  L.rel_ "stylesheet"
                , L.type_ "text/css"
                , L.href_ bulmaRef ]

bulmaRef :: MisoString
bulmaRef = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"

analytics :: MisoString
analytics =
  "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){\
  \(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),\
  \m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)\
  \})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');\
  \ga('create', 'UA-102668481-1', 'auto');\
  \ga('send', 'pageview');"


serverHandlers :: Server ServerRoutes
serverHandlers = contactHandler
  :<|> newsHandler
  :<|> aboutHandler
  :<|> homeHandler
     where
       contactHandler =
         pure . Wrapper $ contact Model {
           uri = safeLink routes contactProxy
         }
       newsHandler =
         pure . Wrapper $ news Model {
           uri = safeLink routes newsProxy
         }
       aboutHandler =
         pure . Wrapper $ about Model {
           uri = safeLink routes aboutProxy
         }
       homeHandler =
         pure . Wrapper $ home Model {
           uri = safeLink routes homeProxy
         }



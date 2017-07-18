{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
module Common where

import           Data.Bool
import qualified Data.Map    as M
import           Data.Monoid
import           Data.Proxy
import           Network.URI
import           Servant.API

import           Miso
import           Miso.String

-- | We can pretty much share everything
--
-- model, action, view, router, links, events map
-- decoders are all shareable

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
type ClientRoutes = Examples
  :<|> Docs
  :<|> Community
  :<|> Home

-- | Handlers
handlers ::
  (Model -> View Action)
    :<|> ((Model -> View Action)
        :<|> ((Model -> View Action) :<|> (Model -> View Action)))
handlers = examples
  :<|> docs
  :<|> community
  :<|> home

-- | Client Routes
type Examples  = "examples" :> View Action
type Docs      = "docs" :> View Action
type Community = "community" :> View Action
type Home      = View Action

-- | Views
community :: Model -> View Action
community = template v
  where
    v = div_ [ class_ $ pack "animated fadeIn" ] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceInDown"
         , src_ misoSrc
         ] [ ]
         , h1_ [ class_ $ pack "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "community" ]
       , h2_ [ class_ $ pack "subtitle animated pulse" ] [
          text "Slack or #IRC"
         ]
       ]

docs :: Model -> View Action
docs = template v
  where
    v = div_ [ class_ $ pack "animated fadeIn" ] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceInDown"
         , src_ misoSrc
         ] [  ]
         , h1_ [ class_ $ pack "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "docs" ]
       , h2_ [ class_ $ pack "subtitle animated pulse" ] [
          text "GHCJS Hackage or README"
         ]
       ]

misoSrc :: MisoString
misoSrc = pack "https://camo.githubusercontent.com/d6641458f09e24e8fef783de8278886949085960/68747470733a2f2f656d6f6a6970656469612d75732e73332e616d617a6f6e6177732e636f6d2f7468756d62732f3234302f6170706c652f39362f737465616d696e672d626f776c5f31663335632e706e67"

examples :: Model -> View Action
examples = template v
  where
    v =
     div_ [ class_ $ pack "animated fadeIn" ] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceInDown"
         , src_ misoSrc
         ] [  ]
         , h1_ [ class_ $ pack "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "examples" ]
       , h2_ [ class_ $ pack "subtitle animated pulse" ] [
          text "TodoMVC / Mario"
         ]
       ]

home :: Model -> View Action
home = template v
  where
    v = div_ [class_ $ pack "animated fadeIn"] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceInDown"
         , src_ misoSrc
         ] [  ]
         , h1_ [ class_ $ pack "title animated pulse"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
           ] [ text "miso" ]
       , h2_ [ class_ $ pack "subtitle animated pulse" ] [
         text "A tasty "
         , a_ [ href_ $pack "https://www.haskell.org/"
              , target_ $ pack "blank"][
             strong_ [] [text . pack $ "Haskell" ]]
         , text $ pack " front-end framework"
         ]
       ]


nav :: View Action
nav =
   nav_ [ class_$pack "navbar is-bold" ] [
     div_ [class_$pack"navbar-menu"][
       div_[class_$pack"navbar-start"][
         a_[class_$pack"navbar-item"
           , onClick (ChangeURI goHome)
           ][ text$pack"Home" ],
         a_[class_$pack"navbar-item"
           , onClick (ChangeURI goExamples)
           ][ text$pack"Examples" ],
         a_[class_$pack"navbar-item"
           , onClick (ChangeURI goDocs)
           ][ text$pack"Docs" ],
         a_[ class_$pack"navbar-item"
           , onClick (ChangeURI goCommunity)
           ][ text$pack"Community" ]
         ]
       ],
       div_ [class_$pack"navbar-end"][
         div_ [class_$pack"navbar-item"][
          div_[class_$pack"field is-grouped"][
            p_ [class_$pack"control is-info"][
             a_[  id_$pack"twitter"
                , class_$pack"button"
                , prop (pack "data-social-network") (pack "Twitter")
                , prop (pack "data-social-action") (pack "tweet")
                , prop (pack "data-social-target") (pack "https://haskell-miso.org")
                , target_$pack"blank"
                , href_$pack "https://twitter.com/intent/tweet?text=Miso: a tasty Haskell front-end framework&url=https://haskell-miso.org&via=dmjio"
                ] [
                 span_ [ class_$pack"icon"] [
                   i_ [class_$pack"fa fa-twitter"] []
                 ], span_ [] [text$pack"Tweet"]
             ],
           p_ [class_$pack"control"][
             a_ [class_$pack"button is-primary"
                ,href_$pack"https://github.com/dmjio/miso"
                ][
                 span_ [ class_$pack"icon"] [
                   i_ [class_$pack"fa fa-github"] []
                 ], span_ [] [text$pack"Github"]
             ]
            ]
          ]
         ]
        ]
       ]
     ]

template :: View Action -> Model -> View Action
template content Model{..} =
  div_ [ ] [
    hero content uri
  , middle
  , footer
  ]

middle =
  section_ [class_ $ pack  "hero" ] [
    div_ [class_ $ pack  "hero-body"] [
      div_ [class_ $ pack  "container"] [
        nav_ [class_ $ pack  "columns"] [
               a_ [ class_ $ pack  "column has-text-centered"
                   , href_ $ pack "https://medium.com/@localvoid/how-to-win-in-web-framework-benchmarks-8bc31af76ce7"
                   , target_ $ pack "_blank"
                   ] [
                  span_ [class_ $ pack  "icon is-large"] [
                      i_ [class_ $ pack  "fa fa-flash"] [ ]
                      ],
                  p_ [class_ $ pack  "title is-4"] [
                     strong_ [] [ text $ pack "Fast"]
                  ],
                  p_ [class_ $ pack  "subtitle"] [
                        text $ pack "Virtual DOM diffing algorithm"
                      ]
                  ]

              , a_ [ class_ $ pack  "column has-text-centered"
                   , href_ $ pack "/uhoh"
                   ] [
                  span_ [class_ $ pack  "icon is-large"] [
                      i_ [class_ $ pack  "fa fa-refresh"] [ ]
                      ],
                  p_ [class_ $ pack  "title is-4"] [
                     strong_ [] [ text $ pack "Isomorphic"]
                  ],
                  p_ [class_ $ pack  "subtitle"]
                      [ text $ pack "Seamless web experience, try to 404 (click here)" ]
                  ],
                  a_ [ class_ $ pack  "column has-text-centered"
                     , target_ $ pack "_blank"
                     , href_ $ pack "http://chimera.labs.oreilly.com/books/1230000000929/index.html"
                     ] [
                    span_ [class_ $ pack "icon is-large"] [
                       i_ [class_ $ pack "fa fa-gears"] [ ]
                    ], p_ [class_ $ pack "title is-4"] [
                        strong_ [] [ text $ pack "Concurrent" ]
                       ],
                      p_ [class_ $ pack  "subtitle"] [
                        text $ pack "Type-safe and polymorphic, GHC Haskell"
                       ]
                    ],
                  a_ [class_ $ pack  "column has-text-centered"
                     , href_ $  pack "https://github.com/ghcjs/ghcjs/blob/master/doc/foreign-function-interface.md"
                     , target_ $ pack "_blank"
                     ] [
                    span_ [class_ $ pack  "icon is-large"] [
                       i_ [class_ $ pack  "fa fa-code-fork"] [ ]
                    ], p_ [class_ $ pack  "title is-4"] [
                        strong_ [] [ text $ pack "Interoperable" ]
                       ],
                      p_ [class_ $ pack  "subtitle"] [
                        text $ pack "via the GHCJS FFI"
                        ]
                    ]
              ]
          ]
        ]
      ]


cols :: View action
cols = section_[][div_ [ class_ $ pack "container" ] [
  div_ [class_ $ pack "columns" ] [
   div_ [ class_ $ pack "column" ] [
     h1_ [class_ $ pack "title" ] [
       span_ [class_$pack"icon is-large"] [i_[class_$pack"fa fa-flash"][]]
     , text $ pack "Fast"
     ]
   , h2_ [class_ $ pack "subtitle" ] [
       text $ pack "Mutable virtual dom implementation"
      ]
   ]
   , div_ [ class_ $ pack "column" ] [
     text $ pack "Second column"
   ]
   , div_ [ class_ $ pack "column" ] [
      text $ pack "Third column"
   ]
   , div_ [ class_ $ pack "column" ] [
      text $ pack "Fourth column"
    ]
  ]]]

the404 :: Model -> View Action
the404 = template v
  where
    v = div_ [] [ img_ [
           width_ $ pack "100"
         , class_ $ pack "animated bounceOutUp"
         , src_ misoSrc
         ] []
         , h1_ [ class_ $ pack "title"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
         ] [ text "404" ]
       , h2_ [ class_ $ pack "subtitle animated pulse" ] [
          text "No soup for you! "
          , a_ [ onClick $ ChangeURI goHome ] [ text " - Go Home" ]
         ]
       ]

-- | Links
goHome, goExamples, goDocs, goCommunity :: URI
( goHome, goExamples, goDocs, goCommunity ) =
    ( safeLink routes homeProxy
    , safeLink routes examplesProxy
    , safeLink routes docsProxy
    , safeLink routes communityProxy
    )

homeProxy :: Proxy Home
homeProxy = Proxy
examplesProxy :: Proxy Examples
examplesProxy = Proxy
docsProxy :: Proxy Docs
docsProxy = Proxy
communityProxy :: Proxy Community
communityProxy = Proxy
routes :: Proxy ClientRoutes
routes = Proxy

-- | Github stars
starMiso :: View action
starMiso = a_ [
    class_ (pack "github-button")
  , href_ (pack "https://github.com/dmjio/miso")
  , prop (pack "data-icon") "octicon-star"
  , prop (pack "data-size") "large"
  , prop (pack "data-show-count") "true"
  , prop (pack "aria-label") "Star dmjio/miso on GitHub"
  ] [ text "Star" ]

forkMiso :: View action
forkMiso = a_ [
    class_ (pack "github-button")
  , href_ (pack "https://github.com/dmjio/miso/fork")
  , prop (pack "data-icon") "octicon-repo-forked"
  , prop (pack "data-size") "large"
  , prop (pack "data-show-count") "true"
  , prop (pack "aria-label") "Fork dmjio/miso on GitHub"
  ] [ text "Fork" ]

-- | Hero
hero :: View Action -> URI -> View Action
hero content uri' =
  section_ [ class_ $ pack "hero is-medium is-primary is-bold has-text-centered" ] [
    div_ [ class_ $pack"hero-head" ] [
     header_ [class_$pack"nav"] [
      div_ [class_$pack"container"] [
        div_ [class_$pack"nav-left"][
          a_ [class_$pack"nav-item"][
                 ]
          ],
        span_ [class_$pack"nav-toggle"] [
          span_[][]
        , span_[][]
        , span_[][]
        ],
         div_ [ class_$pack"nav-right nav-menu"] [
          a_ [ class_$ pack "nav-item " <> do pack $ bool mempty "is-active" (uri' == goHome)
             , onClick (ChangeURI goHome) ] [ text$pack"Home" ],
          a_ [class_$ pack "nav-item " <> do pack $ bool mempty "is-active" (uri' == goExamples)
             , onClick (ChangeURI goExamples)
             ] [ text$pack"Examples" ],
          a_ [class_$ pack "nav-item " <> do pack $ bool mempty "is-active" (uri' == goDocs)
             , onClick (ChangeURI goDocs)
             ] [ text$pack"Docs" ],
          a_ [class_$ pack "nav-item " <> do pack $ bool mempty "is-active" (uri' == goCommunity)
             , onClick (ChangeURI goCommunity)
             ] [ text$pack"Community" ]

          ]]]]
    , div_ [ class_ $ pack "hero-body" ] [
     div_ [ class_ $ pack "container" ] [
           content
         ]
     ]
  ]


-- | Footer
footer :: View action
footer =
  footer_ [ class_ $ pack "footer" ] [
    div_ [ class_ $ pack "container" ] [
      div_ [ class_ $ pack "content has-text-centered" ] [
         p_ [] [
            strong_ [] [ text "Miso" ]
         ,  text " by "
         ,  a_ [ href_ $ pack "https://github.com/dmjio/miso" ]
              [ text "dmjio" ]
         , text ". BSD3"
         , a_ [ href_ $ pack "https://opensource.org/licenses/BSD-3-Clause" ]
              [ text " licensed." ]
         ]
         , p_ [] [ text "The source code for this website is located "
                 , a_ [ href_ $ pack "https://github.com/dmjio/miso/tree/master/examples/haskell-miso.org" ] [  text$pack" here."]
                 ]
         , p_ [] [
           a_ [ class_ $ pack "icon"
              , href_ $ pack "https://github.com/dmjio/miso"
              , target_ (pack "blank")
              ] [span_ [class_$pack"icon is-large"]
                  [i_[class_$pack"fa fa-github"][]]]
         ]
      ]
    ]
  ]

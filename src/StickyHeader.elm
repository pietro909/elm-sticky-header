module StickyHeader exposing
    ( HeaderComponent
    , Model
    , initialModel
    , Msg
    , view
    , update
    , subscriptions
    )

{-| This module provides a header components which accepts a brand and a list of links. It will react to window's scroll.

# Definition
@docs HeaderComponent, Model

# Helpers
@docs initialModel, Msg, view, update, subscriptions

-}

import Html
import Html exposing (div, header, text, h1, nav, a)
import Html.Attributes exposing (href)
import Animation exposing (px)
import Animation
import Scroll exposing (Move)
import Time exposing (millisecond)
import Ports exposing (..)

{-| Represent a single header's component: must have a title, which is shown on the UI, an optional link,
    and a list of css classes to be applied on it.

    -- A person, but maybe we do not know their age.
    link = StickyHeader.HeaderComponent "Home" (Just "/") [ "home-selector" ]
-}
type alias HeaderComponent =
    { title : String
    , link : Maybe String
    , cssClasses : List String
    }

{-| Represent the header's model: attach it to your model

    -- inserting header's model in your application model
    type alias Model =
        { headerModel: StickyHeader.Model }
-}
type alias Model =
    { style : Animation.State
    , current : Float
    , nextGoal : Float
    , brand : Maybe HeaderComponent
    , links : List HeaderComponent
    }

{-| Helper function to initialize the header's model. It accepts an optional brand and a list of links.

    -- initializing your model
    initialModel =
        let
            headerBrand = StickyHeader.HeaderComponent "Header" (Just "#home") []
        in
            { headerModel = StickyHeader.initialModel (Just headerBrand) [] }
-}
initialModel : Maybe HeaderComponent -> List HeaderComponent -> Model
initialModel brand links =
    { style = Animation.style [ Animation.top (px 0) ]
    , current = 0.0
    , nextGoal = 0.0
    , brand = brand
    , links = links
    }

{-| The messages being used for scroll events and header's movement. Are to be put in union with your message type.

    -- extend your own messages
    type Msg
        = StickyHeaderMsg StickyHeader.Msg
        | -- your messages

-}
type Msg
    = Header Move
    | Animate Animation.Msg 

init =
    ( initialModel, Cmd.none )

easing =
    Animation.easing
        { duration = 250 * millisecond
        , ease = (\x -> x^2)
        }

-- todo: on grow, should disappear
animateScroll : Model -> (Model, Cmd a)
animateScroll model =
    let
        start = model.current
        end = model.nextGoal
        style = 
            Animation.queue [ Animation.toWith easing [ Animation.top (px end ) ] ]
                <| Animation.style [ Animation.top (px start) ]
        newModel = { model | style = style }
    in
        (newModel, Cmd.none)

onGrow model =
    Scroll.onUp animateScroll

onShrink model =
    Scroll.onDown animateScroll


{-| Update function to handle the header's messages. It needs to be placed inside your application's update function.

    -- handling header's messages in your application with update function
    update msg model =
        case msg of
            StickyHeaderMsg subMsg->
                let
                    ( updatedHeaderModel, headerCmd ) = StickyHeader.update subMsg model.headerModel
                in
                    ( { model | headerModel = updatedHeaderModel }, Cmd.map StickyHeaderMsg headerCmd )
-}
update : Msg -> Model -> (Model, Cmd a)
update action model =
    case action of
        Animate animMsg ->
            let
                newModel = 
                    { model
                    | style = Animation.update animMsg model.style
                    , current = model.nextGoal 
                    }
            in
                (newModel, Cmd.none)
        Header move ->
            let
                (previous, current) = Debug.log "move" move
                newModel = { model | nextGoal = current } 
            in
                Scroll.handle [ onGrow model, onShrink model ] move newModel

makeLink : HeaderComponent -> Html.Html a
makeLink { link, title } =
    Maybe.map
        (\url -> a [ href url ] [ text title ])
        link
    |> Maybe.withDefault (a [] [ text title ])

{-| Provides the Html, given an updated model.
    
    -- insert it in your view function
    view model =
        App.map StickyHeaderMsg (StickyHeader.view model.headerModel)

-}
view : Model -> Html.Html a
view model =
    let
        styles = Animation.render model.style
        brand = 
            Maybe.map (\b -> h1 [] [ (makeLink b) ]) model.brand
            |> Maybe.withDefault (Html.text "")
        navs = 
            List.map makeLink model.links
    in
        header styles 
            [ brand
            , nav [] navs
            ]

{-| Provide the subscription to the JS port which brings the scroll values.

    -- insert the subscription in you subscription loop
    subscriptions model =
    List.map (Platform.Sub.map StickyHeaderMsg) (StickyHeader.subscriptions model.headerModel)
    |> Sub.batch

-}
subscriptions : Model -> List (Sub Msg)
subscriptions model =
    [ scroll Header
    , Animation.subscription Animate [ model.style ]
    ]

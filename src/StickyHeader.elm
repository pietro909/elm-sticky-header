module StickyHeader exposing (..)

import Html
import Html exposing (div, header, text, h1, nav, a)
import Html.Attributes exposing (href)
import Animation exposing (px)
import Animation
import Scroll exposing (Move)
import Time exposing (millisecond)

import Ports exposing (..)

type alias HeaderComponent =
    { title : String
    , link : Maybe String
    , cssClasses : List String
    }

type alias Model =
    { style : Animation.State
    , current : Float
    , nextGoal : Float
    , brand : Maybe HeaderComponent
    , links : List HeaderComponent
    }

initialModel : Maybe HeaderComponent -> List HeaderComponent -> Model
initialModel brand links =
    { style = Animation.style [ Animation.top (px 0) ]
    , current = 0.0
    , nextGoal = 0.0
    , brand = brand
    , links = links
    }


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

subscriptions : Model -> List (Sub Msg)
subscriptions model =
    [ scroll Header
    , Animation.subscription Animate [ model.style ]
    ]

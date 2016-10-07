module StickyHeader exposing (..)

import Html
import Html exposing (div, header, text, h1)
import Animation exposing (px)
import Animation
import Scroll exposing (Move)
import Time exposing (millisecond)

import Ports exposing (..)

type alias HeaderComponent =
    { title : String
    , link : Maybe String
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


view : Model -> Html.Html a
view model =
    let
        styles = Animation.render model.style 
    in
        header styles [ h1 [] [ text "Header" ] ]

subscriptions : Model -> List (Sub Msg)
subscriptions model =
    [ scroll Header
    , Animation.subscription Animate [ model.style ]
    ]
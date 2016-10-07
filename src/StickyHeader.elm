import Html
import Html.App as App
import Scroll exposing (Move)
import Html exposing (div, header, text, h1)
import Html.Attributes exposing (style)
import Animation exposing (px)
import Time exposing (millisecond)

import Ports exposing (..)

main =
    App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

type alias Model =
    { style : Animation.State
    , current : Float
    , nextGoal : Float
    }

initialModel =
    { style = Animation.style [ Animation.top (px 0) ]
    , current = 0.0
    , nextGoal = 0.0
    }


type Action
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
        start = Debug.log "start" model.current 
        end = Debug.log "end" model.nextGoal 
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


update : Action -> Model -> (Model, Cmd a)
update action model =
    case action of
        Animate animMsg ->
            let
                newModel = 
                    -- if model.current == model.nextGoal then
                    --     model
                    -- else
                        { model
                            | style = Animation.update animMsg model.style --( Debug.log "animate" model.style )
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
      div []
        [ header styles [ h1 [] [ text "Header" ] ]
        , div [] [] 
        ]


{-- SUBSCRIPTIONS
 -  need to collect all the inbound ports in one subscription flow
--}
subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ scroll Header
        , Animation.subscription Animate [ model.style ]
        ]


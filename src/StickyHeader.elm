import Html
import Html.App as App
import Scroll exposing (Move)
import Html exposing (div, header, text, h1)
import Html.Attributes exposing (style)
import Animation exposing (px)
import Time exposing (second)

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


onGrow : Model -> Move -> Maybe (Scroll.Update a b)
onGrow model =
    Scroll.onCrossDown 200 (\m -> (m, Cmd.none))

onShrink : Model -> Move -> Maybe (Scroll.Update a b)
onShrink model =
    Scroll.onCrossUp 200 (\m -> (m, Cmd.none))

-- let
--                 style = 
--                     Animation.queue
--                         [ Animation.toWith
--                             (Animation.easing 
--                                 { duration = 2*second
--                                 , ease = (\x -> x^2)
--                                 }
--                             ) 
--                             [ Animation.height (px 200) ]
--                         ]
--                     <|
--                         Animation.style
--                             [ Animation.height (px 90) ]
--                 newModel = { model | style = style }
--             in
--                 (newModel, Cmd.none)


update action model =
    case action of
        Animate animMsg ->
            let
                newModel = 
                    if model.current == model.nextGoal then
                        model
                    else
                        -- animate!
                        { model
                            | style = Animation.update animMsg ( Debug.log "animate" model.style )
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
      styles = Animation.render model.style ++ [ style [("position", "fixed")]] 
    in
      div []
        [ header
          styles 
          [ h1 [] [ text "Header" ] ]
        , div [ style [("height", "10000px")] ] [] 
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


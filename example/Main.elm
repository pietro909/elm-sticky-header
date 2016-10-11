
import Html
import Html.App as App
import Html exposing (div, header, text, h1, h2, article)
import Html.Attributes exposing (style, class)
import Platform.Sub
import StickyHeader

main =
    App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

type alias Model =
    { headerModel: StickyHeader.Model }

headerLinks =
    List.map 
        (\(title, url) -> StickyHeader.buildActiveHeaderItem title url [])
        [ ("Prelude", "#prelude_to_foundation") 
        , ("Forward", "#forward_the_foundation") 
        , ("Foundation", "#foundation")
        , ("Foundation and Empire", "#foundation_and_empire")
        ]

initialModel =
    let
        headerBrand = StickyHeader.buildActiveHeaderItem "StickyHeader demo" "https://github.com/pietro909/elm-sticky-header" [ "brand" ]
    in
        { headerModel = StickyHeader.initialModel (Just headerBrand) headerLinks }


type Msg
    = StickyHeaderMsg StickyHeader.Msg

init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StickyHeaderMsg subMsg->
            let
                ( updatedHeaderModel, headerCmd ) = StickyHeader.update subMsg model.headerModel
            in
                ( { model | headerModel = updatedHeaderModel }, Cmd.map StickyHeaderMsg headerCmd )


view : Model -> Html.Html Msg
view model =
    article []
        [ App.map StickyHeaderMsg (StickyHeader.view model.headerModel) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    List.map (Platform.Sub.map StickyHeaderMsg) (StickyHeader.subscriptions model.headerModel)
    |> Sub.batch
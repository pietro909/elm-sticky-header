
import Html
import Html.App as App
import Html exposing (div, header, text, h1, h2, article)
import Html.Attributes exposing (style)
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
    [ StickyHeader.HeaderComponent "Prelude" (Just "#prelude_to_foundation") []
    , StickyHeader.HeaderComponent "Forward" (Just "#forward_the_foundation") []
    , StickyHeader.HeaderComponent "Foundation" (Just "#foundation") []
    , StickyHeader.HeaderComponent "Foundation and Empire" (Just "#foundation_and_empire") []
    ]

initialModel =
    let
        headerBrand = StickyHeader.HeaderComponent "Header" (Just "#home") []
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
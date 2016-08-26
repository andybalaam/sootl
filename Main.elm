import Html exposing (Html)
import Html.App exposing (programWithFlags)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Window


type alias Flags =
    { width : Int
    , height : Int
    }


type Msg = Resize Int Int


type alias Model =
    { screen :
        { width : Int
        , height : Int
        }
    }


init : Flags -> (Model, Cmd Msg)
init flags =
    (
        { screen =
            { width = flags.width
            , height = flags.height
            }
        }
    , Cmd.none
    )



view : Model -> Html Msg
view model =
    let
        sw = model.screen.width  - 0
        sh = model.screen.height - 0
    in
        svg
        [ width  <| toString sw
        , height <| toString sh
        ]
        [ rect
            [ x "0"
            , y "0"
            , width (toString model.screen.width)
            , height (toString model.screen.height)
            , fill "#eeffee"
            ]
            []
        , text'
            [ x <| toString <| sw / 2
            , y <| toString <| sh / 2
            , fontSize <| toString <| sh / 10
            , textAnchor "middle"
            ]
            [ text
                ((toString model.screen.width)
                ++ ", "
                ++ (toString model.screen.height))
            ]
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let m =
        case msg of
            Resize w h -> {model | screen = {width = w, height = h}}
    in
        (m, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\size -> Resize size.width size.height)


main =
   programWithFlags
     { init = init
     , view = view
     , update = update
     , subscriptions = subscriptions
     }




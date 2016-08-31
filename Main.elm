import AnimationFrame exposing (times)
import Html exposing (Html)
import Html.App exposing (programWithFlags)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing(Time)
import Window


type alias Flags =
    { width : Int
    , height : Int
    }


type Msg = Resize Int Int | NewFrame Time


type HitShape = Circle Float Float Float


hitCircle : Float -> Float -> Float -> HitShape
hitCircle x y r = Circle x y r


type alias Light =
    Float ->
        { hitboxes : List HitShape
        , svgs     : List (Svg Msg)
        }


type alias Level =
    { lights : List Light
    }


emptyLevel : Level
emptyLevel =
    { lights =
        [
            \t ->
                { hitboxes = []
                , svgs = [text' [ x "20", y "20" ] [ text "Missing Level!" ] ]
                }
        ]
    }


type alias Model =
    { screen :
        { width : Int
        , height : Int
        }
    , startTime : Time
    , time : Time
    , levels : List Level
    }


level0 : Level
level0 =
    { lights =
        [
            \t ->
                { hitboxes = []
                , svgs =
                    [ circle
                        [ cx <| toString <| 20 + 10 * t
                        , cy "20"
                        , r <| toString <| 10 + 5 * t
                        ]
                        []
                    ]
                }
        ]
    }


init : Flags -> (Model, Cmd Msg)
init flags =
    (
        { screen =
            { width = flags.width
            , height = flags.height
            }
        , startTime = -1
        , time = 0
        , levels =
            [
                level0
            ]
        }
    , Cmd.none
    )



view : Model -> Html Msg
view model =
    let
        time = (model.time - model.startTime) / 1000
        sw = model.screen.width  - 0
        sh = model.screen.height - 0
        min = if sw < sh then sw else sh
        tx = (sw - min) / 2
        ty = (sh - min) / 2
        trans = "translate(" ++ (toString tx) ++ "," ++ (toString ty) ++ "),scale(" ++ (toString (min/200)) ++ "),translate(100, 100)"
    in
        svg
        [ width  <| toString sw
        , height <| toString sh
        ]
        [g [transform trans] ((viewExtra model time sw sh) ++ (viewLights model time))]


viewExtra : Model -> Time -> Float -> Float -> List (Svg Msg)
viewExtra model time sw sh =
    [ rect
        [ x "-100"
        , y "-100"
        , width "200"
        , height "200"
        , fill "#eeffee"
        ]
        []
    , text'
        [ x "0"
        , y "0"
        , fontSize "10"
        , textAnchor "middle"
        ]
        [ text
            ((toString model.screen.width)
            ++ ", "
            ++ (toString model.screen.height)
            ++ " at "
            ++ (toString (round time)))
        ]
    ]


viewLights : Model -> Time -> List (Svg Msg)
viewLights model time =
    ( List.concat <| List.map (\lig -> (lig time).svgs) ( firstLevel model.levels ).lights )


firstLevel : List Level -> Level
firstLevel levels =
    case (List.head levels) of
        Just lev  -> lev
        Nothing -> emptyLevel   -- Error - levels should not be empty


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let m =
        case msg of
            Resize w h -> updateResize w h model
            NewFrame t -> updateNewFrame t model
    in
        (m, Cmd.none)


updateResize : Int -> Int -> Model -> Model
updateResize w h model =
    {model | screen = {width = w, height = h}}


updateNewFrame : Time -> Model -> Model
updateNewFrame t model =
    { model
        | time = t
        , startTime = if model.startTime == -1 then t else model.startTime
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes       (\size -> Resize size.width size.height)
        , AnimationFrame.times (\time -> NewFrame time)
        ]


main =
   programWithFlags
     { init = init
     , view = view
     , update = update
     , subscriptions = subscriptions
     }

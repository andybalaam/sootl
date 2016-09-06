import AnimationFrame exposing (times)
import Html exposing (Html)
import Html.App exposing (programWithFlags)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time exposing(Time)
import Window


type alias Flags =
    { width : Int
    , height : Int
    }


type Msg = Resize Int Int | NewFrame Time | BaseClicked Int


type HitShape = Circle Float Float Float


hitCircle : Float -> Float -> Float -> HitShape
hitCircle x y r = Circle x y r


type alias Light =
    Time ->
        { hitboxes : List HitShape
        , svgs     : List (Svg Msg)
        }


type alias Level =
    { background : Time -> List (Svg Msg)
    , lights : List Light
    }


levels : List Level
levels =
    [ level0
    ]


type alias Model =
    { screen :
        { width : Int
        , height : Int
        }
    , startTime : Time
    , time : Time
    , level : Level
    , levelNum : Int
    , player : { position : Int }
    }


darkGreyBackground : Time -> List (Svg Msg)
darkGreyBackground t =
    [ rect
        [ x "-300"
        , y "-300"
        , width "600"
        , height "600"
        , fill "#443333"
        ]
        []
    ]


slowlyCirclingCircle : Light
slowlyCirclingCircle time =
    let t = time - 5
        rr =
            if t < 7       then 95
            else if t < 12 then 95 - 45 * ((t-7)/5)
            else                50
        ry =
            if t < 7 then 0.7
            else if t < 12 then 0.7 + 0.3 * ((t-7)/5)
            else 1
        cxx = if t < 0 then 300 - ((t+5) * 60) else -rr * sin t
        cyy = if t < 0 then -ry * rr           else -ry * rr * cos t
    in
        { hitboxes = [ Circle cxx cyy 15 ]
        , svgs =
            [ circle
                [ cx <| toString cxx
                , cy <| toString cyy
                , r "15"
                , fill "#eeeeff"
                , opacity "0.7"
                ]
                []
            ]
            ++ (message time 4.5 4.0 cxx (cyy - 20) "Stay away")
            ++ (message time 4.5 4.0 cxx (cyy + 27) "from this!")
        }


level0 : Level
level0 =
    { background =
        darkGreyBackground
    , lights =
        [ slowlyCirclingCircle
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
        , level = level0
        , levelNum = 0
        , player = { position = 0 }
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
        trans = "translate("
            ++ (toString tx) ++ ","
            ++ (toString ty) ++ "),scale("
            ++ (toString (min/200))
            ++ "),translate(100, 100)"
    in
        svg
        [ width  <| toString sw
        , height <| toString sh
        ]
        [ g
            [
                transform trans
            ]
            (  (viewBackgrounds model time)
            ++ (viewBases  model time)
            ++ (viewPlayer model time)
            ++ (viewLights model time)
            )
        ]


viewBases model time =
    (  (viewBase model time -40 0 0)
    ++ (viewBase model time  40 0 1)
    ++ (message time 2.5 2.0 40 -25 "Touch to move here")
    )


dist : Float -> Float -> Float -> Float -> Float
dist x1 y1 x2 y2 =
    let
        dx = x1 - x2
        dy = y1 - y2
    in
        sqrt (dx*dx + dy*dy)


circleIntersectsHitShape : Float -> Float -> Float -> HitShape -> Bool
circleIntersectsHitShape x y r shape =
    case shape of
        Circle hx hy hr -> (dist x y hx hy) < (r + hr)


circleIntersectsLight : Time -> Float -> Float -> Float -> Light -> Bool
circleIntersectsLight time x y r light =
    List.any (circleIntersectsHitShape x y r) (light time).hitboxes


circleIsLit : Model -> Time -> Float -> Float -> Float -> Bool
circleIsLit model time x y r =
    List.any (circleIntersectsLight time x y r) model.level.lights


viewBase : Model -> Time -> Float -> Float -> Int -> List (Svg Msg)
viewBase model time x y which =
    let
        rad = 20
        f =
            if (circleIsLit model time x y rad) then
                "#550000"
            else
                "#005500"
    in
    [ g
        [ transform
            <| "translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")"
        ]
        (
            [ circle
                [ fill f
                , stroke "#000000"
                , strokeWidth "1px"
                , cx "0"
                , cy "0"
                , r (toString rad)
                , onMouseDown (BaseClicked which)
                ]
                []
            ]
        )
    ]


playerHappyFace model time =
    [ g
        [ transform "scale(2.1,2.1)" ]
        [ circle
            [ fill "#00ff00"
            , stroke "#000000"
            , strokeWidth "1px"
            , cx "0"
            , cy "0"
            , r "9.174984"
            ]
            []
        , Svg.path
            [ fill "#000000"
            , stroke "#000000"
            , strokeWidth "1px"
            , d "m -8.8934451,-4.1049 17.5271741,-5e-4 c -1.81995,6.5151 -5.462861,6.9077 -8.641304,2.6902 -3.287813,4.1465 -7.8469406,3.7682 -8.8858701,-2.6897 z"
            ]
            []
        , Svg.path
            [ fill "none"
            , stroke "#000000"
            , strokeWidth "1px"
            , d "m -4.5727929,3.7212 c 1.6559109,2.4074 5.7333219,1.5495 8.9673909,1.2228"
            ]
            []
        ]
    ]

message : Time -> Time -> Time -> Float -> Float -> String -> List (Svg Msg)
message time tstart tlength xx yy txt =
    let t = time - tstart in
        if t < 0 then
            []
        else
            let sz = 1 + t * 0.4
                op = 1.0 * (tlength - t)
            in
                if op < 0 then
                    []
                else
                    [ text'
                        [ x <| toString xx
                        , y <| toString yy
                        , fontSize "8"
                        , fontFamily "arial,sans-serif"
                        , textAnchor "middle"
                        , fontVariant "small-caps"
                        , fill "#ffffff"
                        , transform <|
                            "translate("
                                ++ (toString (xx * (1 - sz))) ++ ","
                                ++ (toString (yy * (1 - sz))) ++ ")"
                            ++ ",scale("
                                ++ (toString sz) ++ ", "
                                ++ (toString sz) ++ ")"
                        , opacity <| toString op
                        ]
                        [ text txt ]
                    ]


viewPlayer : Model -> Time -> List (Svg Msg)
viewPlayer model time =
    let x =
        if model.player.position == 0 then -40 else 40
    in
        [ g
            [ transform <| "translate(" ++ (toString x) ++ ",0)"
            ]
            (  (playerHappyFace model time)
            ++ (message time 0.5 2.0 0 -25 "This is you")
            )
        ]


viewLights : Model -> Time -> List (Svg Msg)
viewLights model time =
    ( List.concat <| List.map (\lig -> (lig time).svgs) model.level.lights )


viewBackgrounds : Model -> Time -> List (Svg Msg)
viewBackgrounds model time =
    model.level.background time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let m =
        case msg of
            Resize w h -> updateResize w h model
            NewFrame t -> updateNewFrame t model
            BaseClicked which -> updateMoveBase which model
    in
        (m, Cmd.none)

updateMoveBase : Int -> Model -> Model
updateMoveBase which model =
    let p = model.player
    in
        {model | player = {p | position=which}}

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

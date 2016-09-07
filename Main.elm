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


type alias Light =
    { hitboxes : List HitShape
    , svgs     : List (Svg Msg)
    }


type alias Level =
    { background : LevelTime -> List (Svg Msg)
    , lights : List (LevelTime -> Light)
    , bases : List HitShape
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
    , player :
        { position : Int
        }
    }


-- Time we are through a level in seconds
type LevelTime = LevelTime Float


levelTime : Model -> LevelTime
levelTime model =
    LevelTime <| (model.time - model.startTime) / 1000

secs : LevelTime -> Float
secs t = case t of LevelTime ti -> ti


lightsAtTime : Model -> List Light
lightsAtTime model =
    let t = levelTime model in
        List.map (\lig -> lig t) model.level.lights


darkGreyBackground : LevelTime -> List (Svg Msg)
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


slowlyCirclingCircle : LevelTime -> Light
slowlyCirclingCircle time =
    let t = (secs time) - 5
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
        msg = message time (LevelTime 4.5) (LevelTime 4.0)
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
            ++ (msg cxx (cyy - 20) "Stay away")
            ++ (msg cxx (cyy + 28) "from this!")
        }


level0 : Level
level0 =
    { background =
        darkGreyBackground
    , lights =
        [ slowlyCirclingCircle
        ]
    , bases =
        [ Circle -40 0 20
        , Circle  40 0 20
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
        time = levelTime model
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


viewBases : Model -> LevelTime -> List (Svg Msg)
viewBases model time =
    ((List.concat <| List.indexedMap (viewBase model time) model.level.bases)
        ++ (message
            time (LevelTime 2.5) (LevelTime 2.0)
            40 -25
            "Touch to move here"
        )
    )


dist : Float -> Float -> Float -> Float -> Float
dist x1 y1 x2 y2 =
    let
        dx = x1 - x2
        dy = y1 - y2
    in
        sqrt (dx*dx + dy*dy)


intersect : HitShape -> HitShape -> Bool
intersect s1 s2 =
    case s1 of
        Circle x1 y1 r1 ->
            case s2 of
                Circle x2 y2 r2 -> (dist x1 y1 x2 y2) < (r1 + r2)


isLit : Model -> LevelTime -> HitShape -> Bool
isLit model time shape =
    let intersectsLight time shape light =
        List.any (intersect shape) (light time).hitboxes
    in
        List.any (intersectsLight time shape) model.level.lights


viewBase : Model -> LevelTime -> Int -> HitShape -> List (Svg Msg)
viewBase model time which baseShape =
    let
        f =
            if (isLit model time baseShape) then
                "#550000"
            else
                "#005500"
    in
        case baseShape of
            Circle x y rad ->
                [ circle
                    [ fill f
                    , stroke "#000000"
                    , strokeWidth "1px"
                    , cx <| toString x
                    , cy <| toString y
                    , r  <| toString rad
                    , onMouseDown (BaseClicked which)
                    ]
                    []
                ]


playerHappyFace : Model -> LevelTime -> List (Svg Msg)
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


message :
    LevelTime -> LevelTime -> LevelTime -> Float -> Float -> String
    -> List (Svg Msg)
message time tstart tlength xx yy txt =
    let t = (secs time) - (secs tstart) in
        if t < 0 then
            []
        else
            let sz = 1 + t * 0.4
                op = 1.0 * ((secs tlength) - t)
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


viewPlayer : Model -> LevelTime -> List (Svg Msg)
viewPlayer model time =
    let x =
        if model.player.position == 0 then -40 else 40
    in
        [ g
            [ transform <| "translate(" ++ (toString x) ++ ",0)"
            ]
            ( (playerHappyFace model time)
            ++
            (message time (LevelTime 0.5) (LevelTime 2.0) 0 -25 "This is you")
            )
        ]


viewLights : Model -> LevelTime -> List (Svg Msg)
viewLights model time =
    ( List.concat <| List.map (\lig -> (lig time).svgs) model.level.lights )


viewBackgrounds : Model -> LevelTime -> List (Svg Msg)
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

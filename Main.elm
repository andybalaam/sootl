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


type Msg = Resize Int Int | NewFrame Time | BaseClicked Int | RestartClicked


type alias LevelPointDef = {x : Float, y : Float}
type LevelPoint = LevelPoint LevelPointDef
coords : LevelPoint -> LevelPointDef
coords p =
    case p of LevelPoint p -> p
pt : Float -> Float -> LevelPoint
pt x y =
    LevelPoint {x=x, y=y}
ptadd : LevelPoint -> LevelPoint -> LevelPoint
ptadd p1 p2 =
    let
        c1 = coords p1
        c2 = coords p2
    in
        pt (c1.x + c2.x) (c1.y + c2.y)


type HitShape = Circle LevelPoint Float
makeCircle : Float -> Float -> Float -> HitShape
makeCircle x y r = Circle (LevelPoint {x = x, y = y}) r


type alias Light =
    { hitboxes : List HitShape
    , svgs     : List (Svg Msg)
    }


type alias LevelDef =
    { background : LevelTime -> List (Svg Msg)
    , lights : List Sprite
    , bases : List HitShape
    }

type Level = Level LevelDef

levelDef : Level -> LevelDef
levelDef level =
    case level of Level def -> def

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
        , deathTime : Maybe Time
        }
    }


-- Time we are through a level in seconds
type LevelTime = LevelTime Float
ti : Float -> LevelTime
ti t =
    LevelTime t


levelTime : Model -> LevelTime
levelTime model =
    let t =
        case model.player.deathTime of
            Nothing -> model.time
            Just dt -> dt
    in
        LevelTime <| (t - model.startTime) / 1000


secs : LevelTime -> Float
secs t = case t of LevelTime ti -> ti


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


type alias SpriteFrame = Light

-- A sprite takes:
--   - the previous frame of the sprite
--   - a time delta since the previous frame
--   - the model (i.e. the whole world)
--   - a point give the position or position adjustment of the frame
--   - the time or time adjustment
-- and it returns the next frame
type alias Sprite = SpriteFrame -> LevelTime -> Model -> LevelPoint -> LevelTime -> SpriteFrame



whiteCircle : Sprite
whiteCircle lastFrame deltaT model point time =
    let
        p = coords point
        s = 13
        c = "#ffffff"
        o = "0.7"
    in
        { hitboxes = [ makeCircle p.x p.y s ]
        , svgs =
            [ circle
                [ cx <| toString p.x
                , cy <| toString p.y
                , r  <| toString s
                , fill c
                , opacity o
                ]
                []
            ]
        }


moved : LevelPoint -> Sprite -> Sprite
moved offsetP sprite lastFrame deltaT model point time =
    sprite lastFrame deltaT model (ptadd point offsetP) time


type alias Angle = Float


circling : LevelPoint -> Float -> Angle -> Float -> Sprite -> Sprite
circling
    centre
    radius
    startAngle
    circleTime
    sprite
    lastFrame deltaT model point time =
    let
        p = coords point
        c = coords centre
        t = secs time
        theta = pi * 2 * t / circleTime
        i = theta + startAngle
        x = c.x + (radius * sin i)
        y = c.y + (radius * cos i)
    in
        sprite
            lastFrame
            deltaT
            model
            (LevelPoint {x=p.x+x, y=p.y+y})
            time

still : Sprite -> Sprite
still sprite =
    sprite


timeSlice : LevelTime -> Sprite -> Sprite -> Sprite
timeSlice sliceTime before after =
    \lastFrame deltaT model point time ->
        let
            t  = secs time
            st = secs sliceTime
        in
            if secs time < secs sliceTime then
                before lastFrame deltaT model point time
            else
                after lastFrame deltaT model point (ti (t - st))


slide : LevelTime -> LevelPoint -> LevelPoint -> Sprite -> Sprite -> Sprite
slide sliceTime startP endP during after lastFrame deltaT model point time =
    let
        ttime = secs time
        endTT = secs sliceTime
        p =
            let
                i = ttime / endTT
                j = sin ((2*i-1) * pi / 2)
                k = (1 + j) / 2
                startPP = coords startP
                endPP   = coords endP
                x = startPP.x + (k * (endPP.x - startPP.x))
                y = startPP.y + (k * (endPP.y - startPP.y))
            in
                LevelPoint {x=x, y=y}
        sliced = timeSlice sliceTime (moved p during) after
    in
        sliced lastFrame deltaT model point time


combine : SpriteFrame -> SpriteFrame -> SpriteFrame
combine f1 f2 =
    { hitboxes = f1.hitboxes ++ f2.hitboxes
    , svgs = f1.svgs ++ f2.svgs }


parallel : Sprite -> Sprite -> Sprite
parallel sprite1 sprite2 lastFrame deltaT model point time =
    combine
        (sprite1 lastFrame deltaT model point time)
        (sprite2 lastFrame deltaT model point time)


message : LevelTime -> LevelTime -> LevelPoint -> String -> Sprite
message startT endT deltaP txt lastFrame deltaT model point time =
    let
        t = (secs time) - (secs startT)
        pp = coords (ptadd point deltaP)
        sz = 1 + t * 0.4
        op = 1.0 * ((secs endT) - (secs startT) - t)
        svgs =
            if op < 0 then
                []
            else
                [ textSvg pp.x pp.y sz "#ffffff" op txt [] ]
    in
        { hitboxes = []
        , svgs = svgs
        }


mischiefCircle : Sprite
mischiefCircle =
    let
        c = whiteCircle
        msg1 = message (ti 0.5) (ti 3) (pt 0 -15) "Stay away"
        msg2 = message (ti 0.5) (ti 3) (pt 0  21) "from this!"
        stayAwayCircle = parallel (parallel msg1 msg2) c
    in
           timeSlice (ti 4) (moved (pt 200 -70) c)
        <| slide (ti 1) (pt 200 -70) (pt 0 -55) c
        <| timeSlice (ti 4) (moved (pt 0 -55) stayAwayCircle)
        <| slide (ti 0.5) (pt 0 -55) (pt 0 -75) c
        <| timeSlice (ti 0.5) (moved (pt 0 -75) c)
        <| timeSlice (ti 5) (circling (pt 0 0) 75 pi -5 c)
        <| timeSlice (ti 0.5) (moved (pt 0 -75) c)
        <| slide (ti 0.5) (pt 0 -75) (pt 0 -55) c
        <| timeSlice (ti 0.5) (moved (pt 0 -55) c)
        <| timeSlice (ti 5) (circling (pt 0 0) 55 pi -5 c)
        <| timeSlice (ti 1) (moved (pt 0 -55) c)
        <| timeSlice (ti 5) (circling (pt 0 0) 55 pi -5 c)
        <| moved (pt 0 -55) c


level0 : Level
level0 =
    Level
        { background =
            darkGreyBackground
        , lights =
            [ mischiefCircle
            ]
        , bases =
            [ makeCircle -40 0 20
            , makeCircle  40 0 20
            ]
        }


init : Flags -> (Model, Cmd Msg)
init flags =
    ( initModel flags, Cmd.none )

initModel : Flags -> Model
initModel flags =
    { screen =
        { width = flags.width
        , height = flags.height
        }
    , startTime = -1
    , time = 0
    , level = level0
    , levelNum = 0
    , player =
        { position = 0
        , deathTime = Nothing
        }
    }


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
            (  (viewBackgrounds model)
            ++ (viewBases  model)
            ++ (viewPlayer model)
            ++ (viewLights model)
            ++ (viewRestartButton model)
            )
        ]


viewBases : Model -> List (Svg Msg)
viewBases model =
    let
        time = levelTime model
    in
        ((List.concat <| List.indexedMap (viewBase model time) (levelDef model.level).bases)
            ++ (baseMessage
                time (LevelTime 2.5) (LevelTime 2.0)
                (LevelPoint {x=40, y=-25})
                "Touch to move here"
            )
        )


dist : LevelPoint -> LevelPoint -> Float
dist p1 p2 =
    let
        pp1 = coords p1
        pp2 = coords p2
        dx = pp1.x - pp2.x
        dy = pp1.y - pp2.y
    in
        sqrt (dx*dx + dy*dy)


intersect : HitShape -> HitShape -> Bool
intersect s1 s2 =
    case s1 of
        Circle p1 r1 ->
            case s2 of
                Circle p2 r2 -> (dist p1 p2) < (r1 + r2)


isLit : Model -> LevelTime -> HitShape -> Bool
isLit model time shape =
    let intersectsLight time shape light =
        List.any (intersect shape) (spriteFrame model light).hitboxes
    in
        List.any (intersectsLight time shape) (levelDef model.level).lights


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
            Circle pp rad ->
                let p = coords pp in
                    [ circle
                        [ fill f
                        , stroke "#000000"
                        , strokeWidth "1px"
                        , cx <| toString p.x
                        , cy <| toString p.y
                        , r  <| toString rad
                        , onMouseDown (BaseClicked which)
                        ]
                        []
                    ]


playerSadFace : Model -> LevelTime -> List (Svg Msg)
playerSadFace model time =
    let
        msg = baseMessage time time (LevelTime 1.0)
    in
        [ g
            [ transform "scale(2.1,2.1)" ]
            (
                [ circle
                    [ fill "#ff3100"
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
                    , d "m -7.7,-5.7 16.7303048,5.2248 c -0.459938,0.7095 -0.918545,1.3192 -1.372437,1.8333 -0.453892,0.5141 -0.960714,0.097 -1.401791,0.4236 l -0.920862,0.4509 -0.67903,-2.5808 -1.252743,1.3298 -0.88384,-1.5369 -1.359301,1.4435 -1.180369,-1.3734 c -4.3744806,2.9777 -8.6135091,1.2574 -7.6799318,-5.2165 z"
                    ]
                    []
                , Svg.path
                    [ fill "none"
                    , stroke "#000000"
                    , strokeWidth "1px"
                    , d "m -4.5727929,4.2212 c 6.5205429,-1.4331 9.0681149,-1.0832 8.9673909,1.2228"
                    ]
                    []
                ]
            ++ (msg (LevelPoint {x=0, y=-12}) "Bad luck")
            ++ (msg (LevelPoint {x=0, y=16})  "You were seen!")
            )
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


textSvg : Float -> Float -> Float -> String -> Float -> String
    -> List (Attribute Msg) -> Svg Msg
textSvg px py scale colour op txt attrs =
    text'
        ( [ x <| toString px
        , y <| toString py
        , fontSize "8"
        , fontFamily "arial,sans-serif"
        , textAnchor "middle"
        , fontVariant "small-caps"
        , fill colour
        , transform <|
            "translate("
                ++ (toString (px * (1 - scale))) ++ ","
                ++ (toString (py * (1 - scale))) ++ ")"
            ++ ",scale("
                ++ (toString scale) ++ ", "
                ++ (toString scale) ++ ")"
        , opacity <| toString op
        ] ++ attrs )
        [ text txt ]


baseMessage :
    LevelTime -> LevelTime -> LevelTime -> LevelPoint -> String
    -> List (Svg Msg)
baseMessage time tstart tlength p txt =
    let
        t = (secs time) - (secs tstart)
        pp = coords p
    in
        if t < 0 then
            []
        else
            let sz = 1 + t * 0.4
                op = 1.0 * ((secs tlength) - t)
            in
                if op < 0 then
                    []
                else
                    [ textSvg pp.x pp.y sz "#ffffff" op txt [] ]


viewPlayer : Model -> List (Svg Msg)
viewPlayer model =
    let
        x = if model.player.position == 0 then -40 else 40
        render = if isPlayerAlive model then playerHappyFace else playerSadFace
        time = levelTime model
    in
        [ g
            [ transform <| "translate(" ++ (toString x) ++ ",0)"
            ]
            ( (render model time)
            ++
            (baseMessage
                time
                (LevelTime 0.5)
                (LevelTime 2.0)
                (LevelPoint {x=0, y=-25})
                "This is you"
            )
            )
        ]


isPlayerAlive : Model -> Bool
isPlayerAlive model =
    case model.player.deathTime of
        Nothing -> True
        _       -> False


viewRestartButton : Model -> List (Svg Msg)
viewRestartButton model =
    let
        t = levelTime model
    in
        if isPlayerAlive model then
            []
        else
            let
                md = [onMouseDown RestartClicked]
            in
                [ textSvg 0 80 3 "#55ffff" 0.9 "Try again" md ]


nullSpriteFrame =
    { hitboxes = []
    , svgs     = []
    }

nullTime =
    LevelTime 0


spriteFrame : Model -> Sprite -> SpriteFrame
spriteFrame model sprite =
    let
        time = levelTime model
    in
        sprite nullSpriteFrame nullTime model (LevelPoint {x=0, y=0}) time


viewLights : Model -> List (Svg Msg)
viewLights model =
    ( List.concat
        <| List.map
            (\lig -> (spriteFrame model lig).svgs)
            (levelDef model.level).lights
    )


viewBackgrounds : Model -> List (Svg Msg)
viewBackgrounds model =
    (levelDef model.level).background (levelTime model)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let m =
        case msg of
            Resize w h -> updateResize w h model
            NewFrame t -> updateNewFrame t model
            BaseClicked which -> updateMoveBase which model
            RestartClicked ->
                ( initModel
                    { width = model.screen.width
                    , height = model.screen.height
                    } )
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


getItem : Int -> List a -> Maybe a
getItem n xs =
    List.head <| List.drop n xs


noShape : HitShape
noShape =
    makeCircle 0 0 0


calcDeathTime pl model baseShape t =
    case pl.deathTime of
        Just dt -> Just dt
        Nothing ->
            if (isLit model (levelTime model) baseShape) then
                Just t
            else
                Nothing


updateNewFrame : Time -> Model -> Model
updateNewFrame t model =
    let
        pl = model.player
        st = if model.startTime == -1 then t else model.startTime
        baseShape =
            Maybe.withDefault noShape <|
                getItem model.player.position (levelDef model.level).bases
    in
        { model
            | time = t
            , startTime = st
            , player = { pl | deathTime = calcDeathTime pl model baseShape t }
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

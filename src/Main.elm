module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html, div, h1, img, input, p, span, text)
import Html.Attributes exposing (autofocus, class, id, src, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import Process
import Task
import Time exposing (..)



---- MODEL ----


type TyperStatus
    = Next
    | Done


type alias Model =
    { menu : String
    , prompt : String
    , log : String
    , stdin : String
    , wait : Float
    , typer : ( TyperStatus, ( String, String ) )
    }


menu : String
menu =
    """
    Type one of the following commands at the prompt
    ------------------------------------------------
    help    - display the main commands menu
    about   - display info about me
    contact - display my contact data
    extra   - display additional options
    """


extra =
    """
    cat [option] - about me with options :).
        Options:
            -s - brief summary
            -e - my work experience
            -p - some of my projects
    clear        - clears the screen and returns the prompt
    menu [option], help [option] -commands menu with options
        Options:
            -a - all commands options
    whatisthis   - info about this app
    """


summary =
    """
    -----------------
    Hi, I am Bogdan
    -----------------
    A full stack web developer working with Elixir, Javascript, React, React Native and Elm. I love the web, I love functional programming and I love hiking and trailrunning.
    """


experience =
    """
    ---------------------
    Some of my jobs
    ---------------------
    2019 - now - Full stack developer - Deutsche Back, Bucharest
    2017 - 2019 Full stack web developer - Aenvira Solutions, Bucharest
    2009 - 2017 IT Project Lead - OMV Petrom Global Solutions, Bucharest/Vienna
    2005 - 2009 Project/Team Lead, Software Integration - Crescendo International, Bucharest
    2004 - 2005 Software developer/ IT Consultant - Media Network International, Bucharest
    """


projects =
    """
    --------------------
    Some projects
    --------------------
    shrd2.com - Real time web app built with Elixir, Phoenix LiveView and Svelte.
    bogdannenu.com - Small Elm experiment.
    https://github.com/bnenu/bearnode - Functional, dependecy-less web server in NodeJS build with ADTs.
    https://github.com/bnenu/exo - A functional reactive state management experiment build RxJS subjects and tagged unions.
    """


contact =
    """
    hello_at_bogdannenu.com
    """


pageInfo =
    """
    This app is an experiment to build an interactive terminal experience with Elm 0.19.
    """


beforePrompt : String
beforePrompt =
    "~"


promptSymbol : String
promptSymbol =
    " $ "


init : ( Model, Cmd Msg )
init =
    ( { menu = menu
      , prompt = beforePrompt ++ promptSymbol
      , log = ""
      , stdin = ""
      , wait = 0.15 * 1000
      , typer = ( Next, ( "", "help" ) )
      }
    , getCurrentTime
    )



---- UPDATE ----


type Msg
    = NoOp
    | KeyDown Int
    | Stdin String
    | NextKey
    | ScrollToBottom String
    | NewTime Int


type Command
    = Menu
    | Cat
    | Contact
    | Clear
    | Extra
    | Whatisthis
    | ShowTime
    | Noc
    | Empty


parseInput stdin =
    let
        parsed =
            stdin
                |> String.trim
                |> String.split " "

        cmd =
            parsed
                |> List.head
                |> Maybe.withDefault ""

        args =
            parsed
                |> List.tail
                |> Maybe.withDefault []
    in
    ( cmd, args )


matchCommand : String -> Command
matchCommand s =
    case s of
        "cat" ->
            Cat

        "about" ->
            Cat

        "contact" ->
            Contact

        "help" ->
            Menu

        "menu" ->
            Menu

        "clear" ->
            Clear

        "extra" ->
            Extra

        "whatisthis" ->
            Whatisthis

        "time" ->
            ShowTime

        "" ->
            Empty

        _ ->
            Menu


handleCommand : Model -> List String -> Command -> ( Model, Cmd Msg )
handleCommand model args cmd =
    case cmd of
        Noc ->
            let
                nl =
                    model.log ++ "\n" ++ "command not found"
            in
            ( { model | log = nl }, Cmd.none )

        Empty ->
            ( model, Cmd.none )

        Menu ->
            let
                m =
                    case args of
                        [ "-a" ] ->
                            menu ++ "\n" ++ extra

                        [] ->
                            menu

                        _ ->
                            "unknown arguments"

                nl =
                    model.log ++ "\n" ++ m
            in
            ( { model | log = nl }, Cmd.none )

        Clear ->
            ( { model | log = "" }, Cmd.none )

        Cat ->
            let
                text =
                    case args of
                        [ "-s" ] ->
                            summary

                        [ "-e" ] ->
                            experience

                        [ "-p" ] ->
                            projects

                        [ "-s", "-e" ] ->
                            summary ++ "\n" ++ experience

                        [ "-e", "-s" ] ->
                            experience ++ "\n" ++ summary

                        [ "-e", "-p" ] ->
                            experience ++ "\n" ++ projects

                        [ "-p", "-e" ] ->
                            projects ++ "\n" ++ experience

                        [ "-p", "-s" ] ->
                            projects ++ "\n" ++ summary

                        [ "-s", "-p" ] ->
                            summary ++ "\n" ++ projects

                        [] ->
                            summary ++ "\n" ++ projects

                        [ "-s", "-e", "-p" ] ->
                            summary ++ "\n" ++ experience ++ "\n" ++ projects

                        _ ->
                            "unknown arguments"

                nl =
                    model.log ++ "\n" ++ text
            in
            ( { model | log = nl }, Cmd.none )

        Contact ->
            let
                nl =
                    model.log ++ "\n" ++ contact
            in
            ( { model | log = nl }, Cmd.none )

        Extra ->
            let
                nl =
                    model.log ++ "\n" ++ extra
            in
            ( { model | log = nl }, Cmd.none )

        Whatisthis ->
            let
                nl =
                    model.log ++ "\n" ++ pageInfo
            in
            ( { model | log = nl }, Cmd.none )

        ShowTime ->
            ( model, getCurrentTime )


typer t =
    let
        status =
            t |> Tuple.first

        state =
            t |> Tuple.second

        acc =
            state |> Tuple.first

        x =
            state
                |> Tuple.second
                |> String.uncons

        newState =
            case x of
                Just ( letter, reminder ) ->
                    case reminder of
                        "" ->
                            ( Done, ( acc ++ String.fromChar letter, reminder ) )

                        _ ->
                            ( Next
                            , ( acc ++ String.fromChar letter
                              , reminder
                              )
                            )

                Nothing ->
                    ( Done, ( acc, "" ) )
    in
    newState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Stdin value ->
            ( { model | stdin = value }, Cmd.none )

        KeyDown key ->
            if key == 13 then
                let
                    nl =
                        model.log ++ "\n" ++ model.prompt ++ model.stdin

                    m =
                        { model | log = nl, stdin = "" }

                    ( cmd, args ) =
                        parseInput model.stdin

                    ( updatedModel, command ) =
                        cmd
                            |> matchCommand
                            |> handleCommand m args
                in
                ( updatedModel
                , Cmd.batch
                    [ command
                    , Process.sleep 100
                        |> Task.perform (always (ScrollToBottom "terminal"))
                    ]
                )

            else
                ( model, Cmd.none )

        NextKey ->
            let
                ( status, state ) =
                    typer model.typer

                cmd =
                    case status of
                        Done ->
                            --- Simulate enter at the end of the command
                            Task.perform KeyDown (Task.succeed 13)

                        Next ->
                            Cmd.none
            in
            ( { model
                | typer = ( status, state )
                , stdin = Tuple.first state
              }
            , cmd
            )

        ScrollToBottom container ->
            ( model, jumpToBottom container )

        NewTime time ->
            let
                pTime =
                    prettyTime time

                nl =
                    model.log
                        ++ "\n"
                        ++ "Log time: "
                        ++ pTime
                        ++ " [UTC] on bogdannenu.com"
            in
            ( { model | log = nl }, Cmd.none )



---- VIEW ----


viewLog log =
    let
        lines =
            log
                |> String.split "\n"
                |> List.map (\x -> p [] [ text x ])
    in
    div []
        lines


view : Model -> Html Msg
view model =
    div [ id "terminal" ]
        [ div [ class "container" ]
            [ viewLog model.log
            , span [] [ text model.prompt ]
            , input
                [ type_ "text"
                , class "input"
                , value model.stdin
                , onKeyDown KeyDown
                , onInput Stdin
                , autofocus True
                ]
                []
            ]
        ]



---- UTILS -----


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


getCurrentTime : Cmd Msg
getCurrentTime =
    Time.now
        |> Task.andThen (\t -> Task.succeed (Time.posixToMillis t))
        |> Task.perform NewTime


prettyTime time =
    String.fromInt (timeToYear time)
        ++ " "
        ++ int00 (timeToMonth time)
        ++ " "
        ++ int00 (timeToDay time)
        ++ " "
        ++ int00 (timeToHour time)
        ++ ":"
        ++ int00 (timeToMinute time)
        ++ ":"
        ++ int00 (timeToSecond time)


int00 : Int -> String
int00 i =
    String.padLeft 2 '0' (String.fromInt i)


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


timeToYear : Int -> Int
timeToYear time =
    Time.toYear Time.utc (Time.millisToPosix time)


timeToMonth : Int -> Int
timeToMonth time =
    Time.toMonth Time.utc (Time.millisToPosix time) |> monthToInt


timeToDay : Int -> Int
timeToDay time =
    Time.toDay Time.utc (Time.millisToPosix time)


timeToHour : Int -> Int
timeToHour time =
    Time.toHour Time.utc (Time.millisToPosix time)


timeToMinute : Int -> Int
timeToMinute time =
    Time.toMinute Time.utc (Time.millisToPosix time)


timeToSecond : Int -> Int
timeToSecond time =
    Time.toSecond Time.utc (Time.millisToPosix time)



---- PROGRAM ----


subscriptions model =
    let
        p =
            model.typer
                |> Tuple.second
                |> Tuple.second

        typing =
            if p == "" then
                Sub.none

            else
                Time.every model.wait (always NextKey)
    in
    Sub.batch [ typing ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }

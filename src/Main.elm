module Main exposing (..)

import Element as E
import Html exposing (Html)
import Navigation exposing (Location)
import Route exposing (Route)
import Styles as S
import Window
import View.Master as Master
import View.Spinner as SpinnerView
import Task
import Spinner


type Page
    = Blank
    | NotFound
    | Errored
    | Home
    | Player


type PageState
    = Loading Spinner.Model
    | Loaded Page



---- MODEL ----


type alias Model =
    { navOpen : Bool
    , device : E.Device
    , songs : Songs
    , pageState : PageState
    }


type alias Songs =
    List String


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (Route.fromLocation location)
        { navOpen = False
        , device = E.classifyDevice <| Window.Size 0 0
        , songs = []
        , pageState = Loading Spinner.init
        }



---- UPDATE ----


type Msg
    = SetRoute (Maybe Route)
    | ToggleMenu
    | WindowResize Window.Size
    | SpinnerMsg Spinner.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.pageState msg model


updatePage : PageState -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case ( msg, page ) of
        ( SetRoute route, _ ) ->
            let
                ( newModel, newCmd ) =
                    setRoute route model
            in
                newModel ! [ getWindowSize, newCmd ]

        ( ToggleMenu, _ ) ->
            { model | navOpen = not model.navOpen } ! []

        ( WindowResize size, _ ) ->
            { model | device = E.classifyDevice size } ! []

        ( SpinnerMsg spinnerMsg, Loading spinner ) ->
            let
                spinnerModel =
                    Spinner.update spinnerMsg spinner
            in
                { model | pageState = Loading spinnerModel } ! []

        ( _, Loaded NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            model ! []

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model ! []



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowResize
        , Sub.map SpinnerMsg Spinner.subscription
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        masterFrame =
            Master.frame <| { navOpen = model.navOpen, menuMsg = ToggleMenu }
    in
        case model.pageState of
            Loading spinnerModel ->
                masterFrame <|
                    SpinnerView.spinner spinnerModel

            Loaded _ ->
                masterFrame <| E.el S.None [] (E.text "TEST")


getWindowSize : Cmd Msg
getWindowSize =
    Task.perform WindowResize Window.size


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } ! []

        Just Route.Root ->
            model ! [ Route.modifyUrl Route.Home ]

        Just Route.Home ->
            { model | navOpen = False, pageState = Loading Spinner.init }
                ! []

        Just (Route.Player youTubeID) ->
            { model | navOpen = False, pageState = Loading Spinner.init }
                ! []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

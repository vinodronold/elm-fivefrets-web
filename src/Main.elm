module Main exposing (..)

import Element as E
import Html exposing (Html)
import Navigation exposing (Location)
import Route exposing (Route)
import Styles as S
import Data.Songs as SongsData
import Window
import View.Master as Master
import View.Spinner as SpinnerView
import Task
import Spinner
import Dict
import Pages.Home as HomePage
import Pages.Errored as Errored
import Http


type Page
    = Blank
    | NotFound
    | Errored Errored.PageLoadError
    | Home
    | Player


type PageState
    = Loading Spinner.Model
    | Loaded Page



---- MODEL ----


type alias Model =
    { navOpen : Bool
    , device : E.Device
    , songs : SongsData.Songs
    , pageState : PageState
    }


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (Route.fromLocation location)
        { navOpen = False
        , device = E.classifyDevice <| Window.Size 0 0
        , songs = Dict.empty
        , pageState = Loading Spinner.init
        }



---- UPDATE ----


type Msg
    = SetRoute (Maybe Route)
    | ToggleMenu
    | WindowResize Window.Size
    | SpinnerMsg Spinner.Msg
    | HomeLoaded (Result Http.Error SongsData.Songs)


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

        ( HomeLoaded (Ok songs), _ ) ->
            { model | pageState = Loaded Home, songs = Dict.union songs model.songs } ! []

        ( HomeLoaded (Err errMessage), _ ) ->
            { model | pageState = Loaded (Errored <| Errored.pageLoadError <| toString errMessage) } ! []

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
        , spinnerSubscription model
        ]


spinnerSubscription : { a | pageState : PageState } -> Sub Msg
spinnerSubscription { pageState } =
    case pageState of
        Loading _ ->
            Sub.map SpinnerMsg Spinner.subscription

        _ ->
            Sub.none



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

            Loaded (Errored err) ->
                masterFrame <| Errored.view err

            Loaded Home ->
                masterFrame <| HomePage.view model.songs

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
            let
                ( pageState, cmd ) =
                    if (Dict.isEmpty model.songs) then
                        ( Loading Spinner.init, Task.attempt HomeLoaded HomePage.load )
                    else
                        ( Loaded Home, Cmd.none )
            in
                { model | navOpen = False, pageState = pageState }
                    ! [ cmd ]

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

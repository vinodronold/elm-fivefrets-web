module View.Spinner exposing (spinner)

import Element as E
import Element.Attributes as A
import Spinner
import Styles as S
import ColorPalette exposing (appColors)


spinner : Spinner.Model -> E.Element S.Styles variation msg
spinner spinnerModel =
    let
        spinnerConfig =
            { lines = 18
            , length = 28
            , width = 4
            , radius = 42
            , scale = 1
            , corners = 1
            , opacity = 0.25
            , rotate = 0
            , direction = Spinner.Clockwise
            , speed = 0.5
            , trail = 60
            , translateX = 50
            , translateY = 50
            , shadow = True
            , hwaccel = False
            , color = always <| appColors.primary
            }
    in
        E.screen <|
            E.el S.None [ A.center, A.paddingTop 300 ] <|
                E.html <|
                    Spinner.view spinnerConfig spinnerModel

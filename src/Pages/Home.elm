module Pages.Home exposing (..)

import Data.Songs as Data
import Task exposing (Task)
import Http
import View.Songs exposing (listSongs)
import Element as E
import Styles as S


--- TASK ---


load : Task Http.Error Data.Songs
load =
    Data.getSongs
        |> Http.toTask


view : Data.Songs -> E.Element S.Styles variation msg
view =
    listSongs

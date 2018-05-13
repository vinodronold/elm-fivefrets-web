module View.Songs exposing (..)

import Data.Songs as Data
import Element as E
import Element.Attributes as A
import Route
import Styles as S
import Dict
import Types


listSongs : Data.Songs -> E.Element S.Styles variation msg
listSongs songs =
    E.column S.None [ A.spacing 10 ] <|
        List.map displaySong <|
            Dict.values songs


displaySong : Data.Song -> E.Element S.Styles variation msg
displaySong song =
    E.link (Route.href <| Route.Player song.youtube_id) <|
        E.row S.SongItem
            [ A.spacing 5 ]
            [ displaySongImg song
            , E.column S.None
                [ A.spacing 5, A.verticalCenter ]
                [ E.paragraph S.None [ A.paddingXY 10 0 ] [ E.text song.title ]
                ]
            ]


displaySongImg : Data.Song -> E.Element S.Styles variation msg
displaySongImg song =
    E.image S.None [ A.spread ] { src = Types.urlToString song.imgUrlDefault, caption = song.title }

module Types exposing (..)

import Json.Decode as Decode


---------------------------------------------------------------------------------------------------


type YouTubeID
    = YouTubeID String


decodeYouTubeID : Decode.Decoder YouTubeID
decodeYouTubeID =
    Decode.map YouTubeID Decode.string



---------------------------------------------------------------------------------------------------


type URL
    = URL String


urlToString : URL -> String
urlToString (URL s) =
    s


decodeUrl : Decode.Decoder URL
decodeUrl =
    Decode.map URL Decode.string



---------------------------------------------------------------------------------------------------

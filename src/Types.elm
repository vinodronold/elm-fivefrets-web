module Types exposing (..)

import Json.Decode as Decode
import UrlParser as Url


---------------------------------------------------------------------------------------------------


type YouTubeID
    = YouTubeID String


decodeYouTubeID : Decode.Decoder YouTubeID
decodeYouTubeID =
    Decode.map YouTubeID Decode.string


youTubeIDParser : Url.Parser (YouTubeID -> a) a
youTubeIDParser =
    Url.custom "SONG" (Ok << YouTubeID)


youTubeIDtoString : YouTubeID -> String
youTubeIDtoString (YouTubeID s) =
    s



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

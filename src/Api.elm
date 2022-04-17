module Api exposing (..)

import Http
import Json.Decode
import Json.Encode


type alias LoginRequest =
    { username : String
    , password : String
    }


type alias LoginResponse =
    -- TODO: Check these types for correctness
    { accessToken : String
    , tokenType : String
    , expiresIn : Int
    , refreshToken : String
    }


loginPost :
    (Result Http.Error LoginResponse -> msg)
    -> LoginRequest
    -> Cmd msg
loginPost msg req =
    Http.request
        { method = "POST"
        , expect = Http.expectJson msg loginResponseDecoder
        , headers = []

        -- TODO: remove hardcoded api host
        , url = "http://localhost:8080" ++ "/login"
        , body =
            req
                |> loginRequestEncoder
                |> Http.stringBody "application/x-www-form-urlencoded"
        , timeout = Nothing
        , tracker = Nothing
        }



--
-- Duplicating some code from a PB code generator solution
--
-- Login with PB messages would be nice
--


loginRequestEncoder : LoginRequest -> String
loginRequestEncoder v =
    "grant_type=password&username=" ++ v.username ++ "&password=" ++ v.password


requiredFieldEncoder : String -> (a -> Json.Encode.Value) -> a -> a -> Maybe ( String, Json.Encode.Value )
requiredFieldEncoder name encoder default v =
    if v == default then
        Nothing

    else
        Just ( name, encoder v )


loginResponseDecoder : Json.Decode.Decoder LoginResponse
loginResponseDecoder =
    Json.Decode.lazy <|
        \_ ->
            Json.Decode.succeed LoginResponse
                |> required "access_token" Json.Decode.string ""
                |> required "token_type" Json.Decode.string ""
                |> required "expires_in" Json.Decode.int 0
                |> required "refresh_token" Json.Decode.string ""


required : String -> Json.Decode.Decoder a -> a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
required name decoder default d =
    Json.Decode.map2 (|>) (withDefault default <| Json.Decode.field name decoder) d


withDefault : a -> Json.Decode.Decoder a -> Json.Decode.Decoder a
withDefault default decoder =
    Json.Decode.oneOf
        [ decoder
        , Json.Decode.succeed default
        ]

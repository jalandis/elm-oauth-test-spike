module Main exposing
    ( Model
    , Msg(..)
    , main
    , profileLink
    , update
    , view
    )

import Browser
import Browser.Navigation
import Effects
import Html exposing (Attribute, Html, a, li, span, text, ul)
import Html.Attributes exposing (attribute, href, style)
import Html.Events exposing (onClick)
import Platform.Sub exposing (Sub)
import Url



-- MAIN


main : Program () (Model Browser.Navigation.Key) Msg
main =
    Browser.application
        { init =
            \flags url key ->
                init flags url key
                    |> Tuple.mapSecond (Effects.toCmd key)
        , view = view
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond (Effects.toCmd model.key)
        , subscriptions = subscriptions

        -- What is the difference between these 2?
        --
        -- Handling with onClick is easier to test
        -- Are there unexpected issues with this choice beside the duplicate messages?
        , onUrlChange = always NoMsg
        , onUrlRequest = always NoMsg
        }



-- MODEL


type alias Model key =
    -- Abstract Browser.Navigation.Key is needed for testing
    { key : key
    , url : Url.Url
    }


init : () -> Url.Url -> key -> ( Model key, Effects.Effects Msg )
init _ url key =
    ( Model key url, [] )



-- UPDATE


type Msg
    = NoMsg
    | OnUrlRequest Browser.UrlRequest


update : Msg -> Model key -> ( Model key, Effects.Effects Msg )
update msg model =
    case msg of
        NoMsg ->
            ( model, [] )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | url = url }, [ Effects.InternalLinkClicked url ] )

                Browser.External href ->
                    ( model, [ Effects.ExternalLinkClicked href ] )



-- SUBSCRIPTIONS


subscriptions : Model key -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


testAttribute : String -> Attribute msg
testAttribute title =
    attribute "data-test" title


view : Model key -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ text "The current URL is: "
        , span
            [ style "font-weight" "bold"
            , testAttribute "current-url"
            ]
            [ text (Url.toString model.url) ]
        , ul []
            [ viewInternalLink "home-link" (homeLink model)
            , viewInternalLink "profile-link" (profileLink model)
            , viewExternalLink "external-link" "https://google.com"
            ]
        ]
    }


viewExternalLink : String -> String -> Html Msg
viewExternalLink testAttr url =
    li []
        [ a
            [ Browser.External url
                |> OnUrlRequest
                |> onClick
            , testAttribute testAttr
            , href url
            ]
            [ text url ]
        ]


viewInternalLink : String -> Url.Url -> Html Msg
viewInternalLink testAttr url =
    li []
        [ a
            [ Browser.Internal url
                |> OnUrlRequest
                |> onClick
            , testAttribute testAttr
            , href (Url.toString url)
            ]
            [ text (Url.toString url) ]
        ]


homeLink : Model key -> Url.Url
homeLink model =
    { protocol = model.url.protocol
    , host = model.url.host
    , port_ = model.url.port_
    , path = "/home"
    , query = Nothing
    , fragment = Nothing
    }


profileLink : Model key -> Url.Url
profileLink model =
    { protocol = model.url.protocol
    , host = model.url.host
    , port_ = model.url.port_
    , path = "/profile"
    , query = Nothing
    , fragment = Nothing
    }

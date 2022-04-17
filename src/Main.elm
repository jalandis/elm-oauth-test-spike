module Main exposing
    ( Model
    , Msg(..)
    , User(..)
    , main
    , profileLink
    , update
    , view
    )

import Api
import Browser
import Browser.Navigation
import Effects
import Html exposing (Attribute, Html, a, button, div, input, label, li, span, text, ul)
import Html.Attributes exposing (attribute, for, href, id, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
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
    , user : User
    }


type User
    = Anonymous LoginModel
    | AuthenitcatedUser Api.LoginResponse


type alias LoginModel =
    { username : Maybe String
    , password : Maybe String
    }


init : () -> Url.Url -> key -> ( Model key, Effects.Effects Msg )
init _ url key =
    ( { key = key
      , url = url
      , user = Anonymous { username = Nothing, password = Nothing }
      }
    , []
    )



-- UPDATE


type Msg
    = NoMsg
    | OnUrlRequest Browser.UrlRequest
    | UpdateLoginForm (LoginModel -> LoginModel)
    | HandleLoginRequest
    | HandleLoginResponse (Result Http.Error Api.LoginResponse)


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

        UpdateLoginForm fn ->
            case model.user of
                Anonymous loginForm ->
                    ( { model | user = fn loginForm |> Anonymous }, [] )

                _ ->
                    -- TODO: Show errors
                    ( model, [] )

        HandleLoginRequest ->
            case model.user of
                Anonymous loginForm ->
                    ( model
                    , [ Effects.Login HandleLoginResponse
                            { username = Maybe.withDefault "" loginForm.username
                            , password = Maybe.withDefault "" loginForm.password
                            }
                      ]
                    )

                _ ->
                    -- TODO: Show errors
                    ( model, [] )

        HandleLoginResponse rawResult ->
            case rawResult of
                Ok loginResponse ->
                    ( { model | user = AuthenitcatedUser loginResponse }, [] )

                Err _ ->
                    -- TODO: Show errors
                    ( model, [] )



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
        , case model.user of
            Anonymous loginForm ->
                div []
                    [ text "Login Required"
                    , div []
                        [ label [ for "username" ] [ text "Username:" ]
                        , input
                            [ id "username"
                            , Maybe.withDefault "" loginForm.username |> value
                            , onInput
                                (\username ->
                                    (\m -> { m | username = stringToMaybe username })
                                        |> UpdateLoginForm
                                )
                            ]
                            []
                        ]
                    , div []
                        [ label [ for "password" ] [ text "Password:" ]
                        , input
                            [ id "password"
                            , Maybe.withDefault "" loginForm.password |> value
                            , type_ "password"
                            , onInput
                                (\password ->
                                    (\m -> { m | password = stringToMaybe password })
                                        |> UpdateLoginForm
                                )
                            ]
                            []
                        ]
                    , button [ onClick HandleLoginRequest ] [ text "Login" ]
                    ]

            AuthenitcatedUser loginResponse ->
                div []
                    [ div [] [ text "Login Succeeded" ]
                    , div [] [ text loginResponse.accessToken ]
                    ]
        ]
    }


stringToMaybe : String -> Maybe String
stringToMaybe s =
    if s == "" then
        Nothing

    else
        Just s


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

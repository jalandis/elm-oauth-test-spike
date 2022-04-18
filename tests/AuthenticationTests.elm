module AuthenticationTests exposing (all)

import Api
import Effects
import Expect
import HomePage exposing (page)
import Json.Encode
import Main
import Test exposing (Test, describe, test)
import TestJourney as J
import TestSetup
import Url


initModel : Main.Model ()
initModel =
    { key = ()
    , url =
        { protocol = Url.Https
        , host = "test.com"
        , port_ = Nothing
        , path = ""
        , query = Nothing
        , fragment = Nothing
        }
    , user = Main.Anonymous { username = Nothing, password = Nothing }
    , error = Nothing
    }


all : Test
all =
    describe "authenitcation tests"
        [ test "login" <|
            \_ ->
                TestSetup.startTest initModel
                    |> J.see page.loginForm
                    |> J.input "john@test.com" page.loginForm.username
                    |> J.input "password" page.loginForm.password
                    |> J.click page.loginForm.submit
                    |> J.handleEffect
                        (\effect ->
                            case effect of
                                Effects.Login msg args ->
                                    J.EffectProcessed
                                        (Expect.equal
                                            { username = "john@test.com", password = "password" }
                                            args
                                        )
                                        (msg
                                            (Ok
                                                { accessToken = "access-token"
                                                , tokenType = "token"
                                                , expiresIn = 120
                                                , refreshToken = "refresh-token"
                                                }
                                            )
                                        )

                                _ ->
                                    J.EffectUnexpected
                        )
                    |> J.handleEffect
                        (\effect ->
                            case effect of
                                Effects.SetLocaleStorageItem args ->
                                    J.EffectSeen
                                        (Expect.equal
                                            { key = "elm-oauth-test-spike"
                                            , value =
                                                -- TODO: Deduplicate this code
                                                Json.Encode.encode 0
                                                    (Api.loginResponseEncoder
                                                        { accessToken = "access-token"
                                                        , tokenType = "token"
                                                        , expiresIn = 120
                                                        , refreshToken = "refresh-token"
                                                        }
                                                    )
                                            }
                                            args
                                        )

                                _ ->
                                    J.EffectUnexpected
                        )
                    |> J.dontSee page.loginForm
                    |> J.seeText "access-token" page.accessToken
                    |> J.finish
        ]

module NavigationTests exposing (all)

import Effects
import Expect
import HomePage exposing (page)
import Main
import Test exposing (Test, describe, test)
import TestJourney as J
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
    }


startTest : J.TestState (Main.Model ()) Main.Msg (Effects.Effect Main.Msg)
startTest =
    J.startApplication
        { view = Main.view
        , update = Main.update
        , model = initModel
        , onUrlRequest = Main.OnUrlRequest
        , onUrlChange = Main.OnUrlChange
        , effectToString = Debug.toString
        }


all : Test
all =
    describe "simple navigation tests"
        [ test "follow internal link" <|
            \_ ->
                startTest
                    |> J.seeText "https://test.com" page.currenturl
                    |> J.click page.profile
                    |> handleInternalNavigation (Main.profileLink initModel)
                    |> J.seeText "https://test.com/profile" page.currenturl
                    |> J.finish
        ]


handleInternalNavigation :
    Url.Url
    -> J.TestState model Main.Msg (Effects.Effect Main.Msg)
    -> J.TestState model Main.Msg (Effects.Effect Main.Msg)
handleInternalNavigation expectedUrl t =
    t
        |> J.handleEffect
            (\effect ->
                case effect of
                    Effects.InternalLinkClicked args ->
                        J.EffectSeen (Expect.equal expectedUrl args)

                    _ ->
                        J.EffectUnexpected
            )
        |> J.injectMsg (Main.OnUrlChange expectedUrl)

module TestSetup exposing (startTest)

import Effects
import Main
import TestJourney as J


startTest : Main.Model () -> J.TestState (Main.Model ()) Main.Msg (Effects.Effect Main.Msg)
startTest model =
    J.startApplication
        { view = Main.view
        , update = Main.update
        , model = model
        , onUrlRequest = always Main.NoMsg
        , onUrlChange = always Main.NoMsg
        , effectToString = Debug.toString
        }

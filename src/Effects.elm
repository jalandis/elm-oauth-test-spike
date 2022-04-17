module Effects exposing
    ( Effect(..)
    , Effects
    , toCmd
    )

import Api
import Browser.Navigation
import Http
import Url


type alias Effects msg =
    List (Effect msg)


type Effect msg
    = InternalLinkClicked Url.Url
    | ExternalLinkClicked String
    | Login (Result Http.Error Api.LoginResponse -> msg) Api.LoginRequest



-- map : (a -> msg) -> Effects a -> Effects msg
-- map fn =
--     List.map (mapEffect fn)
-- mapEffect : (a -> msg) -> Effect a -> Effect msg
-- mapEffect _ effect =
--     case effect of
--         InternalLinkClicked args ->
--             InternalLinkClicked args
--         ExternalLinkClicked args ->
--             ExternalLinkClicked args
--         Login m args ->
--            EffectRemoveAccountAccess (m >> fn) args


toCmd :
    Browser.Navigation.Key
    -> Effects msg
    -> Cmd msg
toCmd key effects =
    List.map (\e -> effectToCmd key e) effects
        |> Cmd.batch


effectToCmd :
    Browser.Navigation.Key
    -> Effect msg
    -> Cmd msg
effectToCmd key effect =
    case effect of
        InternalLinkClicked args ->
            Browser.Navigation.pushUrl key (Url.toString args)

        ExternalLinkClicked args ->
            Browser.Navigation.load args

        Login m args ->
            Api.loginPost m args

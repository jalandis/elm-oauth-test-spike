module HomePage exposing (page)

import TestJourney.Page as P


page =
    P.root []
        (\root ->
            { self = root
            , currenturl = P.singleTestAttr root "current-url"
            , home = P.singleTestAttr root "home-link"
            , profile = P.singleTestAttr root "profile-link"
            , external = P.singleTestAttr root "external-link"
            , loginForm =
                P.singleRecordTestAttr root
                    "login-form"
                    (\form ->
                        { self = form
                        , username = P.singleTestAttr form "username"
                        , password = P.singleTestAttr form "password"
                        , submit = P.singleTestAttr form "submit"
                        }
                    )
            , accessToken = P.singleTestAttr root "access-token"
            }
        )

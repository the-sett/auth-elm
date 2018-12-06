# auth-elm

Elm auth module for interacting with the-sett/auth-service.

This is unlikely to be useful to you directly, but you may find a nice example
here showing how a re-usable authentication package can be structured.

The idea here is to capture the interaction with an authentication API as an Elm
package and to present that as a simpler API that applications need authentication
can make use of. The core of the API is the authentication commands, which are:

```
login : Credentials -> Cmd Msg
refresh : Cmd Msg
logout : Cmd Msg
unauthed : Cmd Msg
```

The commands yield messages that must be given to its `update` function which has
this signature and related types:

```
type Status
    = Failed
    | LoggedOut
    | LoggedIn
        { scopes : List String
        , subject : String
        }

update : Msg -> Model -> ( Model, Cmd Msg, Maybe Status )
```

That is, each message will update the internal model, and may produce a change
to the current authentication status.

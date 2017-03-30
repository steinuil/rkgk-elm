module Pixiv exposing (Response, send, more, withOptions, login, refresh)

{-| Requests

@docs Response

@docs send, more, withOptions

@docs login, refresh
-}

import Pixiv.Types exposing (..)
import Pixiv.Decoders as Decoders
import Infix exposing (..)

import Http
import Platform.Cmd as Cmd
import Dict exposing (Dict)
import Json.Decode as Decode


{-| Response type without too much typing. -}
type alias Response t msg = Result Http.Error t -> msg


{-| Send a pre-constructed Pixiv request.

Returns both a Page and a Page Info, to give a bit of info about the page to
display in the UI (e.g. if it's an user works page return the User, if it was
a search return the search term etc)
-}
send : Response (Page, PageInfo) msg -> Request -> Cmd msg
send response request =
  let
    url =
      Dict.foldr (\k v -> (::) (k ++ "=" ++ v)) [] request.params
        |> String.join "&"
        |> (++) ("/" ++ "https://app-api.pixiv.net/" ++ request.url ++ "?")

    (method, auth) = case request.method of
      GetNoAuth -> ("GET", Nothing)
      Get x ->     ("GET", Just x)
      Post x ->    ("POST", Just x)

    decoder =
      Decode.map2 (,) Decoders.request (Decode.succeed request.return)
  in
    Http.send response
      <| httpRequest
        { url = url
        , method = method
        , token = auth
        , expect = decoder
        , body = Nothing
        }


{-| For list resources, Pixiv returns a "next url" to fetch the next page.
This function is to fetch that more easily (or fetch an arbitrary URL, if you
really want to).
-}
more : Response Page msg -> Maybe String -> Url -> Cmd msg
more response auth url =
  Http.send response
    <| httpRequest
      { url = "/" ++ url
      , method = "GET"
      , token = auth
      , expect = Decoders.request
      , body = Nothing
      }


{-| Change the parameters of a request. Merges them only if they're in the list
of allowed parameters.
-}
withOptions : List (String, String) -> Request -> Request
withOptions new request =
  let
    insertIfAllowed k v =
      if
        List.member k request.allowed
      then
        Dict.insert k v
      else
        identity

    newParams = Dict.foldr insertIfAllowed request.params (Dict.fromList new)
  in
    { request | params = newParams }


{-| Logs in and returns a LoginInfo, containing the tokens and basic user info.
-}
login : Response LoginInfo msg -> String -> String -> Cmd msg
login response name password =
  authRequest response
    [ Http.stringPart "grant_type" "password"
    , Http.stringPart "username" name
    , Http.stringPart "password" password
    ]


{-| Get new tokens if you already have a valid refresh token. -}
refresh : Response LoginInfo msg -> String -> Cmd msg
refresh response refreshToken =
  authRequest response
    [ Http.stringPart "grant_type" "refresh_token"
    , Http.stringPart "refresh_token" refreshToken
    ]


authRequest response data =
  Http.send response
    <| httpRequest
      { method = "POST"
      , token = Nothing
      , url = "/" ++ "https://oauth.secure.pixiv.net/auth/token"
      , body = Just <| Http.multipartBody <|
        --[ Http.stringPart "get_secure_url" "1"
        [ Http.stringPart "client_id" "bYGKuGVw91e0NMfPGp44euvGt59s"
        , Http.stringPart "client_secret" "HP3RmkgAmEGro0gn1x9ioawQE8WMfvLXDz3ZqxpK"
        ] ++ data
      , expect = Decoders.login
      }


httpRequest : { url : String, method : String, token : Maybe String, expect : Decode.Decoder a, body : Maybe Http.Body } -> Http.Request a
httpRequest { url, method, token, expect, body } =
  let
    auth = case token of
      Just t -> [ Http.header "Authorization" ("Bearer " ++ t) ]
      Nothing -> []

    headers =
      [ Http.header "App-OS" "ios"
      , Http.header "App-OS-Version" "10.2.1"
      , Http.header "App-Version" "6.4.0"
      , Http.header "User-Agent" "PixivIOSApp/6.0.9 (iOS 10.2.1; iPhone8,1)"
      ] ++ auth
  in
    Http.request
      { url = url
      , method = method
      , headers = headers
      , expect = Http.expectJson expect
      , body = body ?: Http.emptyBody
      , timeout = Nothing
      , withCredentials = False
      }

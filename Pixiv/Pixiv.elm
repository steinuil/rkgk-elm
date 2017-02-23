module Pixiv.Pixiv exposing (..)

import Pixiv.Types exposing (..)
import Pixiv.Decoders as Decoders
import Infix exposing (..)

import Http
import Platform.Cmd as Cmd
import Dict exposing (Dict)
import Json.Decode as Decode


type alias Response msg =
  Result Http.Error (Page, PageInfo) -> msg


-- FIXME: The endpoints urls now don't have the hostname included.


send : Response msg -> Request -> Cmd msg
send response request =
  let
    url =
      Dict.foldr (\k v -> (::) (k ++ "=" ++ v)) [] request.params
        |> String.join "&"
        |> (++) (request.url ++ "?")

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


more : (Result Http.Error Page -> msg) -> Maybe String -> Url -> Cmd msg
more response auth url =
  Http.send response
    <| httpRequest
      { url = url
      , method = "GET"
      , token = auth
      , expect = Decoders.request
      , body = Nothing
      }


login : (Result Http.Error LoginInfo -> msg) -> String -> String -> Cmd msg
login response name password =
  authRequest response
    [ Http.stringPart "grant_type" "password"
    , Http.stringPart "username" name
    , Http.stringPart "password" password
    ]


refresh : (Result Http.Error LoginInfo -> msg) -> String -> Cmd msg
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
      , url = "https://oauth.secure.pixiv.net/auth/token"
      , body = Just <| Http.multipartBody <|
        [ Http.stringPart "get_secure_url" "1"
        , Http.stringPart "client_id" "bYGKuGVw91e0NMfPGp44euvGt59s"
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

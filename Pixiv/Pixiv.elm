module Pixiv.Pixiv exposing (..)

import Pixiv.Types exposing (..)
import Pixiv.Decoders as Decoders

import Http
import Platform.Cmd as Cmd
import Dict exposing (Dict)
import Json.Decode as Decode


type alias Response msg =
  Result Http.Error (Page, PageInfo) -> msg


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

    request_ =
      Http.request
        { url = url
        , method = method
        , headers = appHeaders auth
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
  in
    Http.send response request_


appHeaders : Maybe String -> List Http.Header
appHeaders token =
  let
    auth = case token of
      Just t -> [ Http.header "Authorization" ("Bearer " ++ t) ]
      Nothing -> []
  in
    [ Http.header "App-OS" "ios"
    , Http.header "App-OS-Version" "10.2.1"
    , Http.header "App-Version" "6.4.0"
    , Http.header "User-Agent" "PixivIOSApp/6.0.9 (iOS 10.2.1; iPhone8,1)"
    ] ++ auth

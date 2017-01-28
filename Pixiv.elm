module Pixiv exposing (..)

import Infix exposing (..)

import Json.Decode exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (..)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Http


type alias Id = Int


type alias Tag = String


type alias Url = String


type alias Request =
  { method : Method
  , url : Url
  , params : Params
  }


type alias Token =
  { access : String
  , refresh : String
  }


type Method
  = Get
  | Post Http.Body


type alias Params = Dict String String


type alias User =
  { id : Id
  , name : String
  , handle : String
  , avatar : Url
  , following : Bool
  }


type alias Illust =
  { urls : List Url
  , count : Int
  , id : Id
  , title : String
  , user : User
  , tags : List Tag
  , date : String
  , caption : String
  , bookmarked : Bool
  , thumb : Url
  }



-------------------------------------------------------------------------------
-- Send

send response req =
  let
    encodeParams =
      Dict.foldr (\k v -> (::) (k ++ "=" ++ v)) []
      >> String.join "&"
      >> (++) "?"

    url = req.url ++ encodeParams req.params

    get url =
      Http.request
        { method = "GET"
        , headers = headers
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson illustListDecoder
        , timeout = Nothing
        , withCredentials = False
        }

    request =
      case req.method of
        Get ->
          get url

        Post body ->
          Http.post url body illustListDecoder
  in
    Http.send response request


more response url =
  let request =
    Http.get url illustListDecoder
  in
    Http.send response request


withOptions : List (String, String) -> Request -> Request
withOptions new req =
  let
    member ls = flip List.member ls

    bool = member [ "true", "false" ]

    positive =
      String.toInt >> Result.withDefault -1 >> (<=) 0

    validParams = 
      Dict.fromList
        [ "illust_id" => positive
        , "user_id" => positive
        , "offset" => positive
        , "word" => always True
        , "restrict" => member [ "public", "private" ]
        , "content_type" => member [ "illust", "manga" ]
        , "sort" => member [ "date_asc", "date_desc" ]
        , "search_target" => member [ "partial_match_for_tags", "exact_match_for_tags", "title_and_caption" ]
        , "duration" => member [ "within_last_day", "within_last_week", "within_last_month" ]
        , "seed_illust_ids" => always True
        , "include_total_comments" => bool
        , "include_ranking_labels" => bool
        , "include_ranking_illusts" => bool
        , "filter" => (==) "for_ios"
        , "mode" => member
          [ "day", "week", "month", "day_male", "day_female", "week_original", "week_rookie", "day_manga" ]
        ] 

    valid key =
      Dict.get key validParams |> withDefault (always False)

    join new old =
      Dict.foldr
        (\k v -> if valid k v then Dict.insert k v else identity)
        old
        (Dict.fromList new)
  in
    { req | params = join new req.params }


headers : List Http.Header
headers =
  [ Http.header "App-OS" "ios"
  , Http.header "App-OS-Version" "10.2.1"
  , Http.header "App-Version" "6.4.0"
  , Http.header "User-Agent" "PixivIOSApp/6.0.9 (iOS 10.2.1; iPhone8,1)"
  ]


withProxy : Url -> Request -> Request
withProxy proxy req =
  { req | url = proxy ++ req.url }


-------------------------------------------------------------------------------
-- API endpoints
search : String -> Request
search word =
  -- search_target = partial_match_for_tags | exact_match_for_tags | title_and_caption
  -- sort = date_desc | date_asc
  -- duration = within_last_day | within_last_week | within_last_month
  -- offset = Int
  let
    params = Dict.fromList
      [ "word" => word
      , "search_target" => "partial_match_for_tags"
      , "sort" => "date_desc"
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/search/illust" params


userIllusts : Id -> Request
userIllusts id =
  -- offset = Int
  let
    params = Dict.fromList
      [ "user_id" => (toString id)
      , "type" => "illust"
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/illusts" params


userBookmarks : Id -> Request
userBookmarks id =
  -- max_bookmark_id = ???
  -- tag = String
  let
    params = Dict.fromList
      [ "user_id" => (toString id)
      , "restrict" => "public"
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/bookmarks/illust" params


illust : Id -> Request
illust id =
  -- offset = Int
  let
    params = Dict.fromList
      [ "illust_id" => (toString id) ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/detail" params


illustComments : Id -> Request
illustComments id =
  -- offset = Int
  -- include_total_comments = Bool
  let
    params = Dict.fromList
      [ "illust_id" => (toString id) ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/comments" params


illustRelated : Id -> Request
illustRelated id =
  -- seed_illust_ids = String | List String
  let
    params = Dict.fromList
      [ "illust_id" => (toString id)
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/related" params


recommended : Request
recommended =
  -- offset = Int
  -- content_type = illust | manga
  -- include_ranking_label = Bool
  -- include_ranking_illusts = Bool
  let
    params = Dict.fromList
      [ "content_type" => "manga"
      , "include_ranking_label" => "true"
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/recommended-nologin" params


ranking : Request
ranking =
  let
    -- date = 'YYYY-MM-DD'
    -- offset = Int
    params = Dict.fromList
      [ "mode" => "day"
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/ranking" params


trendingTags : Request
trendingTags =
  let
    params = Dict.fromList [ "filter" => "for_ios" ]
  in
    Request Get "https://app-api.pixiv.net/v1/trending-tags/illust" params



-------------------------------------------------------------------------------
-- JSON responses decoders
userDecoder =
  map5 User
    (field "id" int)
    (field "name" string)
    (field "account" string)
    (field "profile_image_urls" <| field "medium" string)
    (field "is_followed" bool)


illustDecoder =
  let
    illust_ count single multi = case count of
      1 -> Illust [ single ?: "" ] count
      _ -> Illust (multi ?: []) count
  in
    decode illust_
      |> required "page_count" int
      |> required "meta_single_page"
        (maybe <| field "original_image_url" string)
      |> required "meta_pages"
        (maybe <| list <| field "image_urls" <| field "original" string)
      |> required "id" int
      |> required "title" string
      |> required "user" userDecoder
      |> required "tags" (list <| field "name" string)
      |> required "create_date" string
      |> required "caption" string
      |> required "is_bookmarked" bool
      |> required "image_urls" (field "square_medium" string)


illustListDecoder =
  map2 (,)
    (field "illusts" <| list illustDecoder)
    (maybe <| field "next_url" string)


{-
login response name password =
  let
    payload =
      [ "grant_type" => Encode.string "password"
      , "username" => Encode.string name
      , "password" => Encode.string password
      ]
  in
    Http.send response <| getAuth payload


refresh response token =
  let
    payload =
      [ "grant_type" => Encode.string "refresh_token"
      , "refresh_token" => Encode.string token
      ]
  in
    Http.send response <| getAuth payload


getAuth data =
  let
    decoder =
      field "response"
        (map2 Token
          (field "access_token" string)
          (field "refresh_token" string))

    payload =
      Encode.object <|
        [ "get_secure_url" => Encode.int 1
        , "client_id" => Encode.string "bYGKuGVw91e0NMfPGp44euvGt59s"
        , "client_secret" => Encode.string "HP3RmkgAmEGro0gn1x9ioawQE8WMfvLXDz3ZqxpK"
        ] ++ data
  in
    Http.request
      { method = "POST"
      , headers =
        [ Http.header "App-OS" "ios"
        , Http.header "App-OS-Version" "10.2.1"
        , Http.header "App-Version" "6.4.0"
        , Http.header "User-Agent" "PixivIOSApp/6.0.9 (iOS 10.2.1; iPhone8,1)"
        ]
      , url = "http://localhost:9292/" ++ "https://oauth.secure.pixiv.net/v1/auth/token"
      , body = Http.jsonBody payload
      , expect = Http.expectJson decoder
      , timeout = Nothing
      , withCredentials = False
      }
-}

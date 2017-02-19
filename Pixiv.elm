module Pixiv exposing
  ( UserId, IllustId, Tag, Url, Request, Token, User, Illust
  , login, refresh
  , send, more, withOptions, withProxy
  , search, userIllusts, userBookmarks, illustRelated
  , recommendedNoAuth, ranking
  )

{-| An Elm interface to the Pixiv App API.

# Types
@docs User, Illust, Token, Request

## Helper types
@docs UserId, IllustId, Tag, Url

# Logging in
To perform any authenticated requests, you will need to log in first.
@docs login, refresh

# Sending a request
@docs send, more

## Request modifiers
@docs withOptions, withProxy

# API endpoints
@docs search, userIllusts, userBookmarks, illustRelated, recommendedNoAuth, ranking
-}

import Infix exposing (..)

import Json.Decode exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (..)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Http


-------------------------------------------------------------------------------
-- Types

{-| A pixiv user. -}
type alias User =
  { id : UserId
  , name : String
  , handle : String
  , avatar : Url
  , following : Bool
  }


{-| An illustration or album. -}
type alias Illust =
  { urls : List Url
  , count : Int
  , id : IllustId
  , title : String
  , user : User
  , tags : List Tag
  , date : String
  , caption : String
  , bookmarked : Bool
  , thumb : Url
  }


{-| -}
type alias UserId = Int


{-| -}
type alias IllustId = Int


{-| -}
type alias Tag = String


{-| -}
type alias Url = String


{-| Tokens for accessing endpoints that require authentication. -}
type alias Token =
  { access : String
  , refresh : String
  }


{-| Represents a request. Might be useful to include in signatures.  -}
type alias Request =
  { method : Method
  , url : Url
  , params : Params
  }


type Method
  = Get
  | Post Http.Body


type alias Params = Dict String String



-------------------------------------------------------------------------------
-- Login

{-| Logs in with the given name and password.

    Pixiv.login Login "test" "swordfish"
-}
login : (Result Http.Error Token -> msg) -> String -> String -> Cmd msg
login response name password =
  let
    payload =
      [ Http.stringPart "grant_type" "password"
      , Http.stringPart "username" name
      , Http.stringPart "password" password
      ]
  in
    Http.send response <| getAuth payload


{-| Refreshes the tokens when the access token is expired.

    Pixiv.refresh Login tokens.refresh
-}
refresh : (Result Http.Error Token -> msg) -> String -> Cmd msg
refresh response refreshToken =
  let
    payload =
      [ Http.stringPart "grant_type" "refresh_token"
      , Http.stringPart "refresh_token" refreshToken
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
      [ Http.stringPart "get_secure_url" "1"
      , Http.stringPart "client_id" "bYGKuGVw91e0NMfPGp44euvGt59s"
      , Http.stringPart "client_secret" "HP3RmkgAmEGro0gn1x9ioawQE8WMfvLXDz3ZqxpK"
      ] ++ data
  in
    Http.request
      { method = "POST"
      , headers = appHeaders
      , url = "http://localhost:9292/" ++ "https://oauth.secure.pixiv.net/auth/token"
      , body = Http.multipartBody payload
      , expect = Http.expectJson decoder
      , timeout = Nothing
      , withCredentials = False
      }


-------------------------------------------------------------------------------
-- Send

{-| Turns a request into a Cmd.

    Pixiv.userIllusts 102267
      |> Pixiv.send Response
-}
send : (Result Http.Error (List Illust, Maybe Url) -> msg) -> Request -> Cmd msg
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
        , headers = appHeaders
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


{-| For list resources, Pixiv returns a "next url" to fetch the next page.
This function is to fetch that more easily (or fetch an arbitrary URL, if you
really want to).

    Pixiv.more currentPage.more

TODO: come up with a way to hide this and add a proxy.
Perhaps return a `Maybe Cmd msg` instead of a `Maybe Url`, or a
`Maybe Request`?
-}
more : (Result Http.Error (List Illust, Maybe Url) -> msg) -> Url -> Cmd msg
more response url =
  let request =
    Http.get url illustListDecoder
  in
    Http.send response request


{-| Modifier to change the options of a request.
Invalid options will be ignored.

    Pixiv.search "カンナカムイ"
      |> Pixiv.withOptions [ "search_target" => "exact_match_for_tags" ]
      |> Pixiv.send Response
-}
withOptions : List (String, String) -> Request -> Request
withOptions new req =
  let
    member = flip List.member

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


{-| Since Pixiv doesn't allow AJAX calls from another page, you may want to use
this to define a proxy.

    Pixiv.search "カンナカムイ"
      |> Pixiv.withProxy "https://example.com"
      |> Pixiv.send Response
-}
withProxy : Url -> Request -> Request
withProxy proxy req =
  { req | url = proxy ++ req.url }


appHeaders : List Http.Header
appHeaders =
  [ Http.header "App-OS" "ios"
  , Http.header "App-OS-Version" "10.2.1"
  , Http.header "App-Version" "6.4.0"
  , Http.header "User-Agent" "PixivIOSApp/6.0.9 (iOS 10.2.1; iPhone8,1)"
  ]


-------------------------------------------------------------------------------
-- API endpoints

{-| Search for a tag, or keyword.

Valid options:

    search_target, duration, sort, offset
-}
search : String -> Request
search word =
  let
    params = Dict.fromList
      [ "word" => word
      , "search_target" => "partial_match_for_tags"
      , "sort" => "date_desc"
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/search/illust" params


{-| Selected user's illustrations.

Valid options:

    type, offset
-}
userIllusts : UserId -> Request
userIllusts id =
  let
    params = Dict.fromList
      [ "user_id" => (toString id)
      , "type" => "illust"
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/illusts" params


{-| Selected user's bookmarks.

Valid options:

    max_bookmark_id : ??, tag : String, restrict
-}
userBookmarks : UserId -> Request
userBookmarks id =
  let
    params = Dict.fromList
      [ "user_id" => (toString id)
      , "restrict" => "public"
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/bookmarks/illust" params


{-| Illustrations related to the selected one.

Valid options:

    seed_illust_ids : (String | List String)
-}
illustRelated : IllustId -> Request
illustRelated id =
  let
    params = Dict.fromList
      [ "illust_id" => (toString id)
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/related" params


{-| Recommended illustrations based on the list of illustrations provided.
-}
recommendedNoAuth : List IllustId -> Request
recommendedNoAuth ids =
  -- offset = Int
  -- content_type = illust | manga
  -- include_ranking_label = Bool
  -- include_ranking_illusts = Bool
  let
    params = Dict.fromList
      [ "content_type" => "illust"
      , "include_ranking_label" => "true"
      , "filter" => "for_ios"
      , "bookmark_illust_ids" => String.join "," (List.map toString ids)
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/recommended-nologin" params


{-| Ranking illustrations.
-}
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


-------------------------------------------------------------------------------
-- Requests that expect other things

{-| Detail
-}
illustDetail : IllustId -> Request
illustDetail id =
  -- offset = Int
  let
    params = Dict.fromList
      [ "illust_id" => (toString id) ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/detail" params

-- FIXME expect a User
userDetail : UserId -> Request
userDetail id =
  let
    params = Dict.fromList
      [ "user_id" => toString id
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/detail" params


-- FIXME expect a comment list
-- "include_total_comments" => bool
-- offset
illustComments : IllustId -> Request
illustComments id =
  let
    params = Dict.fromList
      [ "illust_id" => toString id
      , "include_total_comments" => "true"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/detail" params


-- FIXME Dunno lol
trendingTags : Request
trendingTags =
  let
    params = Dict.fromList [ "filter" => "for_ios" ]
  in
    Request Get "https://app-api.pixiv.net/v1/trending-tags/illust" params


-- People who bookmarked a work?
illustBookmarkDetail : IllustId -> Request
illustBookmarkDetail id =
  let
    params = Dict.fromList [ "illust_id" => toString id ]
  in
    Request Get "https://app-api.pixiv.net/v2/illust/bookmark/detail" params


ugoiraDetail : IllustId -> Request
ugoiraDetail id =
  let
    params = Dict.fromList [ "illust_id" => toString id ]
  in
    Request Get "https://app-api.pixiv.net/v1/ugoira/metadata" params


-------------------------------------------------------------------------------
-- Authenticated requests

-- "restrict" => member [ "public", "private" ]
-- offset
illustFollowing : Request
illustFollowing =
  let
    params = Dict.fromList [ "restrict" => "public" ]
  in
    Request Get "https://app-api.pixiv.net/v2/illust/follow" params


-- "content_type" => member [ "illust", "manga" ]
-- "include_ranking_label" => bool
-- offset
-- "max_bookmark_id_for_recommend" ??
-- "min_bookmark_id_for_recent_illust" ??
recommended : Request
recommended =
  let
    params = Dict.fromList
      [ "content_type" => "illust"
      , "include_ranking_label" => "false"
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/recommended" params


-- "tags" => always True (space-separated)
addBookmark : IllustId -> Request
addBookmark id =
  let
    params = Dict.fromList
      [ "illust_id" => toString id
      , "restrict" => "public"
      ]
  in
    Request (Post Http.emptyBody) "https://app-api.pixiv.net/v1/illust/bookmark/add" params


deleteBookmark : IllustId -> Request
deleteBookmark id =
  let
    params = Dict.fromList [ "illust_id" => toString id ]
  in
    Request (Post Http.emptyBody) "https://app-api.pixiv.net/v1/illust/bookmark/delete" params


-- What the fuck is this anyway?
-- offset
bookmarkTagsList : Request
bookmarkTagsList =
  let
    params = Dict.fromList [ "restrict" => "public" ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/bookmark-tags/illust" params


-- offset
following : UserId -> Request
following id =
  let
    params = Dict.fromList
      [ "user_id" => toString id
      , "restrict" => "public"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/following" params


-- offset
followers : UserId -> Request
followers id =
  let
    params = Dict.fromList
      [ "user_id" => toString id
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/follower" params


blacklist : UserId -> Request
blacklist id =
  let
    params = Dict.fromList
      [ "user_id" => toString id
      , "filter" => "for_ios"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/list" params


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
      1 -> Illust [single ?: "" ] count
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

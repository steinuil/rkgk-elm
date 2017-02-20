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
import Pixiv.Params as Params

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
  , params : Dict String String
  , allowed : List String
  }


type Method
  = Get
  | Post Http.Body



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
    -- FIXME
    --encodeParams =
    --  List.map (\p -> let (k, v) = Params.toQuery p in k ++ "=" ++ v)
    --    >> String.join "&"
    --    >> (++) "?"

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
      |> Pixiv.withOptions [ Pixiv.Params.offset 2 ]
      |> Pixiv.send Response
-}
withOptions : List (String, String) -> Request -> Request
withOptions new req =
  let
    join new old =
      Dict.foldr
        (\k v -> if List.member k req.allowed
                 then Dict.insert k v
                 else identity)
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
-}
search : String -> Request
search word =
  let
    params = Dict.fromList
      [ Params.word word
      , Params.searchTarget Params.PartialMatchForTags
      , Params.sort Params.DateDesc
      , Params.filterForIos
      ]

    allowed = [ "search_target", "duration", "sort", "offset" ]
  in
    Request Get "https://app-api.pixiv.net/v1/search/illust" params allowed


{-| Selected user's illustrations.
-}
userIllusts : UserId -> Request
userIllusts id =
  let
    params = Dict.fromList
      [ Params.userId id
      , Params.type_ Params.Illust
      , Params.filterForIos
      ]

    allowed = [ "type", "offset" ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/illusts" params allowed


{-| Selected user's bookmarks.  -}
userBookmarks : UserId -> Request
userBookmarks id =
  let
    params = Dict.fromList
      [ Params.userId id
      , Params.filterForIos
      ]

    allowed = [ "max_bookmark_id", "tag", "restrict" ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/bookmarks/illust" params allowed


{-| Illustrations related to the selected one.

Valid options:

    seed_illust_ids : (String | List String)
-}
illustRelated : IllustId -> Request
illustRelated id =
  let
    params = Dict.fromList
      [ Params.illustId id
      , Params.filterForIos
      ]

    allowed = [ "seed_illust_ids" ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/related" params allowed


{-| Recommended illustrations based on the list of illustrations provided.
-}
recommendedNoAuth : List IllustId -> Request
recommendedNoAuth ids =
  let
    params = Dict.fromList
      [ Params.contentType Params.Illust
      , Params.includeRankingLabels False
      , Params.bookmarkIllustIds ids
      , Params.filterForIos
      ]

    allowed =
      [ "content_type", "include_ranking_label", "include_ranking_illusts"
      , "bookmark_illust_ids", "offset"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/recommended-nologin" params allowed


{-| Ranking illustrations.
-}
ranking : Request
ranking =
  let
    params = Dict.fromList
      [ Params.mode Params.Day
      , Params.filterForIos
      ]
    
    allowed = [ "date", "offset" ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/ranking" params allowed


-------------------------------------------------------------------------------
-- Requests that expect other things

{-| Detail
-}
illustDetail : IllustId -> Request
illustDetail id =
  let
    params = Dict.fromList [ Params.illustId id ]
    
    allowed = [ "offset" ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/detail" params allowed

-- FIXME expect a User
userDetail : UserId -> Request
userDetail id =
  let
    params = Dict.fromList
      [ Params.userId id
      , Params.filterForIos
      ]

    allowed = []
  in
    Request Get "https://app-api.pixiv.net/v1/user/detail" params allowed


-- FIXME expect a comment list
-- "include_total_comments" => bool
-- offset
illustComments : IllustId -> Request
illustComments id =
  let
    params = Dict.fromList
      [ Params.illustId id
      , Params.includeTotalComments True
      ]

    allowed = [ "include_total_comments" ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/detail" params allowed


-- FIXME Dunno lol
trendingTags : Request
trendingTags =
  let
    params = Dict.fromList [ Params.filterForIos ]

    allowed = []
  in
    Request Get "https://app-api.pixiv.net/v1/trending-tags/illust" params allowed


-- People who bookmarked a work?
illustBookmarkDetail : IllustId -> Request
illustBookmarkDetail id =
  let
    params = Dict.fromList [ Params.illustId id ]

    allowed = []
  in
    Request Get "https://app-api.pixiv.net/v2/illust/bookmark/detail" params allowed


ugoiraDetail : IllustId -> Request
ugoiraDetail id =
  let
    params = Dict.fromList [ Params.illustId id ]

    allowed = []
  in
    Request Get "https://app-api.pixiv.net/v1/ugoira/metadata" params allowed


-------------------------------------------------------------------------------
-- Authenticated requests

-- "restrict" => member [ "public", "private" ]
-- offset
illustFollowing : Request
illustFollowing =
  let
    params = Dict.fromList []

    allowed = [ "restrict", "offset" ]
  in
    Request Get "https://app-api.pixiv.net/v2/illust/follow" params allowed


recommended : Request
recommended =
  let
    params = Dict.fromList
      [ Params.contentType Params.Illust
      , Params.filterForIos
      ]

    allowed =
      [ "content_type", "include_ranking_label", "max_bookmark_id_for_recommend"
      , "min_bookmark_id_for_recent_illust", "offset"
      ]
  in
    Request Get "https://app-api.pixiv.net/v1/illust/recommended" params allowed


-- "tags" => always True (space-separated)
addBookmark : IllustId -> Request
addBookmark id =
  let
    params = Dict.fromList [ Params.illustId id ]

    allowed = [ "tags", "retrict" ]
  in
    Request (Post Http.emptyBody) "https://app-api.pixiv.net/v1/illust/bookmark/add" params allowed


deleteBookmark : IllustId -> Request
deleteBookmark id =
  let
    params = Dict.fromList [ Params.illustId id ]

    allowed = []
  in
    Request (Post Http.emptyBody) "https://app-api.pixiv.net/v1/illust/bookmark/delete" params allowed


-- What the fuck is this anyway?
-- offset
bookmarkTagsList : Request
bookmarkTagsList =
  let
    params = Dict.fromList [ "restrict" => "public" ]

    allowed = [ "restrict", "offset" ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/bookmark-tags/illust" params allowed


-- offset
following : UserId -> Request
following id =
  let
    params = Dict.fromList [ Params.userId id ]

    allowed = [ "restrict", "offset" ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/following" params allowed


-- offset
followers : UserId -> Request
followers id =
  let
    params = Dict.fromList
      [ Params.userId id
      , Params.filterForIos
      ]

    allowed = [ "offset" ]
  in
    Request Get "https://app-api.pixiv.net/v1/user/follower" params allowed


blacklist : UserId -> Request
blacklist id =
  let
    params = Dict.fromList
      [ Params.userId id
      , Params.filterForIos
      ]

    allowed = []
  in
    Request Get "https://app-api.pixiv.net/v1/user/list" params allowed


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

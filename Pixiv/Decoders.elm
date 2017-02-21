module Pixiv.Decoders exposing (Page, request, LoginInfo, login)

{-| Raw decoders and types for the Pixiv API.

# Page decoders
@docs Page, request

# Login info decoders
@docs LoginInfo, login
-}

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Date exposing (Date)

import Pixiv.Types exposing (..)

-------------------------------------------------------------------------------
-- Utilities

(:=) = field


date : String -> Date
date x =
  case Date.fromString x of
    Ok d  -> d
    Err _ -> Date.fromTime 1487640612


emptyToNothing : String -> Maybe String
emptyToNothing str =
  case str of
    "" -> Nothing
    _ -> Just str

maybeString str =
  custom (map emptyToNothing (str := string))


-------------------------------------------------------------------------------
-- Toplevel decoders

{-| Various types of page. -}
type Page =
    IllustList (List Illust) (Maybe Url)
  | CommentList (List Comment) (Maybe Url)
  | UserPreviews (List (User, List Illust)) (Maybe Url)
  | TrendingTags (List (Tag, Illust))
  | IllustDetail Illust
  | UserDetail ExtendedUser
  | UgoiraData Ugoira


{-| A drop-in decoder that handles (almost) all results of a request. -}
request : Decoder Page
request =
  oneOf
    [ map2 IllustList
      ("illusts" := (list illust))
      ("next_url" := (nullable string))

    , map2 CommentList
      ("comments" := (list comment))
      ("next_url" := (nullable string))

    , map2 UserPreviews
      (list <| map2 (,)
        ("user" := user)
        ("illusts" := (list illust)))
      ("next_url" := (nullable string))

    , map TrendingTags
      (list <| map2 (,)
        ("tag" := string)
        ("illust" := illust))

    , map IllustDetail ("illust" := illust)

    , map UgoiraData (field "ugoira_metadata" ugoira)
    ]


{-| Info about the login session and user. -}
type alias LoginInfo =
  { accessToken : String
  , refreshToken : String
  , user :
    { id : UserId
    , name : String
    , account : String
    , avatar : Url
    }
  }


type alias LoggedUser =
  { id : UserId
  , name : String
  , account : String
  , avatar : Url
  }


{-| Decoder for the login response. -}
login : Decoder LoginInfo
login =
  field "response"
    (map3 LoginInfo
      ("access_token" := string)
      ("refresh_token" := string)
      (field "user"
        (map4 LoggedUser
          ("id" := int)
          ("name" := string)
          ("account" := string)
          (at [ "profile_image_urls", "px_170x170" ] string))))


-------------------------------------------------------------------------------
-- Subdecoders

illust : Decoder Illust
illust =
  let 
    -- If there's a single picture, try to extract the url and stick it in a list.
    singleUrl =
      map (\x -> [x]) <| at [ "meta_single_page", "original_image_url" ] string

    -- Otherwise, extract the list of urls.
    multipleUrls =
      field "meta_pages" <| list <| at [ "image_urls", "original" ] string
  in
    decode Illust
      |> required "id" int
      |> required "title" string
      |> required "user" user
      |> required "caption" string
      |> custom (map date ("create_date" := string))
      |> required "tags" (list <| field "name" string)
      |> required "tools" (list string)
      |> required "page_count" int
      |> requiredAt [ "image_urls", "square_medium" ] string
      |> custom (oneOf [ singleUrl, multipleUrls ])
      |> required "width" int
      |> required "height" int
      |> required "total_views" int
      |> required "total_bookmarks" int
      |> required "total_comments" int
      |> required "is_bookmarked" bool
      |> required "is_muted" bool
      |> required "restrict" int
      |> required "sanity_level" int
      |> required "visible" bool


user : Decoder User
user =
  map6 User
    ("id" := int)
    ("name" := string)
    ("account" := string)
    (at [ "profile_image_urls", "medium" ] string)
    (maybe ("comment" := string))
    (oneOf [ field "is_followed" bool, succeed False ])


comment : Decoder Comment
comment =
  map4 Comment
    ("id" := int)
    ("comment" := string)
    (map date ("date" := string))
    ("user" := user)


ugoira : Decoder Ugoira
ugoira =
  map2 Ugoira
    (at [ "zip_urls", "medium" ] string)
    (field "frames" <| list
      (map2 (,)
        ("file" := string)
        ("delay" := int)))


userDetail : Decoder ExtendedUser
userDetail =
  map3 ExtendedUser
    ("user" := user)
    (field "profile"
      (decode UserInfo
        |> required "webpage" (nullable string)
        |> maybeString "gender"
        |> custom
          (map (emptyToNothing >> Maybe.map date) ("birth" := string))
        |> maybeString "region"
        |> maybeString "job"
        |> required "total_follow_users" int
        |> required "total_follower" int
        |> required "total_mypixiv_users" int
        |> required "total_illusts" int
        |> required "total_manga" int
        |> required "total_novels" int
        |> required "total_illust_bookmarks_public" int
        |> required "background_image_url" (nullable string)
        |> maybeString "twitter_account"
        |> required "twitter_url" (nullable string)
        |> required "is_premium" bool))
    (field "workspace"
      (decode Workspace
        |> maybeString "pc"
        |> maybeString "monitor"
        |> maybeString "tool"
        |> maybeString "scanner"
        |> maybeString "tablet"
        |> maybeString "mouse"
        |> maybeString "printer"
        |> maybeString "desktop"
        |> maybeString "music"
        |> maybeString "desk"
        |> maybeString "chair"
        |> maybeString "comment"
        |> required "workspace_image_url" (nullable string)))

module Pixiv.Decoders exposing
  (Tag, Url, Page(..), Illust, User, Comment, Ugoira
  , Workspace, UserInfo, ExtendedUser
  , request
  )

{-| Raw decoders and types for the Pixiv API.

# Page decoder
@docs Page, request

# Base types
@docs Illust, User, ExtendedUser, Comment, Ugoira, UserInfo, Workspace

# Helper types
@docs Tag, Url
-}

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Date exposing (Date)

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


{-| -}
type alias Tag = String
{-| -}
type alias Url = String

-------------------------------------------------------------------------------
-- Toplevel decoder

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


-------------------------------------------------------------------------------
-- Subdecoders

{-| An illustration.

I'm not sure what restrict and sanityLevel are, but I'll keep them in there
because the API provides them.
-}
type alias Illust =
  { id : Int
  , title : String
  , user : User
  , caption : String
  , date : Date
  , tags : List Tag
  , tools : List String
  , count : Int
  , thumbnail : Url
  , urls : List Url
  , width : Int
  , height : Int
  , views : Int
  , bookmarksNo : Int
  , commentsNo : Int
  , bookmarked : Bool
  , muted : Bool
  , restrict : Int
  , sanityLevel : Int
  , visible : Bool
  }


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


{-| A simple user.

Some resources omit the bio and the following fields,
we're just gonna assume following is False if it doesn't show up.

The only page that omits the following is the comments section anyway.
-}
type alias User =
  { id : Int
  , name : String
  , nick : String
  , avatar : Url
  , bio : Maybe String
  , following : Bool
  }


user : Decoder User
user =
  map6 User
    ("id" := int)
    ("name" := string)
    ("account" := string)
    (at [ "profile_image_urls", "medium" ] string)
    (maybe ("comment" := string))
    (oneOf [ field "is_followed" bool, succeed False ])


{-| A comment.

This is the only resource that omits the following field in the user.
-}
type alias Comment =
  { id : Int
  , body : String
  , date : Date
  , user : User
  }


comment : Decoder Comment
comment =
  map4 Comment
    ("id" := int)
    ("comment" := string)
    (map date ("date" := string))
    ("user" := user)


{-| An ugoira metadata. Go ahead if you're crazy enough to implement this. -}
type alias Ugoira =
  { zip : String, frames : List (String, Int) }


ugoira : Decoder Ugoira
ugoira =
  map2 Ugoira
    (at [ "zip_urls", "medium" ] string)
    (field "frames" <| list
      (map2 (,)
        ("file" := string)
        ("delay" := int)))


{-| Info about a user's workspace. By the way, this can be completely empty. -}
type alias Workspace =
  { pc : Maybe String
  , monitor : Maybe String
  , tool : Maybe String
  , scanner : Maybe String
  , tablet : Maybe String
  , mouse : Maybe String
  , printer : Maybe String
  , desktop : Maybe String
  , music : Maybe String
  , desk : Maybe String
  , chair : Maybe String
  , comment : Maybe String
  , picture : Maybe Url
  }


{-| Additional info about a user. -}
type alias UserInfo =
  { webpage : Maybe String
  , gender : Maybe String
  , birth : Maybe Date
  , region : Maybe String
  , job : Maybe String
  , followsNo : Int
  , followersNo : Int
  , myPixivNo : Int
  , illustNo : Int
  , mangaNo : Int
  , novelNo : Int
  , bookmarkNo : Int
  , background : Maybe Url
  , twitterName : Maybe String
  , twitterUrl : Maybe Url
  , premium : Bool
  }


{-| Info about a user that you get from a user detail page. -}
type alias ExtendedUser =
  { user : User
  , info : UserInfo
  , workspace : Workspace
  }


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

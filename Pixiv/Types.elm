module Pixiv.Types exposing (..)

{-| Base types for working with the Pixiv API.
Meant to be fully exposed.

# Helper types
@docs Tag, Url, UserId, IllustId

# Base types
@docs Illust, User, ExtendedUser, Comment, Ugoira, UserInfo, Workspace

# Others
@docs Request, Page, PageInfo, LoginInfo, Tokens, Method
-}

import Date exposing (Date)
import Dict exposing (Dict)


{-| -}
type alias Tag = String
{-| -}
type alias Url = String
{-| -}
type alias UserId = Int
{-| -}
type alias IllustId = Int


-------------------------------------------------------------------------------
-- Base types

{-| An illustration.

I'm not sure what restrict and sanityLevel are, but I'll keep them in there
because the API provides them.
-}
type alias Illust =
  { id : IllustId
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


{-| A simple user.

Some resources omit the bio and the following fields,
we're just gonna assume following is False if it doesn't show up.

The only page that omits the following is the comments section anyway.
-}
type alias User =
  { id : UserId
  , name : String
  , nick : String
  , avatar : Url
  , bio : Maybe String
  , following : Bool
  }


{-| A comment.

This is the only resource that omits the following field in the user.
-}
type alias Comment =
  { id : Int
  , body : String
  , date : Date
  , user : User
  }


{-| An ugoira metadata. Go ahead if you're crazy enough to implement this. -}
type alias Ugoira =
  { zip : String, frames : List (String, Int) }


{-| Info about a user that you get from a user detail page. -}
type alias ExtendedUser =
  { user : User
  , info : UserInfo
  , workspace : Workspace
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


{-| A request, containing the method and the access token if needed, the url,
the default parameters, the allowed parameters and some additional info to
return.
-}
type alias Request =
  { method : Method
  , url : Url
  , return : PageInfo
  , allowed : List String
  , params : Dict String String
  }


{-| Various types of page. -}
type Page =
    IllustList (List Illust) (Maybe Url)
  | CommentList (List Comment) (Maybe Url)
  | UserPreviews (List (User, List Illust)) (Maybe Url)
  | TrendingTags (List (Tag, Illust))
  | IllustDetail Illust
  | UserDetail ExtendedUser
  | UgoiraData Ugoira


{-| Types of page info. -}
type PageInfo =
    BasePage String
  | UserPage String User
  | IllustPage String Illust


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


{-| Just the tokens for when you only need those. -}
type alias Tokens =
  { accessToken : String
  , refreshToken : String
  }


{-| Types of API requests, Get and Post require the access token. -}
type Method = GetNoAuth | Get String | Post String

module Pixiv.Endpoints exposing
  ( search, ranking, recommended, userIllusts, userBookmarks, related
  , popularPreview
  , illust, user, ugoiraData
  , myFeed, myRecommended, myBookmarkTags, illustBookmarkDetail
  , bookmark, unbookmark, follow, unfollow
  )

{-| Endpoints

# Unauthenticated
@docs search, ranking, recommended, userIllusts, userBookmarks, related
@docs popularPreview

## Single resources
@docs illust, user, ugoiraData

# Authenticated
@docs myFeed, myRecommended, myBookmarkTags, illustBookmarkDetail
@docs bookmark, unbookmark, follow, unfollow
-}

import Pixiv.Types exposing (..)
import Pixiv.Params as Params exposing
  ( RestrictOpts(..), ContentTypeOpts(..), SortOpts(..), SearchTargetOpts(..)
  , DurationOpts(..), ModeOpts(..)
  )
import Pixiv.Decoders as Decoders

import Dict


type alias AccessToken = String

{-
Objectives: eliminate Pixiv.Params because it's fucking ugly.

Stuff to infer: following and unfollowing users.
-}


{-| Search for a tag, or keyword. -}
search : String -> Request
search word =
  { method = GET
  , auth = Nothing
  , url = "v1/search/illust"
  , return = SearchPage word
  , allowed = [ "search_target", "duration", "sort", "offset" ]
  , params = Dict.fromList
    [ Params.word word
    , Params.searchTarget PartialMatchForTags
    , Params.sort DateDesc
    ]
  }


{-| List of popular illustrations. -}
ranking : Request
ranking =
  { method = GET
  , auth = Nothing
  , url = "v1/illust/ranking"
  , return = BasePage "Ranking"
  , allowed = [ "date", "offset" ]
  , params = Dict.fromList []
  }


{-| Recommended illustrations. Can be based on a list of bookmarks provided. -}
recommended : Request
recommended =
  { method = GET
  , auth = Nothing
  , url = "v1/illust/recommended-nologin"
  , return = BasePage "Recommended"
  , allowed = [ "content_type", "bookmark_illust_ids", "offset" ]
  , params = Dict.fromList []
  }


{-| An user's illustrations. -}
userIllusts : User -> Request
userIllusts user =
  { method = GET
  , auth = Nothing
  , url = "v1/user/illusts"
  , return = UserPage "Illustrations" user
  , allowed = [ "type", "offset" ]
  , params = Dict.fromList
    [ Params.userId user.id
    , Params.type_ IllustCont
    ]
  }


{-| An user's bookmarks. -}
userBookmarks : User -> Request
userBookmarks user =
  { method = GET
  , auth = Nothing
  , url = "v1/user/bookmarks/illust"
  , return = UserPage "Bookmarks" user
  , allowed = [ "max_bookmark_id", "tag" ]
  , params = Dict.fromList
    [ Params.userId user.id
    , Params.restrict Public
    ]
  }


{-| Random illustrations related to the selected one. -}
related : Illust -> Request
related illust =
  { method = GET
  , auth = Nothing
  , url = "v2/illust/related"
  , return = IllustPage "Related" illust
  , allowed = [ "seed_illust_id" ]
  , params = Dict.fromList [ Params.illustId illust.id ]
  }


{-| List of the trending tags with an Illust for each. -}
trendingTags : Request
trendingTags =
  { method = GET
  , auth = Nothing
  , url = "v1/trending-tags/illust"
  , return = BasePage "Trending Tags"
  , allowed = []
  , params = Dict.fromList []
  }


{-| List of the users followed by the given user with a few preview illusts. -}
following : User -> Request
following user =
  { method = GET
  , auth = Nothing
  , url = "v1/user/following"
  , return = UserPage "Following" user
  , allowed = [ "offset" ]
  , params = Dict.fromList [ Params.userId user.id ]
  }


{-| Same as above, for the followers of the given user. -}
followers : User -> Request
followers user =
  { method = GET
  , auth = Nothing
  , url = "v1/user/follower"
  , return = UserPage "Followers" user
  , allowed = [ "offset" ]
  , params = Dict.fromList [ Params.userId user.id ]
  }


{-| An illustration's comments. -}
comments : Illust -> Request
comments illust =
  { method = GET
  , auth = Nothing
  , url = "v1/illust/comments"
  , return = IllustPage "Comments" illust
  , allowed = [ "include_total_comments", "offset" ]
  , params = Dict.fromList [ Params.illustId illust.id ]
  }


{-| A preview of a few popular works for that query string. -}
popularPreview : String -> Request
popularPreview query =
  { method = GET
  , auth = Nothing
  , url = "v1/search/popular-preview/illust"
  , return = BasePage ("Popular: " ++ query)
  , allowed = [ "search_target", "duration", "sort", "offset" ]
  , params = Dict.fromList
    [ Params.word query
    , Params.searchTarget PartialMatchForTags
    , Params.sort DateDesc
    ]
  }


-------------------------------------------------------------------------------

{-| Information about a single illustration.

You get exactly the same metadata as in a search or similar stuff.
Useful when taking input from an URL.
-}
illust : IllustId -> Request
illust id =
  { method = GET
  , auth = Nothing
  , url = "v1/illust/detail"
  , return = BasePage "Illust"
  , allowed = []
  , params = Dict.fromList [ Params.illustId id ]
  }


{-| Additional information about a user. -}
user : UserId -> Request
user id =
  { method = GET
  , auth = Nothing
  , url = "v1/user/detail"
  , return = BasePage "User"
  , allowed = []
  , params = Dict.fromList [ Params.userId id ]
  }


{-| Information necessary to play an Ugoira.

Basically, it's a zip file with all the frames and a list of the frames in order
with the duration for each of them. Like fuck I'm supporting Ugoira.
-}
ugoiraData : Illust -> Request
ugoiraData illust =
  { method = GET
  , auth = Nothing
  , url = "v1/ugoira/metadata"
  , return = IllustPage "Ugoira" illust
  , allowed = []
  , params = Dict.fromList [ Params.illustId illust.id ]
  }


-------------------------------------------------------------------------------

{-| New works from the users you follow. -}
myFeed : AccessToken -> Request
myFeed token =
  { method = GET
  , auth = Just token
  , url = "v2/illust/follow"
  , return = BasePage "New Works - Following"
  , allowed = [ "restrict", "offset" ]
  , params = Dict.fromList
    [ Params.restrict Public ]
  }


{-| Recommended illustrations based on your bookmarks. -}
myRecommended : AccessToken -> Request
myRecommended token =
  { method = GET
  , auth = Just token
  , url = "v1/illust/recommended"
  , return = BasePage "Recommended"
  , allowed =
    [ "content_type", "include_ranking_label", "max_bookmark_id_for_recommend"
    , "min_bookmark_id_for_recent_illust", "offset"
    ]
  , params = Dict.fromList [ Params.contentType Params.IllustCont ]
  }


-- FIXME write a decoder for this
{-| Tags you've used to sort your bookmarks -}
myBookmarkTags : AccessToken -> Request
myBookmarkTags token =
  { method = GET
  , auth = Just token
  , url = "v1/user/bookmark-tags/illust"
  , return = BasePage "Bookmark Tags"
  , allowed = [ "restrict", "offset" ]
  , params = Dict.fromList [ Params.restrict Public ]
  }


-- FIXME what
{-| No idea -}
illustBookmarkDetail : AccessToken -> Illust -> Request
illustBookmarkDetail token illust =
  { method = GET
  , auth = Just token
  , url = "v2/illust/bookmark/detail"
  , return = IllustPage "Bookmark Detail" illust
  , allowed = []
  , params = Dict.fromList [ Params.illustId illust.id ]
  }


-------------------------------------------------------------------------------

{-| Stuff -}
bookmark : AccessToken -> Illust -> Request
bookmark token illust =
  { method = POST
  , auth = Just token
  , url = "v2/illust/bookmark/add"
  , return = ActionResult
  , allowed = [ "tags", "restrict" ]
  , params = Dict.fromList
    [ Params.illustId illust.id
    , Params.restrict Public
    ]
  }


{-| Stuff -}
unbookmark : AccessToken -> Illust -> Request
unbookmark token illust =
  { method = POST
  , auth = Just token
  , url = "v1/illust/bookmark/delete"
  , return = ActionResult
  , allowed = []
  , params = Dict.fromList
    [ Params.illustId illust.id ]
  }


{-| Stuff -}
follow : AccessToken -> User -> Request
follow token user =
  { method = POST
  , auth = Just token
  , url = "v1/user/follow/add"
  , return = ActionResult
  , allowed = []
  , params = Dict.fromList
    [ Params.userId user.id
    , Params.restrict Public
    ]
  }


{-| Stuff -}
unfollow : AccessToken -> User -> Request
unfollow token user =
  { method = POST
  , auth = Just token
  , url = "v1/user/follow/delete"
  , return = ActionResult
  , allowed = []
  , params = Dict.fromList [ Params.userId user.id ]
  }

module Pixiv.Endpoints exposing (..)

import Pixiv.Types exposing (..)
import Pixiv.Params as Params exposing
  ( RestrictOpts(..), ContentTypeOpts(..), SortOpts(..), SearchTargetOpts(..)
  , DurationOpts(..), ModeOpts(..)
  )
import Pixiv.Decoders as Decoders

import Dict

{-
Objectives: eliminate Pixiv.Params because it's fucking ugly.
-}


{-| Search for a tag, or keyword. -}
search : String -> Request
search word =
  { method = GetNoAuth
  , url = "/v1/search/illust" 
  , return = BasePage ("Search: " ++ word)
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
  { method = GetNoAuth
  , url = "/v1/illust/ranking"
  , return = BasePage "Ranking"
  , allowed = [ "date", "offset" ]
  , params = Dict.fromList []
  }


{-| Recommended illustrations. Can be based on a list of bookmarks provided. -}
recommendedNoAuth : Request
recommendedNoAuth =
  { method = GetNoAuth
  , url = "/v1/illust/recommended-nologin" 
  , return = BasePage "Recommended"
  , allowed = [ "content_type", "bookmark_illust_ids", "offset" ]
  , params = Dict.fromList []
  }


{-| An user's illustrations. -}
userIllusts : User -> Request
userIllusts user =
  { method = GetNoAuth
  , url = "/v1/user/illusts"
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
  { method = GetNoAuth
  , url = "/v1/user/bookmarks/illust" 
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
  { method = GetNoAuth
  , url = "/v1/illust/related" 
  , return = IllustPage "Related" illust
  , allowed = [ "seed_illust_id" ]
  , params = Dict.fromList [ Params.illustId illust.id ]
  }




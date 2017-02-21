import Pixiv.Types exposing (..)
import Pixiv.Params as Params exposing
  ( RestrictOpts(..), ContentTypeOpts(..), SortOpts(..), SearchTargetOpts(..)
  , DurationOpts(..), ModeOpts(..)
  )
import Pixiv.Decoders as Decoders


{-
Objectives: eliminate Pixiv.Params because it's fucking ugly.
-}

type alias Request =
  { method : Method
  , url : Url
  , return : PageInfo
  , allowed : List String
  , default : List (String, String)
  }


type PageInfo =
    BasePage String
  | UserPage String User
  | IllustPage String Illust


type Method = GetNoAuth | Post | Get


{-| Search for a tag, or keyword. -}
search : String -> Request
search word =
  { method = GetNoAuth
  , url = "/v1/search/illust" 
  , return = BasePage ("Search: " ++ word)
  , allowed = [ "search_target", "duration", "sort", "offset" ]
  , default =
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
  , default = []
  }


{-| Recommended illustrations. Can be based on a list of bookmarks provided. -}
recommendedNoAuth : Request
recommendedNoAuth =
  { method = GetNoAuth
  , url = "/v1/illust/recommended-nologin" 
  , return = BasePage "Recommended"
  , allowed = [ "content_type", "bookmark_illust_ids", "offset" ]
  , default = []
  }


{-| An user's illustrations. -}
userIllusts : User -> Request
userIllusts user =
  { method = GetNoAuth
  , url = "/v1/user/illusts"
  , return = UserPage "Illustrations" user
  , allowed = [ "type", "offset" ]
  , default =
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
  , default =
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
  , default = [ Params.illustId illust.id ]
  }




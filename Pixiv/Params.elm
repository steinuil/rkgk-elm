module Pixiv.Params exposing
  ( Param
  , includeTotalComments, includeRankingLabels, includeRankingIllusts
  , illustId, userId, offset, maxBookmarkId, maxBookmarkIdForRecommend
  , word, tag, tags, seedIllustIds, bookmarkIllustIds, date, filterForIos
  , restrict, RestrictOpts(..)
  , contentType, type_, ContentTypeOpts(..)
  , sort, SortOpts(..)
  , searchTarget, SearchTargetOpts(..)
  , duration, DurationOpts(..)
  , mode, ModeOpts(..)
  , custom
  )

{-| Parameters for Pixiv requests.
@docs Param

# API Endpoints
@docs includeTotalComments, includeRankingLabels, includeRankingIllusts, illustId, userId, offset, maxBookmarkId, maxBookmarkIdForRecommend, word, tag, tags, seedIllustIds, bookmarkIllustIds, date, filterForIos, restrict, RestrictOpts, contentType, type_, ContentTypeOpts, sort, SortOpts, searchTarget, SearchTargetOpts, duration, DurationOpts, mode, ModeOpts, custom

-}

import Infix exposing ((=>))


{-| A parameter. -}
type alias Param = (String, String)

-- Bools
bool : String -> Bool -> Param
bool str d = str => if d then "true" else "false"

{-| Return the full text of the comments in the request. -}
includeTotalComments : Bool -> Param
includeTotalComments = bool "include_total_comments"

{-| Include the ranking position in the illustration info.  -}
includeRankingLabels : Bool -> Param
includeRankingLabels = bool "include_ranking_labels"

{-| Include the ranking illustrations in the result. -}
includeRankingIllusts : Bool -> Param
includeRankingIllusts = bool "include_ranking_illusts"

-- Ints
int : String -> Int -> Param
int str i = str => toString i

{-| The illustration ID. -}
illustId : Int -> Param
illustId = int "illust_id"

{-| The User ID. -}
userId : Int -> Param
userId = int "user_id"

{-| The offset in a list resource. -}
offset : Int -> Param
offset = int "offset"

{-| Dunno. -}
maxBookmarkId : Int -> Param
maxBookmarkId = int "max_bookmark_id"

{-| Dunno. -}
minBookmarkIdForRecentIllust : Int -> Param
minBookmarkIdForRecentIllust = int "min_bookmark_id_for_recent_illust"

{-| Dunno. -}
maxBookmarkIdForRecommend : Int -> Param
maxBookmarkIdForRecommend = int "max_bookmark_id_for_recommend"

-- Strings
{-| The search keyword. -}
word : String -> Param
word = (=>) "word"

{-| The tag to search for in the bookmarks. -}
tag : String -> Param
tag = (=>) "tag"

-- Others
{-| A list of tags to be included in the bookmark. -}
tags : List String -> Param
tags l = "tags" => String.join " " l

{-| A list of Illustration IDs to be used as seed for the related illustrations.

No idea how this works.
-}
seedIllustIds : List Int -> Param
seedIllustIds l =
  "seed_illust_ids" => String.join "," (List.map toString l)

{-| A list of illustrations to be used as seed for the unauthenticated "recommended" page. -}
bookmarkIllustIds : List Int -> Param
bookmarkIllustIds l =
  "bookmark_illust_ids" => String.join "," (List.map toString l)

{-| The date in year-month-day. -}
date : Int -> Int -> Int -> Param
date y m d =
  "date" => String.join "-"
    [ toString y
    , String.padLeft 2 '0' <| toString <| clamp 1 12 d
    , String.padLeft 2 '0' <| toString <| clamp 1 31 d
    ]

{-| No idea what this is, but it's in the default request for lots of things. -}
filterForIos : Param
filterForIos = "filter" => "for_ios"

{-| Restrict resource to either public or private follows/bookmarks. -}
restrict : RestrictOpts -> Param
restrict opt =
  "restrict" => case opt of
    Public -> "public"
    Private -> "private"

{-| -}
type RestrictOpts = Public | Private

{-| Whether to return results for illusts or novels. -}
contentType : ContentTypeOpts -> Param
contentType opt =
  "content_type" => case opt of
    Illust -> "illust"
    Manga -> "manga"

{-| Same as above. -}
type_ : ContentTypeOpts -> Param
type_ opt =
  "type" => case opt of
    Illust -> "illust"
    Manga -> "manga"

{-| -}
type ContentTypeOpts = Illust | Manga

{-| Sorting method. -}
sort : SortOpts -> Param
sort opt =
  "sort" => case opt of
    DateAsc -> "date_asc"
    DateDesc -> "date_desc"

{-| -}
type SortOpts = DateAsc | DateDesc

{-| Text matching mode. -}
searchTarget : SearchTargetOpts -> Param
searchTarget opt =
  "search_target" => case opt of
    PartialMatchForTags -> "partial_match_for_tags"
    ExactMatchForTags -> "exact_match_for_tags"
    TitleAndCaption -> "title_and_caption"

{-| -}
type SearchTargetOpts = PartialMatchForTags | ExactMatchForTags | TitleAndCaption

{-| Displayed content limit. -}
duration : DurationOpts -> Param
duration opt =
  "duration" => case opt of
    WithinLastDay -> "within_last_day"
    WithinLastWeek -> "within_last_week"
    WithinLastMonth -> "within_last_month"

{-| -}
type DurationOpts = WithinLastDay | WithinLastWeek | WithinLastMonth

{-| Mode for ranking illustrations. -}
mode : ModeOpts -> Param
mode opt =
  "mode" => case opt of
    Day -> "day"
    Week -> "week"
    Month -> "month"
    DayMale -> "day_male"
    DayFemale -> "day_female"
    DayManga -> "day_manga"
    WeekOriginal -> "week_original"
    WeekRookie -> "week_rookie"

{-| -}
type ModeOpts = Day | Week | Month | DayMale | DayFemale | DayManga | WeekOriginal | WeekRookie

{-| Define a custom parameter.

    Params.custom "mode" "day"
-}
custom : String -> String -> Param
custom = (=>)

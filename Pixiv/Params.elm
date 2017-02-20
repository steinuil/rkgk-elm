module Pixiv.Params exposing (..)

{-| Parameters for Pixiv requests.

@docs Param

@docs RestrictOpts, ContentTypeOpts, SortOpts, SearchTargetOpts, DurationOpts, ModeOpts

@docs toQuery
-}

import Infix exposing ((=>))

{-| The parameters. This was a pain to type out.
Thank god for the type checker.
-}
type Param
  = IllustId Int
  | UserId Int
  | Offset Int
  | Word String
  | Restrict RestrictOpts
  | ContentType ContentTypeOpts
  | Sort SortOpts
  | SearchTarget SearchTargetOpts
  | Duration DurationOpts
  | SeedIllustIds (List Int)
  | IncludeTotalComments Bool
  | IncludeRankingLabels Bool
  | IncludeRankingIllusts Bool
  | FilterForIos
  | Mode ModeOpts
  | MaxBookmarkId Int
  | Tag String
  | Date Int Int Int
  | MinBookmarkIdForRecentIllust Int
  | MaxBookmarkIdForRecommend Int
  | Tags (List String)

{-| -}
type RestrictOpts = Public | Private
{-| -}
type ContentTypeOpts = Illust | Manga
{-| -}
type SortOpts = DateAsc | DateDesc
{-| -}
type SearchTargetOpts = PartialMatchForTags | ExactMatchForTags | TitleAndCaption
{-| -}
type DurationOpts = WithinLastDay | WithinLastWeek | WithinLastMonth
{-| -}
type ModeOpts = Day | Week | Month | DayMale | DayFemale | DayManga | WeekOriginal | WeekRookie


{-| Transform a parameter into a tuple of strings.

    Params.toQuery (Restrict Public) --> ("restrict", "public")
-}
toQuery : Param -> (String, String)
toQuery param =
  case param of
    IllustId id ->    "illust_id" => toString id
    UserId id ->      "user_id" => toString id
    Offset i ->       "offset" => toString i
    Word word ->      "word" => word
    Tag tag ->        "tag" => tag
    FilterForIos ->   "filter" => "for_ios"

    MaxBookmarkId id ->
      "max_bookmark_id" => toString id

    MinBookmarkIdForRecentIllust id ->
      "min_bookmark_id_for_recent_illust" => toString id

    MaxBookmarkIdForRecommend id ->
      "max_bookmark_id_for_recommend" => toString id

    Date y m d ->
      "date" => String.join "-"
        [ toString y
        , String.padLeft 2 '0' <| toString <| clamp 1 12 d
        , String.padLeft 2 '0' <| toString <| clamp 1 31 d
        ]

    Restrict opt ->
      "restrict" => case opt of
        Public -> "public"
        Private -> "private"

    ContentType opt ->
      "content_type" => case opt of
        Illust -> "illust"
        Manga -> "manga"

    Sort opt ->
      "sort" => case opt of
        DateAsc -> "date_asc"
        DateDesc -> "date_desc"

    SearchTarget opt ->
      "search_target" => case opt of
        PartialMatchForTags -> "partial_match_for_tags"
        ExactMatchForTags -> "exact_match_for_tags"
        TitleAndCaption -> "title_and_caption"

    Duration opt ->
      "duration" => case opt of
        WithinLastDay -> "within_last_day"
        WithinLastWeek -> "within_last_week"
        WithinLastMonth -> "within_last_month"

    SeedIllustIds l ->
      "seed_illust_ids" => String.join "," (List.map toString l)

    IncludeTotalComments d ->
      "include_total_comments" => if d then "true" else "false"
    IncludeRankingLabels d ->
      "include_ranking_labels" => if d then "true" else "false"
    IncludeRankingIllusts d ->
      "include_ranking_illusts" => if d then "true" else "false"

    Mode opt ->
      "mode" => case opt of
        Day -> "day"
        Week -> "week"
        Month -> "month"
        DayMale -> "day_male"
        DayFemale -> "day_female"
        DayManga -> "day_manga"
        WeekOriginal -> "week_original"
        WeekRookie -> "week_rookie"

    Tags l ->
      "tags" => String.join " " l

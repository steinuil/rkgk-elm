import Pixiv.Types exposing (..)
import Pixiv
import Pixiv.Endpoints as Endpoints

import Infix exposing (..)

import LocalStorage

import Html exposing (Html, main_, a, img, text, div, span, nav, form, input)
import Html.Attributes exposing (class, id, src, title, style, href, target, type_, value, height, width)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Markdown
import Navigation
import Task


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


type alias Model =
  { page : (Page, PageInfo)
  , history : List (Page, PageInfo)
  , error : Maybe String
  , tokens : Maybe Tokens
  , loading : Bool
  , query : Maybe String
  }


type Msg =
    Query Request
  | Response (Result Http.Error (Page, PageInfo))
  | More
  | MoreResp (Result Http.Error Page)
  | LoginResp (Result Http.Error LoginInfo)
  | Detail Illust
  | Back
  | Input String
  | Search String
  | Submit
  | DismissErr
  | Inserted (Result LocalStorage.Error ())


init : Maybe String -> (Model, Cmd Msg)
init token =
  let
    model =
      { page = (EmptyPage, BasePage "")
      , history = []
      , error = Nothing
      , tokens = Nothing
      , loading = True
      , query = Nothing
      }

    startPage = case Nothing of
      Nothing -> Endpoints.ranking
      Just toks -> Endpoints.myFeed toks.access

    cmd = startPage
      |> Pixiv.send Response

    login =
      case token of
        Just tok -> Pixiv.refresh LoginResp tok
        Nothing -> Pixiv.login LoginResp "USERNAME" "PASSWORD"
  in
    model ! [ login, cmd ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Query req ->
      let
        new = { model | loading = True }

        req_ = case model.tokens of
          Nothing -> req
          Just { access, refresh } ->
            req |> Pixiv.withAuth access
      in
        new ! [ req_ |> Pixiv.send Response ]

    More ->
      let
        auth = model.tokens |> Maybe.map .access

        url = case Tuple.first model.page of
          IllustList _ url -> url
          CommentList _ url -> url
          UserPreviews _ url -> url
          _ -> Nothing

        cmd = case url of
          Just url -> Pixiv.more MoreResp auth url
          Nothing -> Cmd.none

        new = { model | loading = True }
      in
        new ! [ cmd ]


    -- Successful requests
    Response (Ok page) ->
      let
        history = case model.page of
          (EmptyPage, _) -> model.history
          p -> p :: model.history

        url = case Tuple.second page of
          BasePage name ->
            Navigation.newUrl <| "/base"
          SearchPage query ->
            Navigation.newUrl <| "/search/" ++ (Http.encodeUri query)
          UserPage name user ->
            Navigation.newUrl <| "/user/" ++ name ++ "/" ++ (toString user.id)
          IllustPage name illust ->
            Navigation.newUrl <| "/illust/" ++ name ++ "/" ++ (toString illust.id)
          ActionResult ->
            Cmd.none

        new = { model | page = page, history = history, loading = False }
      in
        new ! [ url ]

    MoreResp (Ok page) ->
      let
        newPage =
          case (Tuple.first model.page, page) of
            (IllustList list _, IllustList list2 newUrl) ->
              IllustList (list ++ list2) newUrl
            (CommentList list _, CommentList list2 newUrl) ->
              CommentList (list ++ list2) newUrl
            (UserPreviews list _, UserPreviews list2 newUrl) ->
              UserPreviews (list ++ list2) newUrl
            (p, _) -> p

        new = { model | page = (newPage, Tuple.second model.page)
                      , loading = False }
      in
        new ! []

    LoginResp (Ok info) ->
      let
        new = { model | tokens = Just <| Tokens info.access info.refresh }

        cmd = LocalStorage.set "rkgk_refreshToken" info.refresh
          |> Task.attempt Inserted
      in
        new ! [ cmd ]

    Inserted (Ok _) ->
      model ! []


    -- Failed requests
    Response (Err message) ->
      let new = { model | error = Just <| toString message, loading = False } in
        new ! []

    MoreResp (Err message) ->
      let new = { model | error = Just <| toString message, loading = False } in
        new ! []

    LoginResp (Err message) ->
      let new = { model | error = Just <| toString message } in
        new ! []

    Inserted (Err message) ->
      let new = { model | error = Just <| toString message } in
        new ! []
      {-
        error =
          case message of
            Http.BadStatus msg ->
              case Json.Decode.decodeString Pixiv.Decoders.error msg.body of
                Ok m -> m
                Err _ -> toString msg
            _ -> toString message
      -}

    Detail illust ->
      let
        page = (IllustDetail illust, IllustPage "Detail" illust)

        url = "/illust/detail/" ++ (toString illust.id)

        new = { model | page = page, history = model.page :: model.history }
      in
        new ! [ Navigation.newUrl url ]

    Back ->
      let
        tail = List.tail model.history ?: []

        (page, history) =
          case List.head model.history of
            Just p ->  (p, tail)
            Nothing -> (model.page, model.history)

        new = { model | page = page, history = history }
      in
        new ! [ Navigation.back 1 ]

    Input str ->
      let
        query = case str of
          "" -> Nothing
          _ -> Just str

        new = { model | query = query }
      in
        new ! []

    Search str ->
      let
        new = { model | query = Just str, loading = True }

        cmd = Endpoints.search str |> Pixiv.send Response
      in
        new ! [ cmd ]

    Submit ->
      case model.query of
        Nothing ->
          model ! []
        Just str ->
          let cmd = Endpoints.search str |> Pixiv.send Response in
            { model | loading = True } ! [ cmd ]

    DismissErr ->
      let new = { model | error = Nothing } in
        new ! []



-- Utilities
view : Model -> Html Msg
view model =
  let
    proxy : String -> Url
    proxy = (++) "/"


    empty = text ""


    thumb illust =
      let count = span [ class "count" ] [ text <| toString illust.count ] in
        a [ class "thumbnail link illust", onClick <| Detail illust ]
          [ img [ src <| proxy illust.thumbnail ] []
          , if illust.count > 1 then count else empty
          ]


    tag name =
      div [ class "tag" ]
        [ a [ class "link", onClick <| Search name ] [ text name ]
        , a [ class "link", onClick <| Input <| ((Maybe.map (\ x -> x ++ " ") model.query) ?: "" ++ name) ] [ text "+" ]
        ]


    pic size url =
      let
        attr =
          (src <| proxy url) ::
            (case size of
              Just (w, h) -> [ width w, height h ]
              Nothing -> [])
      in
        div [ class "picture illust" ] [ img attr [] ]

    --

    share illust =
      let
        link = "http://www.pixiv.net/member_illust.php?mode=medium&illust_id="
          ++ (toString illust.id)

        t =
          illust.title ++ " | " ++ illust.user.name ++ " #pixiv"

        tweet =
          "https://twitter.com/intent/tweet?"
            ++ "text=" ++ Http.encodeUri t
            ++ "&url=" ++ Http.encodeUri link
            ++ "&related=" ++ "steinuil"
      in
        a [ href tweet, target "_blank", class "link" ]
          [ div [] [ text "Tweet" ] ]


    searchBar =
      form [ onSubmit Submit, id "search" ]
        [ input [ type_ "text", onInput Input, value (model.query ?: "") ] []
        , input [ class "button", type_ "submit", value "Search" ] []
        ]


    navBar =
      let
        link name endpoint =
          a [ class "link", onClick <| Query <| endpoint ] [ text name ]

        (related, userIllust, shareButton) =
          case Tuple.first model.page of
            IllustDetail illust ->
              ( link "Related" <| Endpoints.related illust
              , link "User's works" <| Endpoints.userIllusts illust.user
              , share illust
              )
            _ ->
              (empty, empty, empty)

        recs = case model.tokens of
          Nothing -> Endpoints.recommended
          Just {access, refresh} ->
            Endpoints.myRecommended access

        feed = case model.tokens of
          Nothing -> empty
          Just {access, refresh} ->
            link "Feed" <| Endpoints.myFeed access

        popular = case Tuple.second model.page of
          SearchPage query -> link "Most Popular" <| Endpoints.popularPreview query
          _ -> empty

        bookmark =
          case Tuple.first model.page of
            IllustDetail illust ->
              case model.tokens of
                Nothing -> empty
                Just {access, refresh} ->
                  if illust.bookmarked
                  then link "Unbookmark" <| Endpoints.unbookmark access illust
                  else link "Bookmark" <| Endpoints.bookmark access illust
            _ -> empty

        follow =
          case Tuple.second model.page of
            UserPage _ user ->
              case model.tokens of
                Nothing -> empty
                Just {access, refresh} ->
                  link "Follow" <| Endpoints.follow access user
            _ -> empty
      in
        nav []
          [ searchBar
          , feed
          , link "Ranking" Endpoints.ranking
          , link "Recommended" recs
          , popular
          , related
          , userIllust
          , bookmark
          , follow
          , shareButton
          ]


    error = case model.error of
      Just msg ->
        div [ id "error", class "link", onClick DismissErr ] [ text msg ]
      Nothing ->
        empty


    page = case Tuple.first model.page of
      IllustList illusts url ->
        div [ id "list" ] <| illusts <!> thumb
      IllustDetail illust ->
        let
          illusts = case illust.urls of
            first :: rest ->
              pic (Just (illust.width, illust.height)) first :: rest <!> (pic Nothing)
            [] -> []
        in
          div [ id "detail" ] illusts
      _ ->
        empty


    pageInfo =
      let
        name_ t = span [ class "name" ] [ text t ]
      in
        case model.page of
          (EmptyPage, _) -> empty
          (_, ActionResult) -> empty

          (_, BasePage name) ->
            div [ id "page-info", class "base" ] [ name_ name ]

          (_, SearchPage query) ->
            div [ id "page-info", class "base" ] [ name_ <| "Search: " ++ query ]

          (_, UserPage name user) ->
            div [ id "page-info", class "artist" ]
              [ img [ class "thumb", src <| proxy user.avatar ] []
              , div [ class "cont" ] [ name_ name , name_ user.name ]
              ]

          (_, IllustPage name illust) ->
            div [ id "page-info", class "illust" ]
              [ img [ class "thumb", src <| proxy illust.thumbnail ] []
              , name_ name
              ]


    back = case model.history of
      [] -> empty
      _ ->
        a [ id "back", class "link", onClick Back ]
          [ text "back" ]


    more =
      let
        next = case Tuple.first model.page of
          IllustList _ url -> url
          CommentList _ url -> url
          UserPreviews _ url -> url
          _ -> Nothing

        link =
          if model.loading
          then style [ "opacity" => ".3" ]
          else onClick More
      in
        case next of
          Just _ ->
            div [ id "more" ]
              [ a [ class "cont", link ] [ text "Load more" ] ]
          Nothing -> empty


    info = case Tuple.first model.page of
      IllustDetail illust ->
        div [ id "detail-info", class "illust" ]
          [ div [ class "name" ] [ text illust.title ]
          , div [ class "tags" ] <| illust.tags <!> tag
          , Markdown.toHtml [ class "caption" ] illust.caption
          ]
      _ -> empty


    loading = case model.loading of
      True ->
        let box = div [ class "box" ] [] in
          div [ id "loading" ] <| List.repeat 6 box
      False -> empty
  in
    main_ []
      [ back
      , navBar
      , page
      , more
      , info
      , pageInfo
      , loading
      , error -- so that the z-index is the highest
      ]

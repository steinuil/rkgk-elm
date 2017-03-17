import Pixiv.Types exposing (..)
import Pixiv
import Pixiv.Endpoints as Endpoints

import Infix exposing (..)
--import LocalStorage

import Html exposing (Html, main_, a, img, text, div, span)
import Html.Attributes exposing (class, id, src, title)
import Html.Events exposing (onClick)
import Http
import Markdown


main =
  Html.program
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
  }


type Msg =
    Query Request
  | Response (Result Http.Error (Page, PageInfo))


init : (Model, Cmd Msg)
init =
  let
    tokens = Nothing
    {-
    tokens = case (flags.accessToken, flags.refreshToken) of
      (Just a, Just t) -> Just (Tokens a t)
      _ -> Nothing
    -}

    model =
      { page = (EmptyPage, BasePage "")
      , history = []
      , error = Nothing
      , tokens = tokens
      }

    startPage = case tokens of
      Nothing -> Endpoints.ranking
      Just toks -> Endpoints.myFeed toks.accessToken

    cmd = startPage
      |> Pixiv.send Response
  in
    model ! [ cmd ]


update msg model =
  case msg of
    Query req ->
      req |> Pixiv.send Response

    Response (Ok (page, info)) ->
      let new = { model | page = (page, info) } in
        new ! []

    Response (Err message) ->
      let new = { model | error = Just <| toString message } in
        new ! []


-- Utilities
proxy : String -> Url
proxy = (++) "http://localhost:9292/"


empty = text ""


view : Model -> Html Msg
view model =
  let
    thumb illust =
      let count = span [ class "count" ] [ text <| toString illust.count ] in
        a [ class "thumbnail link" ]
          [ img [ src <| proxy illust.thumbnail ] []
          , if illust.count > 1 then count else empty
          ]


    tag name =
      a [ class "tag link" ]
        [ text name ]


    --


    error = case model.error of
      Just msg ->
        div [ id "error", class "link" ] [ text msg ]
      Nothing ->
        empty


    page = case model.page of
      (IllustList illusts url, _) ->
        div [ id "list" ] <| illusts <!> thumb
      _ ->
        empty


    pageInfo = case model.page of
      (EmptyPage, _) ->
        empty

      (_, BasePage name) ->
        div [ id "page-info", class "base" ]
          [ span [ class "name" ] [ text name ] ]

      (_, UserPage name user) ->
        div [ id "page-info", class "artist" ]
          [ text name
          , img [ class "avatar", src <| proxy user.avatar ] []
          , a [ class "name link", title user.nick ]
            [ text user.name ]
          ]

      (_, IllustPage name illust) ->
        div [ id "page-info", class "illust" ]
          [ span [ class "name" ] [ text name ] ]


    back = case model.history of
      [] -> empty
      _ -> a [ id "back", class "link" ] [ text "back" ]


    more =
      let
        next = case Tuple.first model.page of
          IllustList _ url -> url
          CommentList _ url -> url
          UserPreviews _ url -> url
          _ -> Nothing
      in
        case next of
          Just _ -> 
            div [ id "more" ]
              [ div [ class "cont" ]
                [ text "Load more" ]
              ]
          Nothing -> empty


    {-
    info = case model.illust of
      DetailMode illust ->
        div [ id "detail-info", class "illust" ]
          [ div [ class "name" ] [ text illust.title ]
          , div [ class "tags" ] <| illust.tags <!> tag
          , Markdown.toHtml [ class "caption" ] illust.caption
          ]
    -}
  in
    main_ []
      [ back
      , pageInfo
      , page
      , more
      --, info
      , error
      ]

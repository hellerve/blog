module Blargl exposing (main)

import Html exposing (div, textarea, beginnerProgram, text, button)
import Html.Attributes exposing (style, value, class, id)
import Html.Events exposing (onInput)
import Markdown exposing (toHtml)

type Msg = Str String

update msg _ =
  case msg of
    Str s -> s


view model =
  div [ class "bg" ] [
      textarea [ value model
               , onInput Str
               , class "input"
               ] []
    , toHtml [class "input", id "output"] model
  ]


main = beginnerProgram { model="", update=update, view=view }

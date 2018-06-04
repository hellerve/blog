module Blargl exposing (main)

import Html exposing (div, textarea, beginnerProgram, text, button)
import Html.Attributes exposing (style, value, class, id)
import Html.Events exposing (onInput)
import Markdown exposing (toHtml)
import Regex exposing (regex, split)
import String exposing (trim)
import List exposing (length)

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
    , div [ class "wordcount" ]
          [ text (toString (length (split Regex.All (regex "\\s+") (trim model)))) ]
  ]


main = beginnerProgram { model="", update=update, view=view }

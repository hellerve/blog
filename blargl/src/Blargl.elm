port module Blargl exposing (main, click)

import Html exposing (div, textarea, program, text, button)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput, onClick)
import Markdown exposing (toHtml)

type Msg = Str String | Click

port click : String -> Cmd msg

init = ("", Cmd.none)

update msg i =
  case msg of
    Str s -> (s, Cmd.none)
    Click -> (i, click i)


input extra =
  style ([ ("width",  "44vw")
         , ("margin-left", "2.5vw")
         , ("margin-right", "2.5vw")
         , ("height", "80vh")
         , ("margin-top", "5vh")
         , ("border", "none")
         , ("background-color", "white")
         , ("resize", "none")
         , ("float", "left")
         , ("padding", "1em")
         , ("box-sizing", "border-box")
         , ("margin-bottom", "1em")
         , ("font-size", "20px")
         ] ++ extra)

btn = style [ ("border", "3px solid #333")
            , ("border-radius", "0px")
            , ("padding", "5px")
            , ("font-size", "20px")
            , ("margin-left", "90vw")
            ]

bg = style [ ("background-color", "#eee")
           , ("height", "100vh")
           ]

view model =
  div [ bg ] [
      textarea [ value model
               , onInput Str
               , input []
               ] []
    , toHtml [input [("font-family", "charter")]] model
    , button [btn, onClick Click] [text "save"]
  ]


main = program { init=init, update=update, view=view, subscriptions=\x->Sub.none }

module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String exposing (toUpper, repeat, trimRight)
import StartApp.Simple as StartApp

-- MODEL

newEntry phrase points id =
  { phrase = phrase,
    points = points,
    wasSpoken = False,
    id = id
  }

-- initialModel =
--   { entries =
--     [ newEntry "Doing agile" 200 2,
--       newEntry "The Cloud" 300 3,
--       newEntry "Future proof" 100 1,
--       newEntry "Rock-star Ninja" 400 4,
--       newEntry "Sumbling forwards aimlessly" 600 6
--     ]
--   }
initialModel =
  { entries =
    [ newEntry "Doing reflexive" 200 2,
      newEntry "Host it in S P A C E!" 300 3,
      newEntry "Future evident" 100 1,
      newEntry "Rap gansta brawler scrawler" 400 4,
      newEntry "Sumbling forwards aimlessly" 600 6
    ]
  }

-- UPDATE

type Action
  = NoOp
  | Sort
  | Delete Int
  | Mark Int

update action model =
  case action of
    NoOp ->
      model
    Sort ->
      { model | entries = List.sortBy .points model.entries }
    Delete id ->
      let
        remainingEntries =
          List.filter (\e -> id /= e.id) model.entries
      in
        { model | entries = remainingEntries }
    Mark id ->
      let
        ifIdMatchesThenTogglePropertyToTrue entry =
          {entry | wasSpoken = True }
        entriesWithANewOneHighlighted =
          List.map ifIdMatchesThenTogglePropertyToTrue model.entries
      in
        {model | entries = entriesWithANewOneHighlighted }

-- VIEW

title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text

pageHeader =
  h1 [ ] [ title "Bingo!" 3 ]

pageFooter =
  footer [ ]
    [ a [ href "http://webup.info" ]
        [ text "A nice website for testing out Elm" ]
    ]

entryItem address entry =
  li [ classList [ ("highlight", entry.wasSpoken) ] ]
    [ span [class "phrase"] [ text entry.phrase ],
      span [class "points"] [ text (toString entry.points) ],
      button
        [class "delete", onClick address (Delete entry.id) ]
        [  ]

    ]

entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
  in
    ul [ ] entryItems

view address model =
  div [ id "container" ]
    [ pageHeader,
      entryList address model.entries,
      button
        [ class "sort", onClick address Sort ]
        [ text "Sort"],
      button
        [ class "sort", onClick address (Mark 0) ]
        [ text "Mark"],
      pageFooter
    ]

-- WIRE DAT SHIZZLE

main =
  -- initialModel
  --   |> update Sort
  --   |> view
  StartApp.start
    { model = initialModel,
      view = view,
      update = update
    }

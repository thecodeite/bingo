module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import String exposing (toUpper, repeat, trimRight)
import StartApp.Simple as StartApp


import BingoUtils as Utils
-- MODEL

type alias Entry =
  { phrase: String,
    points: Int,
    wasSpoken: Bool,
    id: Int
  }

type alias Model =
  {
    entries: List Entry,
    phraseInput: String,
    pointsInput: String,
    nextID: Int
  }

newEntry : String -> Int -> Int -> Entry
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

initialModel : Model
initialModel =
  { entries =
    [ newEntry "Doing reflexive" 200 2,
      newEntry "Host it in S P A C E!" 300 3,
      newEntry "Future evident" 100 1,
      newEntry "Rap gansta brawler scrawler" 400 4,
      newEntry "Sumbling forwards aimlessly" 600 6
    ],
    phraseInput = "",
    pointsInput = "",
    nextID = 5
  }

-- UPDATE

type Action
  = NoOp
  | Sort
  | Delete Int
  | Mark Int
  | UpdatePhraseInput String
  | UpdatePointsInput String

update : Action -> Model -> Model
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
        updateEntry e =
          if e.id == id then { e | wasSpoken = (not e.wasSpoken) } else e
      in
        {model | entries = List.map updateEntry model.entries }

    UpdatePhraseInput contents ->
      { model | phraseInput = contents }

    UpdatePointsInput contents ->
      { model | pointsInput = contents }
-- VIEW

title : String -> Int -> Html
title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text

pageHeader : Html
pageHeader =
  h1 [ ] [ title "Bingo!" 3 ]

pageFooter : Html
pageFooter =
  footer [ ]
    [ a [ href "http://webup.info" ]
        [ text "A nice website for testing out Elm" ]
    ]

entryItem : Address Action -> Entry -> Html
entryItem address entry =
  li
    [ classList [ ("highlight", entry.wasSpoken) ],
      onClick address (Mark entry.id)
    ]
    [ span [class "phrase"] [ text entry.phrase ],
      span [class "points"] [ text (toString entry.points) ],
      button
        [class "delete", onClick address (Delete entry.id) ]
        [  ]

    ]

totalPoints : List Entry -> Int
totalPoints entries =
  let
    spokenEnties = List.filter .wasSpoken entries
  in
    List.sum (List.map .points spokenEnties)

totalItem : Int -> Html
totalItem total =
  li
    [ class "total" ]
    [ span [ class "label" ] [ text "Total" ],
      span [ class "points" ] [ text (toString total) ]
    ]

entryList : Address Action -> List Entry -> Html
entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
    items = entryItems ++ [ totalItem (totalPoints entries) ]
  in
    ul [ ] items

totalScore entries =
  div [id "total"]
    [ text "Score:",
      entries
        |> (List.filter .wasSpoken)
        |> (List.map .points)
        |> List.sum
        |> toString
        |> text
    ]

entryForm : Address Action -> Model -> Html
entryForm address model =
  div [ ]
    [ input
      [ type' "text",
        placeholder "Phrase",
        value model.phraseInput,
        name "phrase",
        autofocus True,
        Utils.onInput address UpdatePhraseInput
      ]
      [ ],
      input
      [ type' "number",
        placeholder "Points",
        value model.pointsInput,
        name "points",
        Utils.onInput address UpdatePointsInput
       ]
      [ ],
      button [class "add"] [text "Add"],
      h2 [ ]
        [ text (model.phraseInput ++ " " ++ model.pointsInput)
        ]

   ]



view : Address Action -> Model -> Html
view address model =
  div [ id "container" ]
    [ pageHeader,
      entryForm address model,
      entryList address model.entries,
      button
        [ class "sort", onClick address Sort ]
        [ text "Sort"],
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

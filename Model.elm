module Model exposing
  ( BaseEntry, Entry, Model
  , dependentFields, empty, entry, newEntry, setDescription, setEditing
  , uid
  )

import Dict exposing (Dict)

import NonBlankString exposing (NonBlankString, nonBlankString)
import Tagged exposing (Tagged, tag)
import Visibility exposing (Visibility, Active, Completed, all)

type alias Model =
  { active : Dict Int (Entry Active)
  , allCompleted : Bool
  , completed : Dict Int (Entry Completed)
  , entriesCompleted : Int
  , entriesLeft : Int
  , field : String
  , noneVisible : Bool
  , uid : Int
  , visibility : Visibility
  }

type alias BaseEntry =
  { description : NonBlankString
  , editing : Bool
  }

type alias Entry visibility =
  Tagged visibility BaseEntry

empty : Model
empty =
  { active = Dict.empty
  , allCompleted = False
  , completed = Dict.empty
  , entriesCompleted = 0
  , entriesLeft = 0
  , field = ""
  , noneVisible = True
  , uid = 0
  , visibility = all
  }

newEntry : String -> Maybe (Entry Active)
newEntry =
  entry

entry : String -> Maybe (Entry a)
entry title =
  nonBlankString title
    |> Maybe.map (\str -> tag {description = str, editing = False})

dependentFields : Model -> Model
dependentFields =
  allCompleted
  << entriesCompleted
  << entriesLeft
  << noneVisible

allCompleted : Model -> Model
allCompleted ({active, completed} as model) =
  {model | allCompleted = Dict.isEmpty active && not (Dict.isEmpty completed)}

entriesCompleted : Model -> Model
entriesCompleted ({completed} as model) =
  {model | entriesCompleted = Dict.size completed}

entriesLeft : Model -> Model
entriesLeft ({active} as model) =
  {model | entriesLeft = Dict.size active}

noneVisible : Model -> Model
noneVisible ({active, completed} as model) =
  {model | noneVisible = Dict.isEmpty active && Dict.isEmpty completed}

uid : Model -> Model
uid ({active, completed} as model) =
  { model
  | uid =
      Maybe.map2 max (List.maximum (Dict.keys active)) (List.maximum (Dict.keys completed))
        |> Maybe.withDefault 0
        |> (+) 1
  }

setEditing : Bool -> Entry a -> Entry a
setEditing editing =
  Tagged.map (\record -> {record | editing = editing})

setDescription : String -> Entry a -> Maybe (Entry a)
setDescription str entry =
  nonBlankString str
    |> Maybe.map (\non ->
        Tagged.map (\record -> {record | description = non}) entry
      )

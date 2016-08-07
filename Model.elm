module Model exposing
  ( Entry, Model
  , dependentFields, empty, entry, newEntry, setDescription, uid
  )

import Dict exposing (Dict)

import NonBlankString exposing (NonBlankString, nonBlankString)
import Tagged exposing (Tagged, tag)
import Visibility exposing (Visibility, Active, Completed, all)

type alias Model =
  { active : Dict Int (Entry Active)
  , allCompleted : Bool
  , completed : Dict Int (Entry Completed)
  , editing : Maybe Int
  , entriesCompleted : Int
  , entriesLeft : Int
  , field : String
  , noneVisible : Bool
  , uid : Int
  , visibility : Visibility
  }

type alias Entry visibility =
  Tagged visibility NonBlankString

empty : Model
empty =
  { active = Dict.empty
  , allCompleted = False
  , completed = Dict.empty
  , editing = Nothing
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
  Maybe.map tag (nonBlankString title)

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
      (Dict.keys active ++ Dict.keys completed)
        |> List.maximum
        |> Maybe.withDefault 0
        |> (+) 1
  }

setDescription : String -> Entry a -> Maybe (Entry a)
setDescription str _ =
  entry str

module Storage exposing (..)

import Dict exposing (Dict)

import Model exposing (dependentFields, empty, entry, uid)
import NonBlankString exposing (string)
import Tagged exposing (untag)

type alias Model =
  List Entry

type alias Entry =
  { completed : Bool
  , id : Int
  , title : String
  }

storage : Model.Model -> Model
storage ({active, completed, visibility} as model) =
  storageEntry False active ++ storageEntry True completed

egarots : Model -> Model.Model
egarots storageModel =
  let
    (completed, active) =
      List.partition .completed storageModel
  in
    { empty
    | active = yrtneEgarots active
    , completed = yrtneEgarots completed
    }
      |> dependentFields
      |> uid

storageEntry : Bool -> Dict comparable (Model.Entry a) -> List Entry
storageEntry completed =
  Dict.values << Dict.map (\id tagged ->
    { completed = completed
    , id = id
    , title = NonBlankString.string (untag tagged)
    }
  )

yrtneEgarots : List Entry -> Dict Int (Model.Entry a)
yrtneEgarots entries =
  List.filterMap (\{id, title} -> Maybe.map ((,) id) (entry title)) entries
    |> Dict.fromList

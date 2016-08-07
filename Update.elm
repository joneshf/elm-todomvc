module Update exposing (Msg(..), updateWithStorage)

import Dict exposing (Dict)

import Model exposing (Entry, Model, dependentFields, newEntry, setDescription)
import Storage exposing (storage)
import Tagged exposing (retag)
import Visibility exposing (Visibility)

{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = UpdateField String
    | EditingEntry Int
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility Visibility

update : (String -> Cmd msg) -> Msg -> Model -> (Model, Cmd msg)
update focus msg =
  updateCmd focus msg
    << dependentFields
    << updateModel msg

{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : (Storage.Model -> Cmd msg) -> (String -> Cmd msg) -> Msg -> Model -> (Model, Cmd msg)
updateWithStorage setStorage focus msg model =
  case update focus msg model of
    (newModel, cmds) ->
      newModel ! [ setStorage (storage newModel), cmds ]

-- How we update our Model on a given Msg?
updateModel : Msg -> Model -> Model
updateModel msg ({active, completed, uid, field} as model) =
  case msg of
    Add ->
      { model
      | uid = uid + 1
      , field = ""
      , active = Dict.update uid (\_ -> newEntry field) active
      }

    UpdateField str ->
      { model | field = str }

    EditingEntry id ->
      { model | editing = Just id }

    UpdateEntry id task ->
      { model
      | active = updateEntry id task active
      , completed = updateEntry id task completed
      , editing = Nothing
      }

    Delete id ->
      { model
      | active = Dict.remove id active
      , completed = Dict.remove id completed
      }

    DeleteComplete ->
      { model | completed = Dict.empty }

    Check id True ->
      { model
      | active = Dict.remove id active
      , completed =
          Dict.update id (\_ -> Maybe.map retag (Dict.get id active)) completed
      }

    Check id False ->
      { model
      | active =
          Dict.update id (\_ -> Maybe.map retag (Dict.get id completed)) active
      , completed = Dict.remove id completed
      }

    CheckAll True ->
      { model
      | active = Dict.empty
      , completed = Dict.union (Dict.map (\_ -> retag) active) completed
      }

    CheckAll False ->
      { model
      | active = Dict.union active (Dict.map (\_ -> retag) completed)
      , completed = Dict.empty
      }

    ChangeVisibility visibility ->
      { model | visibility = visibility }


updateCmd : (String -> Cmd msg) -> Msg -> a -> (a, Cmd msg)
updateCmd focus msg x =
  case msg of
    EditingEntry id ->
      x ! [ focus ("#todo-" ++ toString id) ]
    _ ->
      x ! []

updateEntry : comparable -> String -> Dict comparable (Entry a) -> Dict comparable (Entry a)
updateEntry id task =
  Dict.update id (flip Maybe.andThen (setDescription task))

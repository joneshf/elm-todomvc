port module Todo exposing (..)
{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Html.App as App

import Model exposing (Model)
import Storage exposing (egarots)
import Update exposing (updateWithStorage)
import View exposing (view)

main : Program (Maybe Storage.Model)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage setStorage focus
    , subscriptions = \_ -> Sub.none
    }

port setStorage : Storage.Model -> Cmd msg

port focus : String -> Cmd msg

init : Maybe Storage.Model -> (Model, Cmd msg)
init savedModel =
  Maybe.withDefault Model.empty (Maybe.map egarots savedModel) ! []

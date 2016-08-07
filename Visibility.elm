module Visibility exposing
  ( Visibility, Active, Completed
  , active, all, completed, string
  )

import These exposing (These(..), these)

type Active
  = Active

type Completed
  = Completed

type alias Visibility =
  These Active Completed

active : Visibility
active =
  This Active

all : Visibility
all =
  These Active Completed

completed : Visibility
completed =
  That Completed

string : Visibility -> String
string =
  these toString toString (\_ _ -> "All")

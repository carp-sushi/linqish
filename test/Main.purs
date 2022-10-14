module Test.Main where

import Prelude (Unit, discard, identity, ($), (>>>))

import Data.Array (zip)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

-- Functionality under test
import Linqish (Query(..), runQuery, select_, join_, where_)

type RowA =
  { id :: Int
  , name :: String
  }

type TableA = Array RowA

tableA :: TableA
tableA =
  [ { id: 1, name: "row 1" }
  , { id: 2, name: "row 2" }
  , { id: 3, name: "row 3" }
  , { id: 4, name: "row 4" }
  ]

type RowB =
  { id :: Int
  , fk :: Int
  , active :: Boolean
  }

type TableB = Array RowB

tableB :: TableB
tableB =
  [ { id: 4, fk: 1, active: true }
  , { id: 3, fk: 2, active: false }
  , { id: 2, fk: 3, active: true }
  , { id: 1, fk: 4, active: false }
  ]

-- ID select: just return all rows from a table.
selectAll :: forall a. Array a -> Array a
selectAll table =
  runQuery $
    Query_ (select_ identity) table

-- Testing where_
selectActives :: TableB -> TableB
selectActives table =
  runQuery $
    Query (select_ identity) table (where_ _.active)

-- Testing select_ with join_
joinTables :: TableA -> TableB -> Array (Tuple RowA RowB)
joinTables a b =
  runQuery $
    Query_ (select_ identity) (join_ a _.id b _.fk)

-- Testing join_ with where_
joinTablesActive :: TableA -> TableB -> Array String
joinTablesActive a b =
  runQuery $
    Query
      (select_ $ fst >>> _.name)
      (join_ a _.id b _.fk)
      (where_ $ snd >>> _.active)

-- Run the tests
main :: Effect Unit
main = do
  runTest do
    suite "Linqish tests" do
      test "select_ returns all rows" do
        Assert.equal (selectAll tableA) tableA
      test "select_ with where_ filters rows" do
        let expected = [ { id: 4, fk: 1, active: true }, { id: 2, fk: 3, active: true } ]
        Assert.equal (selectActives tableB) expected
      test "select_ with join_ joins all rows" do
        Assert.equal (joinTables tableA tableB) (zip tableA tableB)
      test "select_ with join_ and where_ joins and filters" do
        Assert.equal (joinTablesActive tableA tableB) [ "row 1", "row 3" ]

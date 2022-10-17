module Test.Main where

import Prelude

import Data.Array (zip)
import Data.Tuple (Tuple, fst, snd)
import Control.Alternative (class Alternative)
import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

-- Functionality under test
import Linqish (Query(..), runQuery, select_, join_, where_)

--
-- Table A types and helpers - use ADT here
--

data RowA = RowA Int String

rowA_id :: RowA -> Int
rowA_id (RowA id _) = id

rowA_name :: RowA -> String
rowA_name (RowA _ name) = name

instance eqRowA :: Eq RowA where
  eq (RowA i1 _) (RowA i2 _) = i1 == i2

instance showRowA :: Show RowA where
  show (RowA id name) =
    "RowA { id: " <> (show id) <> ", name: " <> name <> " }"

tableA :: Array RowA
tableA =
  [ RowA 1 "row 1"
  , RowA 2 "row 2"
  , RowA 3 "row 3"
  , RowA 4 "row 4"
  ]

--
-- Table B types - use a record type here 
--

newtype RowB = RowB { id :: Int, fk :: Int, active :: Boolean }

instance eqRowB :: Eq RowB where
  eq (RowB rb1) (RowB rb2) = rb1.id == rb2.id

instance showRowB :: Show RowB where
  show (RowB rb) =
    "RowB { id: "
      <> (show rb.id)
      <> ", fk: "
      <> (show rb.fk)
      <> ", active: "
      <> (show rb.active)
      <> " }"

rowB_fk :: RowB -> Int
rowB_fk (RowB rb) = rb.fk

rowB_active :: RowB -> Boolean
rowB_active (RowB rb) = rb.active

tableB :: Array RowB
tableB =
  [ RowB { id: 4, fk: 1, active: true }
  , RowB { id: 3, fk: 2, active: false }
  , RowB { id: 2, fk: 3, active: true }
  , RowB { id: 1, fk: 4, active: false }
  ]

--
-- Test functions
--

-- ID select: just return all rows from a table.
selectAll :: forall m a. Bind m => Alternative m => m a -> m a
selectAll table =
  runQuery $ Query_ (select_ identity) table

-- Testing where_
selectActives :: Array RowB -> Array RowB
selectActives table =
  runQuery $ Query (select_ identity) table (where_ rowB_active)

-- Testing select_ with join_
joinTables :: Array RowA -> Array RowB -> Array (Tuple RowA RowB)
joinTables a b =
  runQuery $ Query_ (select_ identity) (join_ a rowA_id b rowB_fk)

activeNames :: Array RowA -> Array RowB -> Query Array (Tuple RowA RowB) String
activeNames a b =
  Query
    (select_ $ fst >>> rowA_name)
    (join_ a rowA_id b rowB_fk)
    (where_ $ snd >>> rowB_active)

--
-- Test runner
--

-- Run the tests
main :: Effect Unit
main = do
  runTest do
    suite "Linqish tests" do
      test "select_ returns all rows" do
        Assert.equal (selectAll tableA) tableA
      test "select_ with where_ filters rows" do
        Assert.equal (selectActives tableB)
          [ RowB { id: 4, fk: 1, active: true }
          , RowB { id: 2, fk: 3, active: true }
          ]
      test "select_ with join_ joins all rows" do
        Assert.equal (joinTables tableA tableB) (zip tableA tableB)
      test "select_ with join_ and where_ joins and filters" do
        let q = activeNames tableA tableB
        Assert.equal (runQuery q) [ "row 1", "row 3" ]

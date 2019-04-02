--------------------------------------------------------------------------------
--- This module provides general type synonym shortcuts and data types for
--- models (especially helpful for the current implementation of searches).
--- This includes definitions for filters, sortings and orderings.
---
--- @author Lasse Kristopher Meyer
--- @version November 2018
--------------------------------------------------------------------------------

module System.Models (
  Filter,Sorting,SortingType(..),Ordering,OrderingType(..),getOrdering
) where

import Prelude hiding (Ordering)

-----------------------------------------------------------------------------
-- Data types and type synonyms                                            --
-----------------------------------------------------------------------------

--- A filter is a function that takes a list of values and returns a filtered
--- list of values.
type Filter a = [a] -> [a]

--- A sorting is primarily a function that compares two values of a type and
--- defines their relation. The sorting type specifies the "direction" of the
--- sorting function (e.g. `LEQ`).
type Sorting a = (SortingType,a -> a -> Bool)

--- Sorting types specify the "direction" of sorting functions. Currently
--- available sorting types are the following.
--- @cons LEQ - specifies a "lesser-than or equal" sorting (`<=`)
data SortingType = LEQ

--- An ordering takes a sorting function and returns a (possibly modified)
--- sorting function with respect to the sorting type (see `getOrdering`).
type Ordering a = (a -> a -> Bool) -> (a -> a -> Bool)

--- An ordering type specifies the order of a result list.
--- @cons Ascending  - for ascending order
--- @cons Descending - for descending order
data OrderingType = Ascending | Descending

--------------------------------------------------------------------------------
-- Orderings                                                                  --
--------------------------------------------------------------------------------

-- Returns a function that takes a sorting type and returns an ordering function
-- that to be applied on the sorting. For instance, if the sorting type `LEQ`
-- is given with an ascending order, the identity function will be returned.
-- @param sortingType  - the type of sorting
-- @param orderingType - the ordering type specification
getOrdering :: SortingType -> OrderingType -> IO (Ordering a)
getOrdering sortingType orderingType = return $
  case orderingType of
    Ascending  -> case sortingType of
      LEQ -> keepSorting
    Descending -> case sortingType of
      LEQ -> flipSorting
  where
    keepSorting   = id
    flipSorting s = \x y -> not $ s x y

------------------------------------------------------------------------------

{- Other stuff that does not work...
--------------------------------------------------------------------------------
-- Data types and type synonyms                                               --
--------------------------------------------------------------------------------

--- A query is a data object containing all necessary information for searches
--- such as filtering constraints, sorting settings and the ordering of the
--- result list.
--- @cons Query qCstrs s o - generates a new query for a given type with a list
---   `qCstrs` of query constraints, a sorting `s` and an ordering `o`
data Query a = Query [QueryConstraint a] (Sorting a) Ordering

--- ...
data QueryConstraint a
--  = BoolConstraint   (AtomicConstraint a Bool)
--  | IntConstraint    (AtomicConstraint a Int )
  = StringConstraint String (AtomicConstraint a String)
  | CstrDisjunction  [QueryConstraint a]
  | CstrConjunction  [QueryConstraint a]

-- ...
data AtomicConstraint a b
  = Exact           (Selector a String)
  | ExactIgnoreCase (Selector a String)
  | InfixIgnoreCase (Selector a String)
  | Exists (b -> AtomicConstraint a b) (Selector a [b])

-- ...
type Selector a b = a -> b

--- A sorting is a pair containing the sorting type (e.g. `LEQ`) and a function
--- that defines the appropriate relation between two values of the given type.
type Sorting a = (SortingType,Sort a)

--- Sorting types specify the "direction" of sorting functions. Currently
--- available sorting types are the following.
--- @cons LEQ - specifies a "lesser-than or equal" sorting (`<=`)
data SortingType = LEQ

--- The data type `Order` specifies the order of a result list.
--- @cons Ascending  - for ascending order
--- @cons Descending - for descending order
data Ordering = Ascending | Descending

--------------------------------------------------------------------------------
-- Running queries                                                            --
--------------------------------------------------------------------------------

--- Runs a query and returns the list of results as an I/O action.
--- @param query         - the query specifying the filtering, sorting and ordering
--- @param getAllResults - an I/O operation to get the complete list of all
---   results that will be filtered, sorted and ordered
runQuery :: Query a -> IO [a] -> IO [a]
runQuery (Query queryCstrs sorting ordering) getAllResults =
  do allResults <- getAllResults
     filter     <- getFilter queryCstrs
     sort       <- getSort sorting
     order      <- getOrder sorting ordering
     return $ mergeSort (order sort) $ filter allResults

-- ...
getFilter :: [QueryConstraint a] -> IO ([a] -> [a])
getFilter qStrs = return (\res -> 
  filter (foldr (joinCstrs (&&)) (const True) $ map queryCstrToCstr qStrs) res)

-- ...
type Constraint a = a -> Bool

-- ...
joinCstrs
  :: (Bool -> Bool -> Bool) -> Constraint a -> Constraint a -> Constraint a
joinCstrs b cstr1 cstr2 = (\ent -> b (cstr1 ent) (cstr2 ent))

-- ...
queryCstrToCstr :: QueryConstraint a -> Constraint a
queryCstrToCstr queryCstr = case queryCstr of
  StringConstraint str aCstr -> atomicCstrToCstr str aCstr
  CstrDisjunction  qCs       ->
    foldr (joinCstrs (||)) (const True) $ map queryCstrToCstr qCs
  CstrConjunction  qCs       ->
    foldr (joinCstrs (&&)) (const True) $ map queryCstrToCstr qCs
 
-- ...
atomicCstrToCstr :: b -> AtomicConstraint a b -> Constraint a
atomicCstrToCstr val aCstr ent =
  case aCstr of
    Exact           sel -> val==(sel ent)
    ExactIgnoreCase sel -> (lowercased val)==(lowercased$sel ent)
    InfixIgnoreCase sel -> (lowercased val)`isInfixOf`(lowercased$sel ent)
    Exists          aCstr sel ->
      foldr (joinCstrs (||)) (const True) $ map (atomicCstrToCstr . aCstr) (sel ent)
  where
    lowercased = map toLower

-- Simply extracts the actual sort function from the given sorting.
-- @param sorting - the sorting containing the sorting function
getSort :: Sorting a -> IO (Sort a)
getSort = return . snd

-- ...
type Sort a = a -> a -> Bool

-- Returns a function that takes a sorting and returns the (possibly) modified
-- sorting with respect to the given ordering. For instance, if a sorting of
-- type `LEQ` is given in combination with an ascending ordering, the sorting
-- will be returned unmodified.
-- @param sorting  - the sorting containing the sorting type
-- @param ordering - the ordering specification
getOrder :: Sorting a -> Ordering -> IO (Sort a -> Sort a)
getOrder (sortingType,_) ordering = return $
  case ordering of
    Ascending  -> case sortingType of
                    LEQ -> keepSorting
    Descending -> case sortingType of
                    LEQ -> flipSorting
  where
    keepSorting   = id
    flipSorting s = \x y -> not $ s x y
-}

--------------------------------------------------------------------------------
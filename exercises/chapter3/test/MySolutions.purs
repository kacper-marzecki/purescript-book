module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (List(..), any, find, foldl, nubBy, reverse)
import Data.List.Lazy as List
import Data.Maybe (Maybe)
import Data.Set (Set, insert, member)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Prim.Ordering (EQ, LT)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = find predicate book
    where 
    predicate entry = entry.address.street == street



isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = any predicate book
    where 
    predicate entry = entry.lastName == firstName || entry.lastName == lastName

type Acc = Tuple (Set (Tuple String String)) AddressBook 
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = reverse $ snd $ foldl op initialAcc  book
    where 
    initialAcc :: Acc
    initialAcc =  Set.empty /\  Nil
    op :: Acc -> Entry -> Acc
    op ( names /\ result)  entry = 
        if member currentKey names  
        then names /\ result
        else insert currentKey names /\ (Cons entry result)
        where 
            currentKey = (key entry)
    key e = Tuple e.firstName e.lastName

-- alternative solution 
removeDuplicates' :: AddressBook -> AddressBook
removeDuplicates' book = nubBy ordering book
    where 
        ordering a b = 
            if a.firstName == b.firstName && a.lastName == b.lastName 
                then EQ
                -- using Ordering instead of Eq because of the complexity (`O(n log n)` vs `O(n ^2)`) 
                else LT 

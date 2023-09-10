module Chez.Grater.Conversion where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Readable.Types (ReadableFraction(..), ReadableQuantity(..), ReadableUnit(..))
import Chez.Grater.Types
  ( Ingredient(..), Quantity(..), Unit(..), cup, gram, liter, milligram, milliliter, ounce, pinch
  , tablespoon, teaspoon
  )
import Data.Monoid (Sum(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data UnitHierarchy
  = UnitHierarchyEnd (Unit, Quantity)
  | UnitHierarchyMid (Unit, Quantity) (Unit, Quantity)
  deriving (Eq, Ord, Show)

conversionTable :: Map Unit UnitHierarchy
conversionTable = Map.fromList
  [ (ounce, UnitHierarchyEnd (cup, 8))
  , (cup, UnitHierarchyMid (tablespoon, 16) (ounce, 0.125))
  , (tablespoon, UnitHierarchyMid (teaspoon, 3) (cup, 0.0625))
  , (teaspoon, UnitHierarchyMid (pinch, 4) (tablespoon, Quantity $ 1 / 3))
  , (pinch, UnitHierarchyEnd (tablespoon, 0.25))

  , (liter, UnitHierarchyEnd (milliliter, 1000))
  , (gram, UnitHierarchyEnd (milligram, 1000))
  ]

getAllConversions :: Unit -> Map Unit Quantity
getAllConversions = snd . go mempty
  where
    go oldVisited next =
      let visited = Set.insert next oldVisited
      in case (Set.member next oldVisited, Map.lookup next conversionTable) of
        (True, _) -> (oldVisited, mempty)
        (_, Nothing) -> (visited, mempty)
        (_, Just (UnitHierarchyEnd (x, q))) ->
          let (newVisited, newConversions) = go visited x
          in (newVisited, Map.insert x q $ fmap ((*) q) newConversions)
        (_, Just (UnitHierarchyMid (x1, q1) (x2, q2))) ->
          let (newVisited1, newConversions1) = go visited x1
              (newVisited2, newConversions2) = go newVisited1 x2
          in (newVisited2, Map.fromList [(x1, q1), (x2, q2)] <> fmap ((*) q1) newConversions1 <> (fmap ((*) q2) newConversions2))

knownUnitOrdering :: Map Unit Int
knownUnitOrdering = Map.fromList $ zip [ounce, cup, tablespoon, teaspoon, pinch] [1..]

unitOrdering :: Unit -> Unit -> Ordering
unitOrdering x y = case (Map.lookup x knownUnitOrdering, Map.lookup y knownUnitOrdering) of
  (Just a, Just b) -> compare a b
  (Just _, Nothing) -> LT
  (Nothing, Just _) -> GT
  (Nothing, Nothing) -> compare x y

combineQuantities :: Map Unit (Sum Quantity) -> Map Unit (Sum Quantity)
combineQuantities = foldr go mempty . reverse . sortBy (\(x, _) (y, _) -> unitOrdering x y) . Map.toList
  where
    go (nextUnit, nextQuantity) acc = case Map.lookup nextUnit acc of
      Just existingQuantity -> Map.insert nextUnit (nextQuantity <> existingQuantity) acc
      Nothing ->
        let allConversions = getAllConversions nextUnit
            allConversionsKeys = Map.keysSet allConversions
            existingKeys = Map.keysSet acc
            overlappingKeys = headMay . sortBy unitOrdering . Set.toList . Set.intersection allConversionsKeys $ existingKeys
        in case overlappingKeys of
          Nothing -> Map.insert nextUnit nextQuantity acc
          Just existingKey -> Map.insertWith (<>) existingKey (Sum $ getSum nextQuantity * Map.findWithDefault 1 existingKey allConversions) acc

combineItems :: [Ingredient] -> [Ingredient]
combineItems =
  mconcat
    . fmap (\(ingredientName, everythingElse) -> fmap (\(ingredientUnit, ingredientQuantity) -> Ingredient {..}) $ Map.toList everythingElse)
    . Map.toList
    . fmap (fmap getSum . combineQuantities . foldr (uncurry (Map.insertWith (<>))) mempty . fmap (second Sum))
    . Map.unionsWith (<>)
    . fmap ( \x ->
        let Ingredient {..} = x
        in Map.singleton ingredientName [(ingredientUnit, ingredientQuantity)]
      )

mkQuantity :: ReadableQuantity -> Quantity
mkQuantity q =
  let raw = case q of
        ReadableQuantity (Just whole) (Just (ReadableFraction numerator denominator)) -> fromIntegral whole + (fromIntegral numerator / fromIntegral denominator)
        ReadableQuantity Nothing (Just (ReadableFraction numerator denominator)) -> fromIntegral numerator / fromIntegral denominator
        ReadableQuantity (Just whole) Nothing -> fromIntegral whole
        ReadableQuantity Nothing Nothing -> 0
  in if raw == 0 then QuantityMissing else Quantity raw

mkUnit :: Maybe ReadableUnit -> Unit
mkUnit = \case
  Just (ReadableUnit x) | x /= "" -> Unit x
  _ -> UnitMissing

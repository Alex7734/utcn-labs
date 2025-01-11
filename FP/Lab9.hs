data Color = Red | Green | Blue
instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False

-- The classes that can be derived include: Eq , Ord , Show , Read , Enum , Bounded.
data Medal = Gold | Silver | Bronze deriving (Show, Eq)
instance Ord Medal where
  compare Gold Gold = EQ
  compare Gold _ = GT
  compare _ Gold = LT
  compare Silver Silver = EQ
  compare Silver _ = GT
  compare _ Silver = LT
  compare Bronze Bronze = EQ


instance Enum Medal where
  fromEnum Gold = 1
  fromEnum Silver = 2
  fromEnum Bronze = 3
  
  toEnum 1 = Gold
  toEnum 2 = Silver
  toEnum 3 = Bronze

instance Boundedd Medal where
  minBound = Bronze
  maxBound = Gold

medals :: [Medal]
medals = [minBound .. maxBound]

-- Defining new type classes 

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno b = b

instance YesNo String where
    yesno "" = False
    yesno _ = True

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

-- Here c has kind * -> * because it needs a type 
-- ( a in the hasElem and nrElems functions) to be a fully instantiated type.
class Container c where 
    hasElem :: (Eq a) => c a -> a -> Bool
    nrElmems :: c a -> Int

instance Container Maybe where
    hasElem (Just x) y = x == y
    hasElem Nothing _ = False

    nrElems (Just _) = 1
    nrElems Nothing = 0

instance Container [] where
    hasElem l e = elem e l
    nrElems l = length l

-- Definig semigroups and monoids 

-- A semigroup is a set S with an associative binary operation.
instance Semigroup [a] where
    (<>) = (++)

-- A monoid is a semigroup with an identity element.
instance Monoid [a] where
    mempty = []
    mappend = (++)

-- Defining the Maybe monoid

instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> m = m
    m <> Nothing = m
    Just x <> Just y = Just (x <> y)

instance Semigroup a => Monoid (Maybe a) where 
    mempty = Nothing
    mappend = (<>)


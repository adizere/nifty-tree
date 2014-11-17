-- When you do a qualified import, type constructors also have to be preceeded
-- with a module name. So you'd write:
-- type IntMap = Map.Map Int
import qualified Data.Map as Map

-- Type synonims:
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]


-- inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool


type AssocList k v = [(k, v)]


type IntMap v = Map.Map Int v -- equivalent with:
-- type IntMap = Map Int


-- actual new types defined here:
-- data Either = Left a | Right b
--     deriving (Eq, Ord, Read, Show)


--
-- high-school lockers example:
--

-- LockerState is a data type representing whether a locker is taken or free
data LockerState = Taken | Free deriving (Eq, Show)

-- Code is synonim for a locker's code
type Code = String

-- LockerMap is a synonim that maps from Int to pairs of locker state and code
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker number " ++ show lockerNumber ++ " is taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

main = do
    let s = lockerLookup 101 lockers
    print s


--
-- recursive data types
--

-- data List a = Empty | Cons a (List a)
--     deriving (Eq, Show, Read, Ord)
-- the same definition in record syntax:
-- data List a = Empty | Cons { listHead :: a, listTail :: List a }
--     deriving (Eq, Show, Read, Ord)

-- main =
--     print Empty
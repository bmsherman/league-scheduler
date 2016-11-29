module Schedule where
import Control.Monad (forM_, forM, when)
import Data.Function (on)
import Data.List ((\\), partition, sortBy)
import GHC.Exts (sortWith)
import qualified Data.Map as M

type Week = Int

data Player = Player
  { name :: String
  , byeRequests :: [Week]
  } deriving (Eq, Show, Ord)

data ScheduleParams = ScheduleParams
  { players :: [Player]
  , numWeeksP :: Week
  , byesPerWeek :: Week
  }

type ScheduleMap = M.Map (Week, Player) (Maybe Player)

data Schedule = Schedule
  { numWeeks :: Week
  , scheduleMap :: ScheduleMap
  }

byesSoFar :: Schedule -> Player -> [Week]
byesSoFar s p = reverse [ i | i <- [1 .. numWeeks s]
    , Just Nothing <- [M.lookup (i, p) (scheduleMap s)] ]


nextWeek :: Schedule -> Week
nextWeek s = numWeeks s + 1

determineByes :: ScheduleParams -> Schedule -> ([Player], [Player])
determineByes sp s = splitAt (byesPerWeek sp)
  (necessaryByes ++ nextByes)
  where
  ps = players sp
  (necessaryByes, others) = partition 
     (\x -> nextWeek s `elem` byeRequests x) ps
  nextByes = sortBy (mostThenLatest `on` byesSoFar s) others

-- Lists of weeks should be sorted in descending order
mostThenLatest :: [Week] -> [Week] -> Ordering
mostThenLatest w1  w2 = case compare (length w1) (length w2) of
  EQ -> case w1 of 
    [] -> EQ
    x : xs -> compare x (head w2)
  other -> other

whoToPlay :: Schedule -> Player -> [Player] -> Player
whoToPlay s me opponents = 
  head (sortBy (mostThenLatest `on` weeksPlayed) opponents)
  where
  weeksPlayed op = reverse
   [ i | i <- [1 .. numWeeks s]
        , Just (Just p2) <- [M.lookup (i, me) (scheduleMap s)]
        , op == p2
   ]

updateSMWithGames :: [Player] -> Week -> ScheduleMap -> ScheduleMap
updateSMWithGames [] _ = id
updateSMWithGames [x] _ = error "Uneven number of players"
updateSMWithGames (me : xs) i = \sm ->
  let op = whoToPlay (Schedule i sm) me xs in
  updateSMWithGames (xs \\ [op]) i (add me op sm) 
  where
  add p1 p2 = add1 p1 p2 . add1 p2 p1
  add1 :: Player -> Player -> ScheduleMap -> ScheduleMap
  add1 p1 p2 = M.insert (i + 1, p1) (Just p2)

extendSchedule1 :: ScheduleParams -> Schedule -> Schedule
extendSchedule1 sp s = Schedule
  { numWeeks = nw
  , scheduleMap = sm'
  }
  where
  nw = nextWeek s
  sm = scheduleMap s
  smByes = foldr (.) id
    [ M.insert (nw, p) Nothing | p <- byes ] $ sm
  sm' = updateSMWithGames nonbyes (numWeeks s) smByes
  (byes, nonbyes) = determineByes sp s

emptySchedule :: Schedule
emptySchedule = Schedule
  { numWeeks = 0
  , scheduleMap = M.empty
  }

fillSchedule :: ScheduleParams -> Schedule -> Schedule
fillSchedule sp s | numWeeks s == numWeeksP sp = s
fillSchedule sp s | otherwise = fillSchedule sp (extendSchedule1 sp s)

createSchedule :: ScheduleParams -> Schedule
createSchedule sp = fillSchedule sp emptySchedule

showOpponent :: Maybe Player -> String
showOpponent Nothing = "Bye"
showOpponent (Just p) = name p

printSchedule :: Schedule -> IO ()
printSchedule s = forM_ [1 .. numWeeks s] $ \w -> do
  putStrLn ("Week " ++ show w)
  putStrLn ("-------")
  forM_ (M.toList (scheduleMap s)) $ \((i, p), op) ->
    when (i == w) $
      putStrLn (name p ++ "\tvs.\t" ++ showOpponent op)
  putStrLn ""

data Game = 
    Bye Week String
  | Game Week String String

existingSchedule :: [Player] -> [Game] -> Schedule
existingSchedule ps gs = foldr (.) id [ addGame g | g <- gs ] $ emptySchedule
  where
  get :: String -> Player
  get n = case filter (\p -> n == name p) ps of
    x : _ -> x
    [] -> Player n []
  update :: Week -> Player -> Maybe Player -> Schedule -> Schedule
  update w p op s = Schedule
    { numWeeks = numWeeks s `max` w
    , scheduleMap = M.insert (w, p) op (scheduleMap s)
    }
  addGame :: Game -> Schedule -> Schedule
  addGame (Bye w name) = update w (get name) Nothing
  addGame (Game w p1 p2) = let p1' = get p1 in let p2' = get p2 in 
    update w p1' (Just p2') . update w p2' (Just p1')


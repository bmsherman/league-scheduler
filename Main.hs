module Main where
import Schedule

-- Each line is a player in the league. The list of numbers is the
-- weeks where each player requests to have a bye. If you want to modify an
-- existing schedule without affecting bye weeks, you should make sure
-- that all of those future byes are listed here.
lplayers :: [Player]
lplayers = 
  [ Player "Player 1" [1]
  , Player "Player 2" [2, 6]
  , Player "Player 3" [2, 4]
  , Player "Player 4" [3, 5]
  , Player "Player 5" [1, 3]
  , Player "Player 6" []
  ]

-- The list of games which have already been played. There are two options,
-- Bye and Game. The number argument which comes right after either Bye or
-- Game (in this case, everything is 1) indicates which week it occurred in.
-- If a substitute has played, you can write "substitute" or put in the name
-- of the substitute. As long as the substitute isn't put in the list above,
-- the sub still won't be scheduled for future games.
lgames :: [Game]
lgames = 
  [ Bye 1 "Player 1"
  , Bye 1 "Player 5"
  , Bye 1 "Player 4"
  , Game 1 "Player 2" "Substitute"
  , Game 1 "Player 3" "Player 6"
  ]

-- The parameters for scheduling. `players` is which players to schedule,
-- `numWeeksP` is how many weeks long the schedule should be in total,
-- and `byesPerWeek` is how many players should be given byes each week.
schedParams :: ScheduleParams
schedParams = ScheduleParams
  { players = lplayers
  , numWeeksP = 6
  , byesPerWeek = 2
  }

-- This program creates the partial schedule from the list of games we
-- provided, then fills out the rest of the schedule, and prints it.
main :: IO ()
main = do
  printSchedule s
  putStrLn "" >> putStrLn ""
  printScheduleCode lplayers s
  where 
  s = fillSchedule schedParams (existingSchedule lplayers lgames)

module Example1 where
import Schedule

{- The i'th player requests a bye on the
   i'th week -}
eplayers :: Week -> Int -> Player
eplayers nweeks i = Player
  { name = "Player " ++ show i
  , byeRequests = [ i | i <= nweeks ]
  }

eparams :: Int -> ScheduleParams
eparams nplayers = ScheduleParams
  { players = map (eplayers 8) [1 .. nplayers]
  , numWeeksP = 8
  , byesPerWeek = 2
  }

main :: IO ()
main = printSchedule (createSchedule (eparams 8))

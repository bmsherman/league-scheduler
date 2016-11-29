League scheduler
==========================

Create schedules for weekly leagues where games consist of head-to-head
matchups, and where some players get byes each week. Players are
allowed to have specific weeks where they request to have byes, but
beyond this scheduling is done to ensure that byes are evenly distributed
and that players have variety in the opponents they face (it should roughly
look like round robin scheduling).

See example in `Main.hs`.

```
  ghc Main.hs
  ./Main
```

Output:
```
Week 1
-------
Player 1	vs.	Bye
Player 2	vs.	Substitute
Player 3	vs.	Player 6
Player 4	vs.	Bye
Player 5	vs.	Bye
Player 6	vs.	Player 3
Substitute	vs.	Player 2

Week 2
-------
Player 1	vs.	Player 6
Player 2	vs.	Bye
Player 3	vs.	Bye
Player 4	vs.	Player 5
Player 5	vs.	Player 4
Player 6	vs.	Player 1

Week 3
-------
Player 1	vs.	Player 3
Player 2	vs.	Player 6
Player 3	vs.	Player 1
Player 4	vs.	Bye
Player 5	vs.	Bye
Player 6	vs.	Player 2

Week 4
-------
Player 1	vs.	Player 2
Player 2	vs.	Player 1
Player 3	vs.	Bye
Player 4	vs.	Player 5
Player 5	vs.	Player 4
Player 6	vs.	Bye

Week 5
-------
Player 1	vs.	Bye
Player 2	vs.	Player 5
Player 3	vs.	Player 6
Player 4	vs.	Bye
Player 5	vs.	Player 2
Player 6	vs.	Player 3

Week 6
-------
Player 1	vs.	Player 4
Player 2	vs.	Bye
Player 3	vs.	Player 5
Player 4	vs.	Player 1
Player 5	vs.	Player 3
Player 6	vs.	Bye
```

-- Rock, Paper, Scissors
-- Create an RPS game with an AI that can implement different strategies
-- start by modeling the data we want to work on
data Move = Rock | Paper | Scissors
  deriving (Show, Eq)

-- given a list of moves so far, come up with a new move
type Strategy = [Move] -> Move

--
-- Simple Strategies
--

copyCat :: Strategy
copyCat [] = Rock
copyCat (lastMove : _) = lastMove

cycleMoves :: Strategy
cycleMoves [] = Rock
cycleMoves moves = case length moves `mod` 3 of
  0 -> Rock
  1 -> Paper
  2 -> Scissors

alwaysRock :: Strategy
alwaysRock _ = Rock

--
-- Combinators: Fundamental Strategies
--

alternate :: Strategy -> Strategy -> Strategy
alternate strat1 strat2 moves = case length moves `mod` 2 of
  0 -> strat1 moves
  1 -> strat2 moves

switchUp :: Strategy -> Strategy
switchUp strat moves = case strat moves of
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

switchDown :: Strategy -> Strategy
switchDown strat = switchUp (switchUp strat)

--
-- Complex Strategies
--

complexStrategy :: Strategy
complexStrategy = switchUp copyCat `alternate` switchDown cycleMoves

playGame :: Strategy -> [Move] -> IO ()
playGame strategy moves =
  do
    putStr "Enter Move (Rock, Paper, Scissors, Quit): "
    input <- getLine
    putStrLn $ "AI Move: " ++ show (strategy moves)
    case input of
      "Rock" -> playGame strategy (Rock : moves)
      "Paper" -> playGame strategy (Paper : moves)
      "Scissors" -> playGame strategy (Scissors : moves)
      _ -> return ()

main :: IO ()
main =
  do
    playGame complexStrategy []
    putStrLn "Game over!"
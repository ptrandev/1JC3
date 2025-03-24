-- Rock, Paper, Scissors
-- Create an RPS game with an AI that can implement different strategies

data Move = Rock | Paper | Scissors
  deriving (Show, Eq)

-- define a function that, give a list of moves, produces the move to make
type Strategy = [Move] -> Move

--
-- Simple Strategies
--

copyCat :: Strategy
copyCat [] = Rock
copyCat (move:_) = move

alwaysRock :: Strategy
alwaysRock _ = Rock

cycleMoves :: Strategy
cycleMoves [] = Rock
cycleMoves moves = case length moves `mod` 3 of
  0 -> Rock
  1 -> Paper
  2 -> Scissors

--
-- Combinators: Funadmental Stategies
--
alternate :: Strategy -> Strategy -> Strategy
alternate str1 str2 moves = case length moves `mod` 2 of
  0 -> str1 moves
  1 -> str2 moves

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
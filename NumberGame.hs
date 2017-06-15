import Game
import Data.List
import Data.Maybe
import Control.Monad

data NumberGame      = NumberGame [Int] Int deriving Show
data NumberSolution  = NumberSolution [Int] [Token]

instance Show NumberSolution where
    show (NumberSolution nums toks) = show_rec (map show nums) toks
        where
            show_rec stack ops = case (stack, ops) of
                (x, [])        -> head x
                (x:y:zs, t:ts) -> show_rec (f x y t : zs) ts
            f x y op = case op of
                ADD -> e
                SUB -> e
                MUL -> "("++ e ++")"
                DIV -> "("++ e ++")"
                where e = intercalate " " [x, show op, y]

data Token = ADD | SUB | MUL | DIV
all_tokens = [ADD, SUB, MUL, DIV]

instance Show Token where
    show token = case token of
        ADD -> "+"
        SUB -> "-"
        MUL -> "*"
        DIV -> "/"

token_op :: Token -> Int -> Int -> Maybe Int
token_op token x y = case token of
	ADD -> Just (x+y)
	SUB -> Just (x-y)
	MUL -> Just (x*y)
	DIV -> if (m == 0) then (Just d) else Nothing
	where (d, m) = divMod x y

eval_token :: [Int] -> Token -> Maybe [Int]
eval_token nums token = case nums of
	x:y:zs -> (:zs) <$> op x y
	_	   -> Nothing
	where op = token_op token

eval_target :: NumberSolution -> Maybe Int
eval_target (NumberSolution nums ops) = head <$> foldM eval_token nums ops

possible_solutions :: NumberGame -> [NumberSolution]
possible_solutions (NumberGame nums target) =
    [NumberSolution ns ops | ns <- permutations nums, ops <- cart_prod]
    where cart_prod = sequence $ replicate (length nums -1) all_tokens

is_solution :: NumberGame -> NumberSolution -> Bool
is_solution (NumberGame nums target) soln = fromMaybe False maybe_target
    where maybe_target = fmap (target==) (eval_target soln)

solve_number :: NumberGame -> Maybe NumberSolution
solve_number game = find (is_solution game) (possible_solutions game)




--

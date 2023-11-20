  -- Do not alter the following line
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Assignment2 (transaction_to_string, trade_report_list, stock_test, get_trades, trade_report, update_money, profit, profit_report, complex_profit_report) where
import Distribution.Compat.Lens (_1)
import Data.IntMap (update)


type Transaction = (Char, Int, Int, String, Int)

test_log :: [Transaction]
test_log = [('B', 100, 1104,  "VTI",  1),
            ('B', 200,   36, "ONEQ",  3),
            ('B',  50, 1223,  "VTI",  5),
            ('S', 150, 1240,  "VTI",  9),
            ('B', 100,  229, "IWRD", 10),
            ('S', 200,   32, "ONEQ", 11),
            ('S', 100,  210, "IWRD", 12)
            ]


-- Part A


transaction_to_string :: Transaction -> String
transaction_to_string (action, unit, price, stock, day)
  | unit == 1 = if action == 'B' then
                  "Bought " ++ show unit ++ " unit of " ++ stock ++ " for " ++ show price ++ " pounds each on day " ++ show day
                else
                   "Sold " ++ show unit ++ " unit of " ++ stock ++ " for " ++ show price ++ " pounds each on day " ++ show day
  | otherwise = if action == 'B' then
                "Bought " ++ show unit ++ " units of " ++ stock ++ " for " ++ show price ++ " pounds each on day " ++ show day
                else
                  "Sold " ++ show unit ++ " units of " ++ stock ++ " for " ++ show price ++ " pounds each on day " ++ show day

trade_report_list :: [Transaction] -> [String]
trade_report_list = map transaction_to_string

stock_test :: String -> Transaction -> Bool
stock_test stock_to_check (_, _, _, stock, _)
  | stock == stock_to_check = True
  | otherwise = False

get_trades :: String -> [Transaction] -> [Transaction]
get_trades stock_to_check = filter (\(_, _, _, stock, _) -> stock == stock_to_check)

trade_report :: String -> [Transaction] -> String
trade_report stock_to_check transaction = unlines (trade_report_list (get_trades stock_to_check transaction))



-- Part B


update_money :: Transaction -> Int -> Int
update_money (action, units, price, _, _) current_money
  | action == 'B' = current_money - (price * units)
  | otherwise = current_money + (price * units)

profit :: [Transaction] -> String -> Int
profit transaction stock_to_check = foldr update_money 0 (get_trades stock_to_check transaction)

profit_report_helper :: [Transaction] -> [Char] -> [Char]
profit_report_helper transaction stock_to_check = stock_to_check ++ ": " ++ show (profit transaction stock_to_check)

profit_report :: [String] -> [Transaction] -> String
profit_report stock_to_check transaction = unlines (map (profit_report_helper transaction) stock_to_check)

-- Part C


test_str_log = "BUY 100 VTI 1\nBUY 200 ONEQ 3\nBUY 50 VTI 5\nSELL 150 VTI 9\nBUY 100 IWRD 10\nSELL 200 ONEQ 11\nSELL 100 IWRD 12\n"



type Prices = [(String, [Int])]

test_prices :: Prices
test_prices = [
                ("VTI", [1689, 1785, 1772, 1765, 1739, 1725, 1615, 1683, 1655, 1725, 1703, 1726, 1725, 1742, 1707, 1688, 1697, 1688, 1675]),
                ("ONEQ", [201, 203, 199, 199, 193, 189, 189, 183, 185, 190, 186, 182, 186, 182, 182, 186, 183, 179, 178]),
                ("IWRD", [207, 211, 213, 221, 221, 222, 221, 218, 226, 234, 229, 229, 228, 222, 218, 223, 222, 218, 214])
              ]



complex_profit_report :: String -> Prices -> String
complex_profit_report = error "Not implemented"



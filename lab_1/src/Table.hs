-- | Plain-text tables for console output.
module Table (renderTable, printTable, printRecords) where

import Classes (Displayable (..))
import Data.List (intercalate, transpose)
import Data.Proxy (Proxy)

maxWidth :: Int
maxWidth = 40

renderTable :: [String] -> [[String]] -> String
renderTable hs rows = unlines ([sep, line hs, sep] ++ map line rows' ++ [sep])
  where
    rows' = map (map clip) rows
    widths = map (maximum . map length) (transpose (hs : rows'))
    line cs = "| " ++ intercalate " | " (zipWith pad widths cs) ++ " |"
    sep = "+" ++ intercalate "+" [replicate (w + 2) '-' | w <- widths] ++ "+"
    pad w s = s ++ replicate (w - length s) ' '
    clip s = if length s > maxWidth then take (maxWidth - 3) s ++ "..." else s

printTable :: [String] -> [[String]] -> IO ()
printTable _ [] = putStrLn "No records."
printTable hs rows = do
  putStr (renderTable hs rows)
  putStrLn ("Rows: " ++ show (length rows))

printRecords :: Displayable a => Proxy a -> [a] -> IO ()
printRecords p xs = printTable (headers p) (map cells xs)

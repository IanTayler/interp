module Main where

import           Data.Maybe
import           System.IO

import qualified Data.Text                          as T

import           Data.Interp.InterTree
import           Data.Interp.InterTree.DirectRunner
import           Data.Interp.Parser
import           Data.Interp.Tokenizer

printTreesAndValues Nothing _ = return ()
printTreesAndValues (Just []) _ = return ()
printTreesAndValues (Just (t:trees)) (Just (v:values)) = do
  putStrLn
    (show t ++ " ==> " ++ show (fst v) ++ " with state: " ++ show (snd v))
  printTreesAndValues (Just trees) (Just values)

evalInOrder :: [InterTree] -> InterState -> [(InterValue, InterState)]
evalInOrder (tree:trees) state =
  (nextVal, nextState) : evalInOrder trees nextState
  where
    (nextVal, nextState) = evalTree tree state

main :: IO ()
main = do
  content <- getContents
  let resultTrees = parse $ T.pack content
  let valuesAndStates =
        if null resultTrees
          then Nothing
          else Just $ evalInOrder (fromJust resultTrees) initState
  printTreesAndValues resultTrees valuesAndStates

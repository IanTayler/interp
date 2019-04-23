module Main where

import           Data.Maybe
import           System.IO

import qualified Data.Text                          as T

import           Data.Interp.InterTree.DirectRunner
import           Data.Interp.Parser
import           Data.Interp.Tokenizer

printTreesAndValues Nothing _ = return ()
printTreesAndValues (Just []) _ = return ()
printTreesAndValues (Just (t:trees)) (Just (v:values)) = do
  putStrLn (show t ++ " ==> " ++ show v)
  printTreesAndValues (Just trees) (Just values)

main :: IO ()
main = do
  content <- getContents
  let resultTrees = parse $ T.pack content
  let values =
        if null resultTrees
          then Nothing
          else Just $
               map (\t -> fst (evalTree t initState)) (fromJust resultTrees)
  printTreesAndValues resultTrees values

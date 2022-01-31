{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Set (fromList)
import qualified Data.Text as T
import HW3.Action (HIO (runHIO), HiPermission (..))
import HW3.Base (HiError, HiValue)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Prettyprinter.Render.Terminal (Color (..), bgColor, bold, color)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStr, outputStrLn,
                                 runInputT)
import System.IO.Error (catchIOError)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  putStrLn "Welcome to hi! Write :q to quit."
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing    -> return ()
        Just ":q"  -> return ()
        Just input -> do
          case parse input of
            Left peb   -> outputStrLn $ errorBundlePretty peb
            Right expr -> do
              res <- liftIO (try (runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime]))
                :: IO (Either SomeException (Either HiError HiValue)))
              case res of
                Left err          -> outputStrLn $ "I/O error: " ++ show err
                Right (Left err)  -> outputStrLn $ "Error: " ++ show err
                Right (Right val) -> outputStrLn $ show $ prettyValue val
          loop

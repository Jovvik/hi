{-# LANGUAGE DerivingVia #-}

module HW3.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import qualified Data.ByteString as B
import Data.Sequence (fromList)
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import HW3.Base (ConvertibleToHiValue (toValue), HiAction (..), HiMonad (..), HiValue (HiValueNull),
                 returnValue)
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random (Random (randomR), getStdRandom)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Show, Ord)

data PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

withPermission :: Foldable t => HiPermission -> IO a -> t HiPermission -> IO a
withPermission perm workload perms = if perm `elem` perms
  then workload
  else throwIO $ PermissionRequired perm

instance HiMonad HIO where
  runAction (HiActionRead path) = HIO
    { runHIO = withPermission AllowRead $ do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          paths <- listDirectory path
          returnValue $ fromList $ map (toValue . T.pack) paths
        else do
          bytes <- B.readFile path
          case decodeUtf8' bytes of
            Left _    -> returnValue bytes
            Right txt -> returnValue txt
    }
  runAction (HiActionWrite path bytes) = HIO
    { runHIO = withPermission AllowWrite $ do
        B.writeFile path bytes
        returnValue HiValueNull
    }
  runAction (HiActionMkDir path) = HIO
    { runHIO = withPermission AllowWrite $ do
      createDirectory path
      returnValue HiValueNull
    }
  runAction (HiActionChDir path) = HIO
    { runHIO = withPermission AllowRead $ do
      setCurrentDirectory path
      returnValue HiValueNull
    }
  runAction HiActionCwd = HIO
    { runHIO = withPermission AllowRead $ do
      dir <- getCurrentDirectory
      returnValue $ T.pack dir
    }
  runAction HiActionNow = HIO
    { runHIO = withPermission AllowTime $ do
      time <- getSystemTime
      returnValue $ systemToUTCTime time
    }
  runAction (HiActionRand start end) = HIO
    { runHIO = \_ -> do
      rand <- getStdRandom (randomR (start, end))
      returnValue rand
    }
  runAction (HiActionEcho text) = HIO
    { runHIO = withPermission AllowWrite $ do
      putStrLn $ T.unpack text
      returnValue HiValueNull
    }

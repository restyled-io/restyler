{-# LANGUAGE UndecidableInstances #-}

module Restyler.Capabilities.DownloadFile.Staged
    ( StagedDownloadFile(..)
    , StagedDownloadFiles
    , stagedDownloadFiles
    , HasStagedDownloadFiles(..)
    , stageDownloadFile
    , stageDownloadFileError
    , stageDownloadFileInvalidURL
    )
where

import Restyler.Prelude

import Control.Lens ((%=))
import Control.Monad.State
import Network.HTTP.Client (HttpException(..))
import Prelude (userError)
import Restyler.App.Error
import Restyler.Capabilities.DownloadFile
import Restyler.Capabilities.System.State
import qualified RIO.HashMap as HashMap

newtype StagedDownloadFile m a = StagedDownloadFile
    { unStagedDownloadFile :: m a
    }
    deriving newtype (Applicative, Functor, Monad, MonadError e, MonadState s)

instance (MonadError AppError m, MonadState env m, HasFS env, HasStagedDownloadFiles env)
    => MonadDownloadFile (StagedDownloadFile m) where
    downloadFile url path = do
        mRemoteFile <- gets
            $ view (stagedDownloadFilesL . unL . to (HashMap.lookup url))

        case mRemoteFile of
            Nothing -> do
                staged <- gets $ view (stagedDownloadFilesL . unL)
                throwError
                    $ OtherError
                    $ toException
                    $ userError
                    $ "The URL "
                    <> show url
                    <> " was requested via DownloadFile, but nothing's been staged."
                    <> "\nURLs currently staged: "
                    <> show staged
            Just (RemoteFile content) -> addNormalFile path content
            Just (RemoteFileError ex) -> throwError $ HttpError ex
            Just RemoteFileInvalidURL ->
                throwError $ HttpError $ InvalidUrlException
                    (unpack url)
                    "Staged to be treated as invalid by DownloadFile."

newtype StagedDownloadFiles = StagedDownloadFiles
    { unStagedDownloadFiles :: HashMap Text RemoteFile
    }

unL :: Lens' StagedDownloadFiles (HashMap Text RemoteFile)
unL = lens unStagedDownloadFiles $ \x y -> x { unStagedDownloadFiles = y }

stagedDownloadFiles :: StagedDownloadFiles
stagedDownloadFiles = StagedDownloadFiles HashMap.empty

class HasStagedDownloadFiles env where
    stagedDownloadFilesL :: Lens' env StagedDownloadFiles

instance HasStagedDownloadFiles StagedDownloadFiles where
    stagedDownloadFilesL = id

data RemoteFile
    = RemoteFile Text
    | RemoteFileError HttpException
    | RemoteFileInvalidURL
    deriving stock Show

stageDownloadFile
    :: (MonadState env m, HasStagedDownloadFiles env) => Text -> Text -> m ()
stageDownloadFile url content =
    stagedDownloadFilesL . unL %= HashMap.insert url (RemoteFile content)

stageDownloadFileError
    :: (MonadState env m, HasStagedDownloadFiles env)
    => Text
    -> HttpException
    -> m ()
stageDownloadFileError url ex =
    stagedDownloadFilesL . unL %= HashMap.insert url (RemoteFileError ex)

stageDownloadFileInvalidURL
    :: (MonadState env m, HasStagedDownloadFiles env) => Text -> m ()
stageDownloadFileInvalidURL url =
    stagedDownloadFilesL . unL %= HashMap.insert url RemoteFileInvalidURL

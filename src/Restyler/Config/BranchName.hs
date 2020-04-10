module Restyler.Config.BranchName
    ( BranchName
    , interpolateBranchName
    )
where

import Restyler.Prelude

import Data.Aeson
import qualified Data.Text as T

newtype BranchName = BranchName
    { interpolateBranchName :: Text -> Text
    }

branchName :: Text -> BranchName
branchName x = BranchName $ \base -> T.replace interpolation base x

asInterpolation :: BranchName -> Text
asInterpolation b = interpolateBranchName b interpolation

instance Eq BranchName where
    (==) = (==) `on` asInterpolation

instance Show BranchName where
    show = show . asInterpolation

instance FromJSON BranchName where
    parseJSON = withText "BranchName" $ \input -> do
        unless (interpolation `T.isInfixOf` input)
            $ fail
            $ "A BranchName should contain the interpolation "
            <> unpack interpolation
            <> " somewhere. Otherwise, every Restyle PR would get the same branch"
            <> " name, which will likely cause problems."
        pure $ branchName input

instance ToJSON BranchName where
    toJSON = String . asInterpolation

interpolation :: Text
interpolation = "{base}"

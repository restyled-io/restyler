module SpecHelper
    ( module X
    , module SpecHelper
    ) where

import Restyler.Prelude as X
import Test.Hspec as X
    hiding
        ( expectationFailure
        , shouldBe
        , shouldContain
        , shouldEndWith
        , shouldMatchList
        , shouldNotBe
        , shouldNotContain
        , shouldNotReturn
        , shouldNotSatisfy
        , shouldReturn
        , shouldSatisfy
        , shouldStartWith
        )
import Test.Hspec.Expectations.Lifted as X
import Test.QuickCheck as X

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Restyler.App.Error
import Restyler.Capabilities.DownloadFile
import Restyler.Capabilities.DownloadFile.Staged
import Restyler.Capabilities.Logger
import Restyler.Capabilities.System
import Restyler.Config
import Restyler.Restyler

loadDefaultConfig
    :: ( MonadLogger m
       , MonadSystem m
       , MonadDownloadFile m
       , MonadError AppError m
       , MonadState env m
       , HasStagedDownloadFiles env
       )
    => m Config
loadDefaultConfig = do
    stageManifest "stable" testRestylers
    loadConfigContent defaultConfigContent

loadConfigText
    :: ( MonadLogger m
       , MonadSystem m
       , MonadDownloadFile m
       , MonadError AppError m
       )
    => Text
    -> m Config
loadConfigText = loadConfigContent . encodeUtf8 . dedent

loadConfigContent
    :: ( MonadLogger m
       , MonadSystem m
       , MonadDownloadFile m
       , MonadError AppError m
       )
    => ByteString
    -> m Config
loadConfigContent = loadConfigFrom . pure . ConfigContent

stageManifest
    :: (MonadState env m, HasStagedDownloadFiles env)
    => Text
    -> [Restyler]
    -> m ()
stageManifest channel =
    stageDownloadFile (manifestUrl channel) . decodeUtf8 . Yaml.encode

manifestUrl :: Text -> Text
manifestUrl channel =
    "https://docs.restyled.io/data-files/restylers/manifests/"
        <> channel
        <> "/restylers.yaml"

testRestylers :: [Restyler]
testRestylers =
    [ someRestyler { rName = "astyle" }
    , someRestyler { rName = "autopep8" }
    , someRestyler { rName = "black" }
    , someRestyler { rName = "dfmt" }
    , someRestyler { rName = "elm-format" }
    , someRestyler { rName = "hindent", rEnabled = False }
    , someRestyler { rName = "jdt", rEnabled = False }
    , someRestyler { rName = "pg_format" }
    , someRestyler { rName = "php-cs-fixer" }
    , someRestyler { rName = "prettier" }
    , someRestyler { rName = "prettier-markdown" }
    , someRestyler { rName = "prettier-ruby" }
    , someRestyler { rName = "prettier-yaml" }
    , someRestyler { rName = "reorder-python-imports" }
    , someRestyler { rName = "rubocop" }
    , someRestyler { rName = "rustfmt" }
    , someRestyler { rName = "shellharden" }
    , someRestyler { rName = "shfmt" }
    , someRestyler { rName = "stylish-haskell" }
    , someRestyler { rName = "terraform" }
    , someRestyler { rName = "yapf" }
    ]

someRestyler :: Restyler
someRestyler = Restyler
    { rEnabled = True
    , rName = "test-restyler"
    , rImage = "restyled/restyler-test-restyler"
    , rCommand = ["restyle"]
    , rDocumentation = []
    , rArguments = []
    , rInclude = ["**/*"]
    , rInterpreters = []
    , rDelimiters = Nothing
    , rSupportsArgSep = True
    , rSupportsMultiplePaths = True
    }

dedent :: Text -> Text
dedent x = T.unlines $ map (T.drop indent) ls
  where
    ls = T.lines $ T.dropWhileEnd isSpace $ T.dropWhile (== '\n') x
    indent = fromMaybe 0 $ minimumMaybe indents
    indents = map (T.length . T.takeWhile (== ' ')) ls

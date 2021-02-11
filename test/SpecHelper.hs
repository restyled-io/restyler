module SpecHelper
    ( module X
    , module SpecHelper
    )
where

import Restyler.Prelude as X
import Test.Hspec as X hiding
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
import qualified Data.Yaml as Yaml
import Restyler.App.Error
import Restyler.Capabilities.DownloadFile
import Restyler.Capabilities.DownloadFile.Staged
import Restyler.Capabilities.Logger
import Restyler.Capabilities.System
import Restyler.Config
import Restyler.Restyler

tryError :: MonadError e m => m a -> m (Either e a)
tryError f = (Right <$> f) `catchError` (pure . Left)

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
    loadConfigFrom [ConfigContent defaultConfigContent]

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

module SpecHelper
    ( someRestyler

    -- * Config
    , loadDefaultConfig
    , testRestylers

    -- * Re-exports
    , module X
    ) where

import Restyler.Prelude as X hiding
    (readFileBinary, readFileUtf8, writeFileUtf8)
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

import Data.Yaml (decodeThrow)
import Restyler.Config
import Restyler.Restyler

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

loadDefaultConfig :: RIO env Config
loadDefaultConfig = do
    config <- decodeThrow defaultConfigContent
    resolveRestylers config testRestylers

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

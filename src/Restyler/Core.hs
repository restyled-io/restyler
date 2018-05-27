{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Core
    ( restyle
    )
where

import Restyler.Prelude

restyle :: [Restyler] -> [FilePath] -> AppM ()
restyle restylers allPaths = do
    cwd <- getCurrentDirectory

    logDebugN $ "All paths: " <> tshow allPaths
    logDebugN $ "Current working directory: " <> pack cwd
    logDebugN $ "Restylers: " <> tshow (map rName restylers)

    for_ restylers $ \r@Restyler {..} -> do
        paths <- filterM (shouldRestyle r) allPaths

        unless (null paths) $ do
            logInfoN $ "Restyling " <> tshow paths <> " via " <> pack rName
            callProcess "docker" $ dockerArguments cwd r paths

shouldRestyle :: Restyler -> FilePath -> AppM Bool
shouldRestyle restyler path =
    (&&) <$> doesFileExist path <*> shouldInclude restyler path

shouldInclude :: Restyler -> FilePath -> AppM Bool
shouldInclude Restyler {..} path =
    liftIOApp
        $ (includePath rInclude path ||)
        <$> anyM (path `hasInterpreter`) rInterpreters

dockerArguments :: FilePath -> Restyler -> [FilePath] -> [String]
dockerArguments dir Restyler {..} paths =
    [ "run"
        , "--rm"
        , "--volume"
        , dir <> ":/code"
        , "--net"
        , "none"
        , "restyled/restyler-" <> rName
        , rCommand
        ]
        ++ rArguments
        ++ prependArgSep (map prependIfRelative paths)
  where
    prependArgSep
        | rSupportsArgSep = ("--" :)
        | otherwise = id

    prependIfRelative path
        | any (`isPrefixOf` path) ["/", "./", "../"] = path
        | otherwise = "./" <> path

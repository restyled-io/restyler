{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Core
    ( restyle
    )
where

import Restyler.Prelude

-- | Run the main restyling process
restyle :: [Restyler] -> [FilePath] -> AppM ()
restyle restylers allPaths = do
    cwd <- getCurrentDirectory
    existingPaths <- filterM doesFileExist allPaths

    logDebugN $ "Restylers: " <> tshow (map rName restylers)
    logDebugN $ "Paths: " <> tshow existingPaths

    for_ restylers $ \r@Restyler {..} -> do
        paths <- filterM (shouldInclude r) existingPaths

        unless (null paths) $ do
            logInfoN $ "Restyling " <> tshow paths <> " via " <> pack rName
            callProcess "docker" $ dockerArguments cwd r paths

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

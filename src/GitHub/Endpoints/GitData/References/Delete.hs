-- | <https://developer.github.com/v3/git/refs/#delete-a-reference>
module GitHub.Endpoints.GitData.References.Delete
    ( deleteReference
    , deleteReferenceR
    )
where

import Prelude

import GitHub.Data
import GitHub.Request

deleteReference
    :: Auth
    -> Name Owner
    -> Name Repo
    -> Name GitReference
    -> IO (Either Error ())
deleteReference auth user repo ref =
    executeRequest auth $ deleteReferenceR user repo ref

deleteReferenceR
    :: Name Owner
    -> Name Repo
    -> Name GitReference
    -> GenRequest 'MtUnit 'RW ()
deleteReferenceR user repo ref = Command Delete path mempty
  where
    path =
        [ "repos"
        , toPathPart user
        , toPathPart repo
        , "git"
        , "refs"
        , toPathPart ref
        ]

module TahoeLAFS.Storage.Client (
    version,
    createImmutableStorageIndex,
    writeImmutableShare,
    adviseCorruptImmutableShare,
    getImmutableShareNumbers,
    readImmutableShares,
    createMutableStorageIndex,
    readvAndTestvAndWritev,
    readMutableShares,
    getMutableShareNumbers,
    adviseCorruptMutableShare,
    renewLease,
) where

import Servant (
    (:<|>) (..),
 )
import Servant.Client (
    client,
 )

import TahoeLAFS.Storage.API (
    api,
 )

( version
        :<|> renewLease
        :<|> createImmutableStorageIndex
        :<|> writeImmutableShare
        :<|> adviseCorruptImmutableShare
        :<|> getImmutableShareNumbers
        :<|> readImmutableShares
        :<|> createMutableStorageIndex
        :<|> readvAndTestvAndWritev
        :<|> readMutableShares
        :<|> getMutableShareNumbers
        :<|> adviseCorruptMutableShare
    ) = client api

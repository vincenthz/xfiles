{-# LANGUAGE OverloadedStrings #-}
module Tools.ChronoFs.Config where

import Data.FileFormat (FileFormat(..))

defaultBadFileformats :: [FileFormat]
defaultBadFileformats =
    [ FT_ELF
    , FT_VHD
    , FT_VHDX
    , FT_QEMU_QCOW
    , FT_AR
    , FT_CAML_CMI
    , FT_Haskell_Interface
    , FT_Python_Compiled
    , FT_JavaClass_Compiled
    ]

backupDirName :: String
backupDirName = ".chronofs"

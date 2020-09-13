module Windows.Flags.ExWindowStyle where

import Windows.Types (DWORD)

empty, appWindow, noRedirectionBitmap :: DWORD
empty = 0
appWindow = 0x00040000
noRedirectionBitmap = 0x00200000

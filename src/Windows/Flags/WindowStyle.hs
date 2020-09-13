module Windows.Flags.WindowStyle where

import Windows.Types (DWORD)
import Data.Bits ((.|.))

empty, border, caption, maximizeBox, minimizeBox, sysMenu, overlapped, thickFrame, visible :: DWORD
empty = 0
overlapped = 0x00000000
border = 0x00800000
caption = 0x00c00000
maximizeBox = 0x00010000
minimizeBox = 0x00020000
sysMenu = 0x00080000
thickFrame = 0x00040000
visible = 0x10000000

-- composited
overlappedWindow :: DWORD
overlappedWindow = overlapped .|. caption .|. sysMenu .|. thickFrame .|. minimizeBox .|. maximizeBox

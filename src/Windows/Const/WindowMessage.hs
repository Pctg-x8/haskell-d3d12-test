module Windows.Const.WindowMessage where

import Foreign.C.Types (CUInt)

destroy, quit :: CUInt
destroy = 0x0002
quit = 0x0012

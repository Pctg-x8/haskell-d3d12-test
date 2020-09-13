module Windows.Const.HResult where

import Windows.Types (HRESULT)

isSucceeded :: HRESULT -> Bool
isSucceeded x = x >= 0

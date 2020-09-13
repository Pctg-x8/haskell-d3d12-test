{-# LANGUAGE TypeFamilies #-}

module Windows.ComBase where

import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr)
import Foreign.Storable (peek)
import Windows.Struct.GUID (GUID(..))
import Windows.Types (HRESULT)
import Foreign.C.Types (CLong(..))

class ComInterface a where
  type VTable a

  guid :: Ptr a -> GUID

  vtable :: Ptr a -> IO (Ptr (VTable a))
  getFunctionPtr :: Int -> Ptr a -> IO (FunPtr f)
  vtable = peek . castPtr
  getFunctionPtr index this = vtable this >>= peek . flip plusPtr (index * 8)

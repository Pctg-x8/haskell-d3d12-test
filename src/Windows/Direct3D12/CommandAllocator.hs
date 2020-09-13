{-# LANGUAGE TypeFamilies #-}

module Windows.Direct3D12.CommandAllocator (ID3D12CommandAllocator) where

import Windows.Types (HRESULT)
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr)
import Foreign.C.Types (CLong(..))

data ID3D12CommandAllocatorVtbl
newtype ID3D12CommandAllocator = ID3D12CommandAllocator (Ptr ID3D12CommandAllocatorVtbl)
instance Storable ID3D12CommandAllocator where
  sizeOf _ = 8
  alignment _ = 8
  peek p = ID3D12CommandAllocator <$> peek (castPtr p)
  poke p (ID3D12CommandAllocator vp) = castPtr p `poke` vp
instance ComInterface ID3D12CommandAllocator where
  type VTable ID3D12CommandAllocator = ID3D12CommandAllocatorVtbl
  guid _ = GUID 0x6102dee4 0xaf59 0x4b09 0xb999b44d73f09b24

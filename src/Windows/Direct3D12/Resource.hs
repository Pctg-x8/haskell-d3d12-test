{-# LANGUAGE TypeFamilies #-}

module Windows.Direct3D12.Resource (ID3D12Resource) where

import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))

data ID3D12ResourceVtbl
newtype ID3D12Resource = ID3D12Resource (Ptr ID3D12ResourceVtbl)
instance Storable ID3D12Resource where
  sizeOf _ = 8
  alignment _ = 8
  peek p = ID3D12Resource <$> peek (castPtr p)
  poke p (ID3D12Resource v) = castPtr p `poke` v
instance ComInterface ID3D12Resource where
  type VTable ID3D12Resource = ID3D12ResourceVtbl
  guid _ = GUID 0x696442be 0xa72e 0x4059 0xbc795b5c98040fad

{-# LANGUAGE TypeFamilies #-}

module Windows.Dxgi.Adapter where

import Windows.ComBase (ComInterface(..))
import Windows.Struct.GUID (GUID(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CULong)
import qualified Windows.Com.IUnknown as UnkBase
import Windows.Types (HRESULT)

data IDXGIAdapterVtbl
data IDXGIAdapter = IDXGIAdapter (Ptr IDXGIAdapterVtbl)
instance ComInterface IDXGIAdapter where
  type VTable IDXGIAdapter = IDXGIAdapterVtbl
  guid _ = GUID 0x2411e7e1 0x12ac 0x4ccf 0xbd149798e8534dc0
instance Storable IDXGIAdapter where
  sizeOf _ = 8
  alignment _ = 8
  peek p = IDXGIAdapter <$> peek (castPtr p)
  poke p (IDXGIAdapter v) = poke (castPtr p) v


-- reexports and autocast
addRef, release :: Ptr IDXGIAdapter -> IO CULong
queryInterface :: (ComInterface i, Storable i) => Ptr IDXGIAdapter -> IO (Either HRESULT (Ptr i))
addRef = UnkBase.addRef . castPtr
release = UnkBase.addRef . castPtr
queryInterface = UnkBase.queryInterface . castPtr

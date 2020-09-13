{-# LANGUAGE TypeFamilies #-}

module Windows.Dxgi.Factory2 where

import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Windows.Types
import Windows.Struct.GUID (GUID(..))
import Windows.LibLoader (Lib, getProcAddress)
import qualified Windows.Const.HResult as HR
import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr, nullPtr)
import Foreign.C.Types (CLong(..), CULong, CUInt(..))
import Foreign.Marshal.Alloc (alloca)
import qualified Windows.Com.IUnknown as UnkBase
import Windows.Dxgi.Adapter (IDXGIAdapter)
import Windows.Dxgi.SwapChain3 (IDXGISwapChain3)
import qualified Windows.Struct.Dxgi as DxgiStruct

data IDXGIFactory2Vtbl
data IDXGIFactory2 = IDXGIFactory2 (Ptr IDXGIFactory2Vtbl)
instance ComInterface IDXGIFactory2 where
  type VTable IDXGIFactory2 = IDXGIFactory2Vtbl
  guid _ = GUID 0x770aae78 0xf26f 0x4dba 0xa829253c83d1b387
instance Storable IDXGIFactory2 where
  sizeOf _ = 8
  alignment _ = 8
  peek p = IDXGIFactory2 <$> peek (castPtr p)
  poke p (IDXGIFactory2 vptr) = flip poke vptr $ castPtr p

type PFN_CreateDXGIFactory2 = FunPtr (CUInt -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT)
foreign import ccall "dynamic" createDXGIFactory2Caller :: PFN_CreateDXGIFactory2 -> CUInt -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
_DXGI_CREATE_FACTORY_DEBUG = 0x01
createDxgiFactory2 :: (ComInterface i, Storable i) => Lib -> Bool -> IO (Either HRESULT (Ptr i))
createDxgiFactory2 lib debug =
  alloca $ \ptr ->
  alloca $ \refiid -> do
    fn <- getProcAddress "CreateDXGIFactory2" lib
    peek ptr >>= poke refiid . guid
    hr <- createDXGIFactory2Caller fn (fromIntegral $ if debug then _DXGI_CREATE_FACTORY_DEBUG else 0) refiid (castPtr ptr)
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr

-- reexports and autocast
addRef, release :: Ptr IDXGIFactory2 -> IO CULong
queryInterface :: (ComInterface i, Storable i) => Ptr IDXGIFactory2 -> IO (Either HRESULT (Ptr i))
addRef = UnkBase.addRef . castPtr
release = UnkBase.addRef . castPtr
queryInterface = UnkBase.queryInterface . castPtr

foreign import ccall "dynamic" dcall_enumAdapters :: FunPtr (Ptr IDXGIFactory2 -> CUInt -> Ptr (Ptr ()) -> IO HRESULT) -> Ptr IDXGIFactory2 -> CUInt -> Ptr (Ptr ()) -> IO HRESULT

_VTBL_INDEX_ENUM_ADAPTERS = 7
enumAdapters :: Ptr IDXGIFactory2 -> CUInt -> IO (Either HRESULT (Ptr IDXGIAdapter))
enumAdapters this adapter = alloca $ \ptr -> do
  fn <- getFunctionPtr _VTBL_INDEX_ENUM_ADAPTERS this
  hr <- dcall_enumAdapters fn this adapter (castPtr ptr)
  if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr

type PFN_CreateSwapChainForComposition = Ptr IDXGIFactory2 -> Ptr UnkBase.IUnknown -> Ptr DxgiStruct.SwapChainDesc1 -> Ptr () -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createSwapChainForComposition :: FunPtr PFN_CreateSwapChainForComposition -> PFN_CreateSwapChainForComposition
_VTBL_INDEX_CREATE_SWAP_CHAIN_FOR_COMPOSITION = 24
createSwapChainForComposition :: Ptr IDXGIFactory2 -> Ptr d -> DxgiStruct.SwapChainDesc1 -> IO (Either HRESULT (Ptr IDXGISwapChain3))
createSwapChainForComposition this device desc =
  alloca $ \ptr ->
  alloca $ \descref -> do
    fn <- dcall_createSwapChainForComposition <$> getFunctionPtr _VTBL_INDEX_CREATE_SWAP_CHAIN_FOR_COMPOSITION this
    poke descref desc
    hr <- fn this (castPtr device) descref nullPtr $ castPtr ptr
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr
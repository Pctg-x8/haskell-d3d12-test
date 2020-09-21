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
import qualified Foreign.Marshal.Utils as Marshal
import qualified Windows.Com.IUnknown as UnkBase
import Windows.Dxgi.Adapter (IDXGIAdapter)
import Windows.Dxgi.SwapChain3 (IDXGISwapChain3)
import qualified Windows.Struct.Dxgi as DxgiStruct
import Windows.Com.Monad (ComT, comT, runComT, handleHRESULT)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)

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
createDxgiFactory2 :: (ComInterface i, Storable i) => Lib -> Bool -> ComT IO (Ptr i)
createDxgiFactory2 lib debug = flip runContT pure $ do
  ptr <- ContT $ \f -> comT $ alloca $ runComT . f
  refiid <- ContT $ \f -> comT $ peek ptr >>= flip Marshal.with (runComT . f) . guid
  fn <- liftIO $ createDXGIFactory2Caller <$> getProcAddress "CreateDXGIFactory2" lib
  hr <- liftIO $ fn (fromIntegral $ if debug then _DXGI_CREATE_FACTORY_DEBUG else 0) refiid $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

-- reexports and autocast
addRef, release :: Ptr IDXGIFactory2 -> IO CULong
queryInterface :: (ComInterface i, Storable i) => Ptr IDXGIFactory2 -> IO (Either HRESULT (Ptr i))
addRef = UnkBase.addRef . castPtr
release = UnkBase.addRef . castPtr
queryInterface = UnkBase.queryInterface . castPtr

foreign import ccall "dynamic" dcall_enumAdapters :: FunPtr (Ptr IDXGIFactory2 -> CUInt -> Ptr (Ptr ()) -> IO HRESULT) -> Ptr IDXGIFactory2 -> CUInt -> Ptr (Ptr ()) -> IO HRESULT

_VTBL_INDEX_ENUM_ADAPTERS = 7
enumAdapters :: Ptr IDXGIFactory2 -> CUInt -> ComT IO (Ptr IDXGIAdapter)
enumAdapters this adapter = flip runContT pure $ do
  ptr <- ContT $ \f -> comT $ alloca $ runComT . f
  fn <- liftIO $ dcall_enumAdapters <$> getFunctionPtr _VTBL_INDEX_ENUM_ADAPTERS this
  hr <- liftIO $ fn this adapter $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

type PFN_CreateSwapChainForComposition = Ptr IDXGIFactory2 -> Ptr UnkBase.IUnknown -> Ptr DxgiStruct.SwapChainDesc1 -> Ptr () -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createSwapChainForComposition :: FunPtr PFN_CreateSwapChainForComposition -> PFN_CreateSwapChainForComposition
_VTBL_INDEX_CREATE_SWAP_CHAIN_FOR_COMPOSITION = 24
createSwapChainForComposition :: Ptr IDXGIFactory2 -> Ptr d -> DxgiStruct.SwapChainDesc1 -> ComT IO (Ptr IDXGISwapChain3)
createSwapChainForComposition this device desc = flip runContT pure $ do
  ptr <- ContT $ \f -> comT $ alloca $ runComT . f
  descref <- ContT $ \f -> comT $ Marshal.with desc $ runComT . f
  fn <- liftIO $ dcall_createSwapChainForComposition <$> getFunctionPtr _VTBL_INDEX_CREATE_SWAP_CHAIN_FOR_COMPOSITION this
  hr <- liftIO $ fn this (castPtr device) descref nullPtr $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)
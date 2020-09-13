{-# LANGUAGE TypeFamilies #-}

module Windows.Dxgi.SwapChain3 (IDXGISwapChain3, present, getBuffer, getCurrentBackBufferIndex) where

import Windows.Struct.GUID (GUID(..))
import Windows.Types (HRESULT)
import qualified Windows.Const.HResult as HR
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr)
import Foreign.C.Types (CUInt(..), CLong(..))
import Foreign.Marshal.Alloc (alloca)

data IDXGISwapChain3Vtbl
data IDXGISwapChain3 = IDXGISwapChain3 (Ptr IDXGISwapChain3Vtbl)
instance ComInterface IDXGISwapChain3 where
  type VTable IDXGISwapChain3 = IDXGISwapChain3Vtbl
  guid _ = GUID 0x94d99bdb 0xf1f8 0x4ab0 0xb2367da0170edab1
instance Storable IDXGISwapChain3 where
  sizeOf _ = 8
  alignment _ = 8
  peek p = IDXGISwapChain3 <$> peek (castPtr p)
  poke p (IDXGISwapChain3 vp) = castPtr p `poke` vp

_VTBL_INDEX_PRESENT = 8
type PFN_Present = Ptr IDXGISwapChain3 -> CUInt -> CUInt -> IO HRESULT
foreign import ccall "dynamic" dcall_present :: FunPtr PFN_Present -> PFN_Present
present :: Ptr IDXGISwapChain3 -> Int -> Int -> IO (Either HRESULT ())
present this syncInterval flags = do
  hr <- getFunctionPtr _VTBL_INDEX_PRESENT this >>= \f -> dcall_present f this (fromIntegral syncInterval) (fromIntegral flags)
  pure $ if HR.isSucceeded hr then Right () else Left hr

_VTBL_INDEX_GET_BUFFER = 9
type PFN_GetBuffer = Ptr IDXGISwapChain3 -> CUInt -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_getBuffer :: FunPtr PFN_GetBuffer -> PFN_GetBuffer
getBuffer :: ComInterface r => Ptr IDXGISwapChain3 -> Int -> IO (Either HRESULT (Ptr r))
getBuffer this index =
  alloca $ \ptr ->
  alloca $ \refiid -> do
    fn <- dcall_getBuffer <$> getFunctionPtr _VTBL_INDEX_GET_BUFFER this
    peek ptr >>= poke refiid . guid
    hr <- fn this (fromIntegral index) refiid $ castPtr ptr
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr

_VTBL_INDEX_GET_CURRENT_BACK_BUFFER_INDEX = 36
type PFN_GetCurrentBackBufferIndex = Ptr IDXGISwapChain3 -> IO CUInt
foreign import ccall "dynamic" dcall_getCurrentBackBufferIndex :: FunPtr PFN_GetCurrentBackBufferIndex -> PFN_GetCurrentBackBufferIndex
getCurrentBackBufferIndex :: Ptr IDXGISwapChain3 -> IO Int
getCurrentBackBufferIndex this = fromIntegral <$> (getFunctionPtr _VTBL_INDEX_GET_CURRENT_BACK_BUFFER_INDEX this >>= flip dcall_getCurrentBackBufferIndex this)

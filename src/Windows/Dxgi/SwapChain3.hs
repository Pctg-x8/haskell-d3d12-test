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
import qualified Foreign.Marshal.Utils as Marshal
import Windows.Com.Monad (ComT, comT, runComT, handleHRESULT)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)

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
present :: Ptr IDXGISwapChain3 -> Int -> Int -> ComT IO ()
present this syncInterval flags =
  liftIO (getFunctionPtr _VTBL_INDEX_PRESENT this >>= \f -> dcall_present f this (fromIntegral syncInterval) (fromIntegral flags)) >>= handleHRESULT

_VTBL_INDEX_GET_BUFFER = 9
type PFN_GetBuffer = Ptr IDXGISwapChain3 -> CUInt -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_getBuffer :: FunPtr PFN_GetBuffer -> PFN_GetBuffer
getBuffer :: ComInterface r => Ptr IDXGISwapChain3 -> Int -> ComT IO (Ptr r)
getBuffer this index = flip runContT pure $ do
  ptr <- ContT $ \f -> comT $ alloca $ runComT . f
  refiid <- ContT $ \f -> comT $ peek ptr >>= flip Marshal.with (runComT . f) . guid
  fn <- liftIO $ dcall_getBuffer <$> getFunctionPtr _VTBL_INDEX_GET_BUFFER this
  hr <- liftIO $ fn this (fromIntegral index) refiid $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

_VTBL_INDEX_GET_CURRENT_BACK_BUFFER_INDEX = 36
type PFN_GetCurrentBackBufferIndex = Ptr IDXGISwapChain3 -> IO CUInt
foreign import ccall "dynamic" dcall_getCurrentBackBufferIndex :: FunPtr PFN_GetCurrentBackBufferIndex -> PFN_GetCurrentBackBufferIndex
getCurrentBackBufferIndex :: Ptr IDXGISwapChain3 -> IO Int
getCurrentBackBufferIndex this = fromIntegral <$> (getFunctionPtr _VTBL_INDEX_GET_CURRENT_BACK_BUFFER_INDEX this >>= flip dcall_getCurrentBackBufferIndex this)

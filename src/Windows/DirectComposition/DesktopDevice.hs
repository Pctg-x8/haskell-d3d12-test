{-# LANGUAGE TypeFamilies #-}

module Windows.DirectComposition.DesktopDevice (IDCompositionDesktopDevice, commit, createVisual, createTargetForHwnd) where

import Windows.Types (HRESULT, HWND, BOOL)
import qualified Windows.Const.HResult as HR
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, FunPtr)
import Foreign.C.Types (CLong(..), CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Windows.DirectComposition.Target (IDCompositionTarget)
import Windows.DirectComposition.Visual2 (IDCompositionVisual2)
import Windows.Com.Monad (ComT, comT, runComT, handleHRESULT)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)

data IDCompositionDesktopDeviceVtbl
newtype IDCompositionDesktopDevice = IDCompositionDesktopDevice (Ptr IDCompositionDesktopDeviceVtbl)
instance Storable IDCompositionDesktopDevice where
  sizeOf _ = 8
  alignment _ = 8
  peek p = IDCompositionDesktopDevice <$> peek (castPtr p)
  poke p (IDCompositionDesktopDevice vp) = poke (castPtr p) vp
instance ComInterface IDCompositionDesktopDevice where
  type VTable IDCompositionDesktopDevice = IDCompositionDesktopDeviceVtbl
  guid _ = GUID 0x5f4633fe 0x1e08 0x4cb8 0x8c75ce24333f5602

_VTBL_INDEX_COMMIT = 3
foreign import ccall "dynamic" dcall_commit :: FunPtr (Ptr IDCompositionDesktopDevice -> IO HRESULT) -> Ptr IDCompositionDesktopDevice -> IO HRESULT
commit :: Ptr IDCompositionDesktopDevice -> ComT IO ()
commit this = liftIO (getFunctionPtr _VTBL_INDEX_COMMIT this >>= flip dcall_commit this) >>= handleHRESULT

_VTBL_INDEX_CREATE_VISUAL = 6
type PFN_CreateVisual = Ptr IDCompositionDesktopDevice -> Ptr (Ptr IDCompositionVisual2) -> IO HRESULT
foreign import ccall "dynamic" dcall_createVisual :: FunPtr PFN_CreateVisual -> PFN_CreateVisual
createVisual :: Ptr IDCompositionDesktopDevice -> ComT IO (Ptr IDCompositionVisual2)
createVisual this = comT $ alloca $ \ptr -> runComT $ do
  fn <- liftIO $ dcall_createVisual <$> getFunctionPtr _VTBL_INDEX_CREATE_VISUAL this
  liftIO (fn this ptr) >>= handleHRESULT >> lift (peek ptr)

_VTBL_INDEX_CREATE_TARGET_FOR_HWND = 24
type PFN_CreateTargetForHwnd = Ptr IDCompositionDesktopDevice -> HWND -> BOOL -> Ptr (Ptr IDCompositionTarget) -> IO HRESULT
foreign import ccall "dynamic" dcall_createTargetForHwnd :: FunPtr PFN_CreateTargetForHwnd -> PFN_CreateTargetForHwnd
createTargetForHwnd :: Ptr IDCompositionDesktopDevice -> HWND -> Bool -> ComT IO (Ptr IDCompositionTarget)
createTargetForHwnd this hwnd topmost = comT $ alloca $ \ptr -> runComT $ do
  fn <- liftIO $ dcall_createTargetForHwnd <$> getFunctionPtr _VTBL_INDEX_CREATE_TARGET_FOR_HWND this
  let topmost_ = if topmost then 1 else 0
  liftIO (fn this hwnd topmost_ ptr) >>= handleHRESULT >> lift (peek ptr)

{-# LANGUAGE TypeFamilies #-}

module Windows.DirectComposition.Target (IDCompositionTarget, setRoot) where

import Windows.Types (HRESULT)
import qualified Windows.Const.HResult as HR
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, FunPtr)
import Foreign.C.Types (CLong(..))
import Windows.DirectComposition.Visual2 (IDCompositionVisual2)
import Windows.Com.Monad (ComT, handleHRESULT)
import Control.Monad.IO.Class (liftIO)

data IDCompositionTargetVtbl
newtype IDCompositionTarget = IDCompositionTarget (Ptr IDCompositionTargetVtbl)
instance Storable IDCompositionTarget where
  sizeOf _ = 8
  alignment _ = 8
  peek p = IDCompositionTarget <$> peek (castPtr p)
  poke p (IDCompositionTarget vp) = poke (castPtr p) vp
instance ComInterface IDCompositionTarget where
  type VTable IDCompositionTarget = IDCompositionTargetVtbl
  guid _ = GUID 0xeacdd04c 0x117e 0x4e17 0x88f4d1b12b0e3d89

_VTBL_INDEX_SET_ROOT = 3
type PFN_SetRoot = Ptr IDCompositionTarget -> Ptr IDCompositionVisual2 -> IO HRESULT
foreign import ccall "dynamic" dcall_setRoot :: FunPtr PFN_SetRoot -> PFN_SetRoot
setRoot :: Ptr IDCompositionTarget -> Ptr IDCompositionVisual2 -> ComT IO ()
setRoot this visual = liftIO (getFunctionPtr _VTBL_INDEX_SET_ROOT this >>= \f -> dcall_setRoot f this visual) >>= handleHRESULT

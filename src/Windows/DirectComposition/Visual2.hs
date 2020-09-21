{-# LANGUAGE TypeFamilies #-}

module Windows.DirectComposition.Visual2 (IDCompositionVisual2, setContent) where

import Windows.Types (HRESULT)
import qualified Windows.Const.HResult as HR
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Windows.Com.IUnknown (IUnknown)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, FunPtr)
import Foreign.C.Types (CLong(..))
import Windows.Com.Monad (ComT, handleHRESULT)
import Control.Monad.IO.Class (liftIO)

data IDCompositionVisual2Vtbl
newtype IDCompositionVisual2 = IDCompositionVisual2 (Ptr IDCompositionVisual2Vtbl)
instance Storable IDCompositionVisual2 where
  sizeOf _ = 8
  alignment _ = 8
  peek p = IDCompositionVisual2 <$> peek (castPtr p)
  poke p (IDCompositionVisual2 vp) = poke (castPtr p) vp
instance ComInterface IDCompositionVisual2 where
  type VTable IDCompositionVisual2 = IDCompositionVisual2Vtbl
  guid _ = GUID 0xe8de1639 0x4331 0x4b26 0xbc5f6a321d347a85

_VTBL_INDEX_SET_CONTENT = 15
type PFN_SetContent = Ptr IDCompositionVisual2 -> Ptr IUnknown -> IO HRESULT
foreign import ccall "dynamic" dcall_setContent :: FunPtr PFN_SetContent -> PFN_SetContent
setContent :: Ptr IDCompositionVisual2 -> Ptr a -> ComT IO ()
setContent this content = liftIO (getFunctionPtr _VTBL_INDEX_SET_CONTENT this >>= \f -> dcall_setContent f this $ castPtr content) >>= handleHRESULT

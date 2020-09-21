{-# LANGUAGE TypeFamilies #-}

module Windows.Direct3D12.Fence (ID3D12Fence, setEventOnCompletion, FenceFlags) where

import Foreign.Ptr (Ptr, castPtr, FunPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CLong(..), CUInt(..))
import Windows.ComBase (ComInterface(..))
import Windows.Struct.GUID (GUID(..))
import Windows.Types (HRESULT, HANDLE)
import Data.Word (Word64(..))
import Windows.Com.Monad (ComT, handleHRESULT)
import Control.Monad.IO.Class (liftIO)

data ID3D12FenceVtbl
newtype ID3D12Fence = ID3D12Fence (Ptr ID3D12FenceVtbl)
instance Storable ID3D12Fence where
  sizeOf _ = 8
  alignment _ = 8
  peek p = ID3D12Fence <$> peek (castPtr p)
  poke p (ID3D12Fence vp) = castPtr p `poke` vp
instance ComInterface ID3D12Fence where
  type VTable ID3D12Fence = ID3D12FenceVtbl
  guid _ = GUID 0x0a753dcf 0xc4d8 0x4b91 0xadf6be5a60d95a76

type FenceFlags = CUInt

_VTBL_INDEX_SET_EVENT_ON_COMPLETION = 9
type PFN_SetEventOnCompletion = Ptr ID3D12Fence -> Word64 -> HANDLE -> IO HRESULT
foreign import ccall "dynamic" dcall_setEventOnCompletion :: FunPtr PFN_SetEventOnCompletion -> PFN_SetEventOnCompletion
setEventOnCompletion :: Ptr ID3D12Fence -> Word64 -> HANDLE -> ComT IO ()
setEventOnCompletion this value event = liftIO (getFunctionPtr _VTBL_INDEX_SET_EVENT_ON_COMPLETION this >>= \f -> dcall_setEventOnCompletion f this value event) >>= handleHRESULT

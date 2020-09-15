{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module Windows.D3DBlob (ID3DBlob, getBufferPointer, getBufferSize) where

import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, WordPtr(..))

data BlobVtbl
newtype ID3DBlob = ID3DBlob (Ptr BlobVtbl) deriving Storable
instance ComInterface ID3DBlob where
  type VTable ID3DBlob = BlobVtbl
  guid _ = GUID 0x8ba5fb08 0x5195 0x40e2 0xac580d989c3a0102

_VTBL_INDEX_GET_BUFFER_POINTER = 3
type PFN_Getter a = Ptr ID3DBlob -> IO a
foreign import ccall "dynamic" dcall_getBufferPointer :: FunPtr (PFN_Getter (Ptr ())) -> PFN_Getter (Ptr ())
getBufferPointer :: Ptr ID3DBlob -> IO (Ptr ())
getBufferPointer this = getFunctionPtr _VTBL_INDEX_GET_BUFFER_POINTER this >>= flip dcall_getBufferPointer this

_VTBL_INDEX_GET_BUFFER_SIZE = 4
foreign import ccall "dynamic" dcall_getBufferSize :: FunPtr (PFN_Getter WordPtr) -> PFN_Getter WordPtr
getBufferSize :: Ptr ID3DBlob -> IO WordPtr
getBufferSize this = getFunctionPtr _VTBL_INDEX_GET_BUFFER_SIZE this >>= flip dcall_getBufferSize this

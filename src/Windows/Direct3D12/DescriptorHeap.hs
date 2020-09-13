{-# LANGUAGE TypeFamilies #-}

module Windows.Direct3D12.DescriptorHeap
  ( ID3D12DescriptorHeap, getCPUDescriptorHandleForHeapStart, getGPUDescriptorHandleForHeapStart
  , CPUDescriptorHandle(..), GPUDescriptorHandle(..)
  , DescriptorHeapDesc, defaultDescriptorHeapDesc, setDescriptorHeapDescShaderVisible
  , DescriptorHeapType, descriptorHeapTypeCBVSRVUAV, descriptorHeapTypeSampler, descriptorHeapTypeRTV, descriptorHeapTypeDSV
  ) where

import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr, WordPtr(..))
import Data.Word (Word64(..))
import Foreign.C.Types (CUInt)
import Data.Bits ((.|.))
import Windows.Struct.Direct3D12 (CPUDescriptorHandle, GPUDescriptorHandle)

data ID3D12DescriptorHeapVtbl
newtype ID3D12DescriptorHeap = ID3D12DescriptorHeap (Ptr ID3D12DescriptorHeapVtbl)
instance ComInterface ID3D12DescriptorHeap where
  type VTable ID3D12DescriptorHeap = ID3D12DescriptorHeapVtbl
  guid _ = GUID 0x8efb471d 0x616c 0x4f49 0x90f7127bb763fa51
instance Storable ID3D12DescriptorHeap where
  sizeOf _ = 8
  alignment _ = 8
  peek p = ID3D12DescriptorHeap <$> peek (castPtr p)
  poke p (ID3D12DescriptorHeap vp) = castPtr p `poke` vp

type DescriptorHeapType = CUInt
descriptorHeapTypeCBVSRVUAV, descriptorHeapTypeSampler, descriptorHeapTypeRTV, descriptorHeapTypeDSV :: DescriptorHeapType
descriptorHeapTypeCBVSRVUAV = 0
descriptorHeapTypeSampler = descriptorHeapTypeCBVSRVUAV + 1
descriptorHeapTypeRTV = descriptorHeapTypeSampler + 1
descriptorHeapTypeDSV = descriptorHeapTypeRTV + 1
type DescriptorHeapFlags = CUInt
descriptorHeapFlagShaderVisible :: DescriptorHeapFlags
descriptorHeapFlagShaderVisible = 0x01
data DescriptorHeapDesc = DescriptorHeapDesc
  { descType :: DescriptorHeapType
  , descNumDescriptors :: CUInt
  , descFlags :: DescriptorHeapFlags
  , descNodeMask :: CUInt
  }
instance Storable DescriptorHeapDesc where
  sizeOf _ = 4 * 4
  alignment _ = 4
  peek p = DescriptorHeapDesc <$> peek (castPtr p) <*> peek (plusPtr p 4) <*> peek (plusPtr p 8) <*> peek (plusPtr p 12)
  poke p d = castPtr p `poke` descType d *> plusPtr p 4 `poke` descNumDescriptors d *> plusPtr p 8 `poke` descFlags d *> plusPtr p 12 `poke` descNodeMask d
defaultDescriptorHeapDesc :: DescriptorHeapType -> Int -> DescriptorHeapDesc
defaultDescriptorHeapDesc ty count = DescriptorHeapDesc ty (fromIntegral count) 0 0
setDescriptorHeapDescShaderVisible :: DescriptorHeapDesc -> DescriptorHeapDesc
setDescriptorHeapDescShaderVisible d = d { descFlags = descFlags d .|. descriptorHeapFlagShaderVisible }

_VTBL_INDEX_GET_CPU_DESCRIPTOR_HANDLE_FOR_HEAP_START = 9
_VTBL_INDEX_GET_GPU_DESCRIPTOR_HANDLE_FOR_HEAP_START = 10
-- ほんとうは&CPUDescriptorHandleが帰る 中身は同じ
-- 構造体なのでポインタで帰ってくる
type PFN_GetCPUDescriptorHandleForHeapStart = Ptr ID3D12DescriptorHeap -> IO (Ptr WordPtr)
-- ほんとうは&GPUDescriptorHandleが帰る 中身は同じ
type PFN_GetGPUDescriptorHandleForHeapStart = Ptr ID3D12DescriptorHeap -> IO (Ptr Word64)
foreign import ccall "dynamic" dcall_getCPUDescriptorHandleForHeapStart :: FunPtr PFN_GetCPUDescriptorHandleForHeapStart -> PFN_GetCPUDescriptorHandleForHeapStart
foreign import ccall "dynamic" dcall_getGPUDescriptorHandleForHeapStart :: FunPtr PFN_GetGPUDescriptorHandleForHeapStart -> PFN_GetGPUDescriptorHandleForHeapStart
getCPUDescriptorHandleForHeapStart :: Ptr ID3D12DescriptorHeap -> IO CPUDescriptorHandle
getGPUDescriptorHandleForHeapStart :: Ptr ID3D12DescriptorHeap -> IO GPUDescriptorHandle
getCPUDescriptorHandleForHeapStart this = peek . castPtr =<< (getFunctionPtr _VTBL_INDEX_GET_CPU_DESCRIPTOR_HANDLE_FOR_HEAP_START this >>= flip dcall_getCPUDescriptorHandleForHeapStart this)
getGPUDescriptorHandleForHeapStart this = peek . castPtr =<< (getFunctionPtr _VTBL_INDEX_GET_GPU_DESCRIPTOR_HANDLE_FOR_HEAP_START this >>= flip dcall_getGPUDescriptorHandleForHeapStart this)

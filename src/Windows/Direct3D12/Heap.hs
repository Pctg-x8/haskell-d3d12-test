{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module Windows.Direct3D12.Heap
  ( ID3D12Heap
  , HeapType, heapTypeDefault, heapTypeUpload, heapTypeReadback, heapTypeCustom
  , CPUPageProperty, cpuPagePropertyUnknown, cpuPagePropertyNotAvailable, cpuPagePropertyWriteCombine, cpuPagePropertyWriteBack
  , MemoryPool, memoryPoolUnknown, memoryPoolL0, memoryPoolL1
  , HeapProperties(..), heapDefaultProperties, heapUploadProperties
  , HeapFlags, heapFlagNone, heapFlagDenyBuffers, heapFlagDenyRTDSTextures, heapFlagDenyNonRTDSTextures
  , heapFlagAllowAllBuffersAndTextures, heapFlagAllowOnlyBuffers, heapFlagAllowOnlyNonRTDSTextures, heapFlagAllowOnlyRTDSTextures
  , HeapDesc(..)
  ) where

import Windows.Types (HRESULT)
import qualified Windows.Const.HResult as HR
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr)
import Foreign.C.Types (CLong(..), CInt, CUInt)
import Data.Word (Word64)

data ID3D12HeapVtbl
newtype ID3D12Heap = ID3D12Heap (Ptr ID3D12HeapVtbl) deriving Storable
instance ComInterface ID3D12Heap where
  type VTable ID3D12Heap = ID3D12HeapVtbl
  guid _ = GUID 0x6b3b2502 0x6e51 0x45b3 0x90ee9884265e8df3

type HeapType = CInt
heapTypeDefault, heapTypeUpload, heapTypeReadback, heapTypeCustom :: HeapType
heapTypeDefault = 1
heapTypeUpload = 2
heapTypeReadback = 3
heapTypeCustom = 4
type CPUPageProperty = CInt
cpuPagePropertyUnknown, cpuPagePropertyNotAvailable, cpuPagePropertyWriteCombine, cpuPagePropertyWriteBack :: CPUPageProperty
cpuPagePropertyUnknown = 0
cpuPagePropertyNotAvailable = 1
cpuPagePropertyWriteCombine = 2
cpuPagePropertyWriteBack = 3
type MemoryPool = CInt
memoryPoolUnknown, memoryPoolL0, memoryPoolL1 :: MemoryPool
memoryPoolUnknown = 0
memoryPoolL0 = 1
memoryPoolL1 = 2
data HeapProperties = HeapProperties
  { propertiesType :: HeapType
  , propertiesCPUPageProperty :: CPUPageProperty
  , propertiesMemoryPoolPreference :: MemoryPool
  , propertiesCreationNodeMask :: CUInt
  , propertiesVisibleNodeMask :: CUInt }
instance Storable HeapProperties where
  sizeOf _ = 4 * 5
  alignment _ = 4
  peek p = HeapProperties <$>
    peek (castPtr p) <*>
    peek (plusPtr p 4) <*>
    peek (plusPtr p 8) <*>
    peek (plusPtr p 12) <*>
    peek (plusPtr p 16)
  poke p (HeapProperties ty page mp cnm vnm) = do
    poke (castPtr p) ty
    poke (plusPtr p 4) page
    poke (plusPtr p 8) mp
    poke (plusPtr p 12) cnm
    poke (plusPtr p 16) vnm
heapDefaultProperties, heapUploadProperties :: HeapProperties
heapDefaultProperties = HeapProperties heapTypeDefault cpuPagePropertyUnknown memoryPoolUnknown 0 0
heapUploadProperties = HeapProperties heapTypeUpload cpuPagePropertyUnknown memoryPoolUnknown 0 0
type HeapFlags = CInt
heapFlagNone, heapFlagDenyBuffers, heapFlagDenyRTDSTextures, heapFlagDenyNonRTDSTextures :: HeapFlags
heapFlagAllowAllBuffersAndTextures, heapFlagAllowOnlyBuffers, heapFlagAllowOnlyNonRTDSTextures, heapFlagAllowOnlyRTDSTextures :: HeapFlags
heapFlagNone = 0
heapFlagDenyBuffers = 0x04
heapFlagDenyRTDSTextures = 0x40
heapFlagDenyNonRTDSTextures = 0x80
heapFlagAllowAllBuffersAndTextures = 0
heapFlagAllowOnlyBuffers = 0xc0
heapFlagAllowOnlyNonRTDSTextures = 0x44
heapFlagAllowOnlyRTDSTextures = 0x84
data HeapDesc = HeapDesc
  { descSizeInBytes :: Word64
  , descProperties :: HeapProperties
  , descAlignment :: Word64
  , descFlags :: HeapFlags }
instance Storable HeapDesc where
  sizeOf _ = 28 + sizeOf (undefined :: HeapProperties)
  alignment _ = 8
  peek p = HeapDesc <$>
    peek (castPtr p) <*>
    peek (plusPtr p 8) <*>
    -- considering alignment to 8 bytes(HeapProperties has 20 bytes)
    peek (plusPtr p $ 8 + sizeOf (undefined :: HeapProperties) + 4) <*>
    peek (plusPtr p $ 8 + sizeOf (undefined :: HeapProperties) + 4 + 8)
  poke p (HeapDesc size props align flags) = do
    poke (castPtr p) size
    poke (plusPtr p 8) props
    poke (plusPtr p $ 8 + sizeOf (undefined :: HeapProperties) + 4) align
    poke (plusPtr p $ 8 + sizeOf (undefined :: HeapProperties) + 4 + 8) flags

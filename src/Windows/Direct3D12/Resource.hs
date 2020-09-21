{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module Windows.Direct3D12.Resource
  ( ID3D12Resource, map, unmap, getGPUVirtualAddress
  , ResourceDimension, resourceDimensionUnknown, resourceDimensionBuffer, resourceDimensionTexture1D
  , resourceDimensionTexture2D, resourceDimensionTexture3D
  , TextureLayout, textureLayoutUnknown, textureLayoutRowMajor
  , textureLayout64KBUndefinedSwizzle, textureLayout64KBStandardSwizzle
  , ResourceFlags, resourceFlagsNone, resourceFlagsAllowRenderTarget, resourceFlagsAllowDepthStencil
  , resourceFlagsAllowUnorderedAccess, resourceFlagsDenyShaderResource, resourceFlagsAllowCrossAdapter
  , resourceFlagsAllowSimultaneousAccess, resourceFlagsVideoDecodeReferenceOnly
  , ResourceDesc(..)
  , ResourceStates, resourceStatePresent, resourceStateRenderTarget, resourceStateVertexAndConstantBuffer
  , resourceStateCopyDest, resourceStateGenericRead
  ) where

import Prelude hiding (map)
import Windows.Types (HRESULT)
import qualified Windows.Const.HResult as HR
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CUInt(..), CInt, CLong(..))
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Marshal.Utils as Marshal
import Data.Word (Word64, Word16)
import Data.Bits ((.|.))
import Windows.Const.Dxgi (Format)
import Windows.Struct.Dxgi (SampleDesc)
import Windows.Struct.Direct3D12 (Range)
import Windows.Com.Monad (ComT, comT, runComT, handleHRESULT)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)

data ID3D12ResourceVtbl
newtype ID3D12Resource = ID3D12Resource (Ptr ID3D12ResourceVtbl) deriving Storable
instance ComInterface ID3D12Resource where
  type VTable ID3D12Resource = ID3D12ResourceVtbl
  guid _ = GUID 0x696442be 0xa72e 0x4059 0xbc795b5c98040fad

_VTBL_INDEX_MAP = 8
type PFN_Map = Ptr ID3D12Resource -> CUInt -> Ptr Range -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_map :: FunPtr PFN_Map -> PFN_Map
map :: Ptr ID3D12Resource -> CUInt -> Range -> ComT IO (Ptr ())
map this subres readRange = flip runContT pure $ do
  ptr <- ContT $ \f -> comT $ alloca $ runComT . f
  rangeRef <- ContT $ \f -> comT $ Marshal.with readRange $ runComT . f
  fn <- liftIO $ dcall_map <$> getFunctionPtr _VTBL_INDEX_MAP this
  hr <- liftIO $ fn this subres rangeRef ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

_VTBL_INDEX_UNMAP = 9
type PFN_Unmap = Ptr ID3D12Resource -> CUInt -> Ptr Range -> IO ()
foreign import ccall "dynamic" dcall_unmap :: FunPtr PFN_Unmap -> PFN_Unmap
unmap :: Ptr ID3D12Resource -> CUInt -> Range -> IO ()
unmap this subres writeRange = Marshal.with writeRange $ \rangeRef -> do
  fn <- dcall_unmap <$> getFunctionPtr _VTBL_INDEX_UNMAP this
  fn this subres rangeRef

_VTBL_INDEX_GET_GPU_VIRTUAL_ADDRESS = 11
type PFN_GetGPUVirtualAddress = Ptr ID3D12Resource -> IO Word64
foreign import ccall "dynamic" dcall_getGPUVirtualAddress :: FunPtr PFN_GetGPUVirtualAddress -> PFN_GetGPUVirtualAddress
getGPUVirtualAddress :: Ptr ID3D12Resource -> IO Word64
getGPUVirtualAddress this = getFunctionPtr _VTBL_INDEX_GET_GPU_VIRTUAL_ADDRESS this >>= flip dcall_getGPUVirtualAddress this

type ResourceDimension = CInt
resourceDimensionUnknown, resourceDimensionBuffer, resourceDimensionTexture1D, resourceDimensionTexture2D, resourceDimensionTexture3D :: ResourceDimension
resourceDimensionUnknown = 0
resourceDimensionBuffer = 1
resourceDimensionTexture1D = 2
resourceDimensionTexture2D = 3
resourceDimensionTexture3D = 4
type TextureLayout = CInt
textureLayoutUnknown, textureLayoutRowMajor, textureLayout64KBUndefinedSwizzle, textureLayout64KBStandardSwizzle :: TextureLayout
textureLayoutUnknown = 0
textureLayoutRowMajor = 1
textureLayout64KBUndefinedSwizzle = 2
textureLayout64KBStandardSwizzle = 3
type ResourceFlags = CInt
resourceFlagsNone, resourceFlagsAllowRenderTarget, resourceFlagsAllowDepthStencil, resourceFlagsAllowUnorderedAccess :: ResourceFlags
resourceFlagsDenyShaderResource, resourceFlagsAllowCrossAdapter, resourceFlagsAllowSimultaneousAccess, resourceFlagsVideoDecodeReferenceOnly :: ResourceFlags
resourceFlagsNone = 0
resourceFlagsAllowRenderTarget = 0x01
resourceFlagsAllowDepthStencil = 0x02
resourceFlagsAllowUnorderedAccess = 0x04
resourceFlagsDenyShaderResource = 0x08
resourceFlagsAllowCrossAdapter = 0x10
resourceFlagsAllowSimultaneousAccess = 0x20
resourceFlagsVideoDecodeReferenceOnly = 0x40
data ResourceDesc = ResourceDesc
  { descDimension :: ResourceDimension
  , descAlignment :: Word64
  , descWidth :: Word64
  , descHeight :: CUInt
  , descDepthOrArraySize :: Word16
  , descMipLevels :: Word16
  , descFormat :: Format
  , descSampleDesc :: SampleDesc
  , descTextureLayout :: TextureLayout
  , descFlags :: ResourceFlags }
instance Storable ResourceDesc where
  sizeOf _ = 52
  alignment _ = 8
  peek p = ResourceDesc <$>
    peek (castPtr p) <*>
    peek (plusPtr p 8) <*>
    peek (plusPtr p 16) <*>
    peek (plusPtr p 24) <*>
    peek (plusPtr p 28) <*>
    peek (plusPtr p 30) <*>
    peek (plusPtr p 32) <*>
    peek (plusPtr p 36) <*>
    peek (plusPtr p 44) <*>
    peek (plusPtr p 48)
  poke p (ResourceDesc dim align width height doa mip fmt sd tl flags) = do
    poke (castPtr p) dim
    poke (plusPtr p 8) align
    poke (plusPtr p 16) width
    poke (plusPtr p 24) height
    poke (plusPtr p 28) doa
    poke (plusPtr p 30) mip
    poke (plusPtr p 32) fmt
    poke (plusPtr p 36) sd
    poke (plusPtr p 44) tl
    poke (plusPtr p 48) flags

type ResourceStates = CUInt
resourceStateRenderTarget, resourceStatePresent, resourceStateVertexAndConstantBuffer, resourceStateCopyDest, resourceStateGenericRead :: ResourceStates
resourceStateRenderTarget = 0x04
resourceStatePresent = 0
resourceStateVertexAndConstantBuffer = 0x01
resourceStateCopyDest = 0x400
resourceStateGenericRead = 0x01 .|. 0x02 .|. 0x40 .|. 0x80 .|. 0x200 .|. 0x800

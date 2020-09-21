{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module Windows.Direct3D12.RootSignature
  ( ID3D12RootSignature, serializeRootSignature
  , RootSignatureVersion, rootSignatureVersion1
  , RootSignatureDesc(..), StaticSamplerDesc(..), RootParameter(..), DescriptorRange(..)
  , ShaderVisibility, shaderVisibilityAll, shaderVisibilityVertex, shaderVisibilityHull, shaderVisibilityDomain, shaderVisibilityGeometry, shaderVisibilityPixel
  , DescriptorRangeType, descriptorRangeTypeSRV, descriptorRangeTypeUAV, descriptorRangeTypeCBV, descriptorRangeTypeSampler
  , RootSignatureFlags, rootSignatureFlagAllowInputAssemblerInputLayout
  ) where

import Windows.Types (HRESULT)
import qualified Windows.Const.HResult as HR
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr, nullPtr)
import Foreign.C.Types (CUInt, CLong(..), CInt(..), CFloat)
import qualified Foreign.Marshal.Utils as Marshal
import Foreign.Marshal.Alloc (alloca)
import Windows.LibLoader (Lib, getProcAddress)
import Windows.Const.Direct3D12 (Filter, TextureAddressMode, ComparisonFunc, StaticBorderColor)
import Windows.D3DBlob (ID3DBlob)
import Windows.Com.Monad (ComT, comT, runComT, handleHRESULT)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)

data ID3D12RootSignatureVtbl
newtype ID3D12RootSignature = ID3D12RootSignature (Ptr ID3D12RootSignatureVtbl) deriving Storable
instance ComInterface ID3D12RootSignature where
  type VTable ID3D12RootSignature = ID3D12RootSignatureVtbl
  guid _ = GUID 0xc54a6b66 0x72df 0x4ee8 0x8be5a946a1429214

type ShaderVisibility = CInt
shaderVisibilityAll, shaderVisibilityVertex, shaderVisibilityHull, shaderVisibilityDomain, shaderVisibilityGeometry, shaderVisibilityPixel :: ShaderVisibility
shaderVisibilityAll = 0
shaderVisibilityVertex = 1
shaderVisibilityHull = 2
shaderVisibilityDomain = 3
shaderVisibilityGeometry = 4
shaderVisibilityPixel = 5
type DescriptorRangeType = CInt
descriptorRangeTypeSRV, descriptorRangeTypeUAV, descriptorRangeTypeCBV, descriptorRangeTypeSampler :: DescriptorRangeType
descriptorRangeTypeSRV = 0
descriptorRangeTypeUAV = descriptorRangeTypeSRV+1
descriptorRangeTypeCBV = descriptorRangeTypeUAV+1
descriptorRangeTypeSampler = descriptorRangeTypeCBV+1
data DescriptorRange = DescriptorRange DescriptorRangeType CUInt CUInt CUInt CUInt
instance Storable DescriptorRange where
  sizeOf _ = 4 * 5
  alignment _ = 4
  peek p = DescriptorRange <$> peek (castPtr p) <*> peek (plusPtr p 4) <*> peek (plusPtr p 8) <*> peek (plusPtr p 12) <*> peek (plusPtr p 16)
  poke p (DescriptorRange ty count base regspace offs) = do
    poke (castPtr p) ty
    poke (plusPtr p 4) count
    poke (plusPtr p 8) base
    poke (plusPtr p 12) regspace
    poke (plusPtr p 16) offs
data RootParameter =
  RootDescriptorTable ShaderVisibility CUInt (Ptr DescriptorRange) |
  RootConstants ShaderVisibility CUInt CUInt CUInt |
  RootDescriptorCBV ShaderVisibility CUInt CUInt |
  RootDescriptorSRV ShaderVisibility CUInt CUInt |
  RootDescriptorUAV ShaderVisibility CUInt CUInt
instance Storable RootParameter where
  sizeOf _ = 4 * 5
  alignment _ = 8
  peek p = do
    ty <- peek $ castPtr p :: IO CInt
    sv <- peek $ plusPtr p 24 :: IO ShaderVisibility
    case ty of
      0 -> RootDescriptorTable sv <$> peek (plusPtr p 8) <*> peek (plusPtr p 16)
      1 -> RootConstants sv <$> peek (plusPtr p 8) <*> peek (plusPtr p 12) <*> peek (plusPtr p 16)
      2 -> RootDescriptorCBV sv <$> peek (plusPtr p 8) <*> peek (plusPtr p 12)
      3 -> RootDescriptorSRV sv <$> peek (plusPtr p 8) <*> peek (plusPtr p 12)
      4 -> RootDescriptorUAV sv <$> peek (plusPtr p 8) <*> peek (plusPtr p 12)
  poke p (RootDescriptorTable sv len ptr) = do
    poke (castPtr p) (0 :: CInt)
    poke (plusPtr p 24) sv
    poke (plusPtr p 8) len
    poke (plusPtr p 16) ptr
  poke p (RootConstants sv register regspace count) = do
    poke (castPtr p) (1 :: CInt)
    poke (plusPtr p 24) sv
    poke (plusPtr p 8) register
    poke (plusPtr p 12) regspace
    poke (plusPtr p 16) count
  poke p (RootDescriptorCBV sv register regspace) = do
    poke (castPtr p) (2 :: CInt)
    poke (plusPtr p 24) sv
    poke (plusPtr p 8) register
    poke (plusPtr p 12) regspace
  poke p (RootDescriptorSRV sv register regspace) = do
    poke (castPtr p) (3 :: CInt)
    poke (plusPtr p 24) sv
    poke (plusPtr p 8) register
    poke (plusPtr p 12) regspace
  poke p (RootDescriptorUAV sv register regspace) = do
    poke (castPtr p) (4 :: CInt)
    poke (plusPtr p 24) sv
    poke (plusPtr p 8) register
    poke (plusPtr p 12) regspace
data StaticSamplerDesc = StaticSamplerDesc
  { ssDescFilter :: Filter
  , ssDescAddressU :: TextureAddressMode
  , ssDescAddressV :: TextureAddressMode
  , ssDescAddressW :: TextureAddressMode
  , ssDescMipLODBias :: CFloat
  , ssDescMaxAnisotropy :: CUInt
  , ssDescComparisonFunc :: ComparisonFunc
  , ssDescBorderColor :: StaticBorderColor
  , ssDescMinLOD :: CFloat
  , ssDescMaxLOD :: CFloat
  , ssDescShaderRegister :: CUInt
  , ssDescRegisterSpace :: CUInt
  , ssDescShaderVisibility :: ShaderVisibility
  }
instance Storable StaticSamplerDesc where
  sizeOf _ = 4 * 13
  alignment _ = 4
  peek p = StaticSamplerDesc <$>
    peek (castPtr p) <*>
    peek (plusPtr p 4) <*>
    peek (plusPtr p 8) <*>
    peek (plusPtr p 12) <*>
    peek (plusPtr p 16) <*>
    peek (plusPtr p 20) <*>
    peek (plusPtr p 24) <*>
    peek (plusPtr p 28) <*>
    peek (plusPtr p 32) <*>
    peek (plusPtr p 36) <*>
    peek (plusPtr p 40) <*>
    peek (plusPtr p 44) <*>
    peek (plusPtr p 48)
  poke p sd = do
    poke (castPtr p) $ ssDescFilter sd
    poke (plusPtr p 4) $ ssDescAddressU sd
    poke (plusPtr p 8) $ ssDescAddressV sd
    poke (plusPtr p 12) $ ssDescAddressW sd
    poke (plusPtr p 16) $ ssDescMipLODBias sd
    poke (plusPtr p 20) $ ssDescMaxAnisotropy sd
    poke (plusPtr p 24) $ ssDescComparisonFunc sd
    poke (plusPtr p 28) $ ssDescBorderColor sd
    poke (plusPtr p 32) $ ssDescMinLOD sd
    poke (plusPtr p 36) $ ssDescMaxLOD sd
    poke (plusPtr p 40) $ ssDescShaderRegister sd
    poke (plusPtr p 44) $ ssDescRegisterSpace sd
    poke (plusPtr p 48) $ ssDescShaderVisibility sd
type RootSignatureFlags = CInt
rootSignatureFlagAllowInputAssemblerInputLayout :: RootSignatureFlags
rootSignatureFlagAllowInputAssemblerInputLayout = 0x01
data RootSignatureDesc = RootSignatureDesc CUInt (Ptr RootParameter) CUInt (Ptr StaticSamplerDesc) RootSignatureFlags
instance Storable RootSignatureDesc where
  sizeOf _ = 8 * 4 + 4
  alignment _ = 8
  peek p = RootSignatureDesc <$>
    peek (castPtr p) <*>
    peek (plusPtr p 8) <*>
    peek (plusPtr p 16) <*>
    peek (plusPtr p 24) <*>
    peek (plusPtr p 32)
  poke p (RootSignatureDesc paramCount ptrParams ssCount ptrSamplers flags) = do
    poke (castPtr p) paramCount
    poke (plusPtr p 8) ptrParams
    poke (plusPtr p 16) ssCount
    poke (plusPtr p 24) ptrSamplers
    poke (plusPtr p 32) flags

type RootSignatureVersion = CInt
rootSignatureVersion1 :: RootSignatureVersion
rootSignatureVersion1 = 0x01

type PFN_D3D12SerializeRootSignature = Ptr RootSignatureDesc -> RootSignatureVersion -> Ptr (Ptr ID3DBlob) -> Ptr (Ptr ID3DBlob) -> IO HRESULT
foreign import ccall "dynamic" dcall_serializeRootSignature :: FunPtr PFN_D3D12SerializeRootSignature -> PFN_D3D12SerializeRootSignature
serializeRootSignature :: Lib -> RootSignatureDesc -> RootSignatureVersion -> ComT IO (Ptr ID3DBlob)
serializeRootSignature lib desc v = flip runContT pure $ do
  blobptr <- ContT $ \f -> comT $ alloca $ runComT . f
  descRef <- ContT $ \f -> comT $ Marshal.with desc $ runComT . f
  fn <- liftIO $ dcall_serializeRootSignature <$> getProcAddress "D3D12SerializeRootSignature" lib
  hr <- liftIO $ fn descRef v blobptr nullPtr
  lift $ handleHRESULT hr >> lift (peek blobptr)

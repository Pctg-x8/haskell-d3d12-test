{-# LANGUAGE TypeFamilies, GeneralisedNewtypeDeriving #-}

module Windows.Direct3D12.PipelineState where

import Windows.Types (BOOL, LPCSTR)
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Windows.Const.Direct3D12 (ComparisonFunc, comparisonFuncNever, comparisonFuncAlways)
import Windows.Direct3D12.RootSignature (ID3D12RootSignature)
import qualified Windows.Const.Dxgi as DxgiConst
import qualified Windows.Struct.Dxgi as DxgiStruct
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, WordPtr, castPtr, plusPtr, nullPtr)
import Foreign.C.Types (CUInt, CInt, CFloat)
import Foreign.Marshal.Array (peekArray, pokeArray)
import qualified Foreign.Marshal.Utils as MarshalUtils
import Data.Word (Word8)
import Data.Bits ((.&.), complement)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (toForeignPtr)
import Foreign.ForeignPtr (withForeignPtr)

data ID3D12PipelineStateVtbl
newtype ID3D12PipelineState = ID3D12PipelineState (Ptr ID3D12PipelineStateVtbl) deriving Storable
instance ComInterface ID3D12PipelineState where
  type VTable ID3D12PipelineState = ID3D12PipelineStateVtbl
  guid _ = GUID 0x765a30f3 0xf624 0x4c6f 0xa828ace948622445

data ShaderBytecode = ShaderBytecode (Ptr ()) WordPtr
emptyShaderBytecode :: ShaderBytecode
emptyShaderBytecode = ShaderBytecode nullPtr 0
withNewShaderBytecode :: ByteString -> (ShaderBytecode -> IO a) -> IO a
withNewShaderBytecode bs action =
  let
    (ptr, offs, len) = toForeignPtr bs
  in withForeignPtr ptr $ \pinned -> action $ ShaderBytecode (plusPtr pinned offs) (fromIntegral len)
instance Storable ShaderBytecode where
  sizeOf _ = sizeOf (undefined :: WordPtr) * 2
  alignment _ = alignment (undefined :: WordPtr)
  peek p = ShaderBytecode <$> peek (castPtr p) <*> peek (plusPtr p $ sizeOf (undefined :: WordPtr))
  poke p (ShaderBytecode bp l) = poke (castPtr p) bp *> poke (plusPtr p $ sizeOf (undefined :: WordPtr)) l
data StreamOutputDesc = StreamOutputDesc (Ptr ()) CUInt (Ptr CUInt) CUInt CUInt
instance Storable StreamOutputDesc where
  sizeOf _ = sizeOf (undefined :: Ptr a) * 3 + 4 * 2
  alignment _ = alignment (undefined :: Ptr a)
  peek p = StreamOutputDesc <$>
    peek (castPtr p) <*>
    peek (plusPtr p $ sizeOf (undefined :: Ptr a)) <*>
    peek (plusPtr p $ sizeOf (undefined :: Ptr a) * 2) <*>
    peek (plusPtr p $ sizeOf (undefined :: Ptr a) * 3) <*>
    peek (plusPtr p $ sizeOf (undefined :: Ptr a) * 3 + 4)
  poke p (StreamOutputDesc declarations numEntries bufferStrides numStrides rasterizedStream) = do
    poke (castPtr p) declarations
    poke (plusPtr p $ sizeOf (undefined :: Ptr a)) numEntries
    poke (plusPtr p $ sizeOf (undefined :: Ptr a) * 2) bufferStrides
    poke (plusPtr p $ sizeOf (undefined :: Ptr a) * 3) numStrides
    poke (plusPtr p $ sizeOf (undefined :: Ptr a) * 3 + 4) rasterizedStream
streamOutputDescDisabled :: StreamOutputDesc
streamOutputDescDisabled = StreamOutputDesc nullPtr 0  nullPtr 0 0
type Blend = CInt
blendZero, blendOne, blendSrcColor, blendInvSrcColor, blendSrcAlpha, blendInvSrcAlpha, blendDestAlpha, blendInvDestAlpha :: Blend
blendDestColor, blendInvDestColor, blendSrcAlphaSat, blendBlendFactor, blendInvBlendFactor, blendSrc1Color, blendInvSrc1Color :: Blend
blendSrc1Alpha, blendInvSrc1Alpha :: Blend
blendZero = 1
blendOne = 2
blendSrcColor = 3
blendInvSrcColor = 4
blendSrcAlpha = 5
blendInvSrcAlpha = 6
blendDestAlpha = 7
blendInvDestAlpha = 8
blendDestColor = 9
blendInvDestColor = 10
blendSrcAlphaSat = 11
blendBlendFactor = 14
blendInvBlendFactor = 15
blendSrc1Color = 16
blendInvSrc1Color = 17
blendSrc1Alpha = 18
blendInvSrc1Alpha = 19
type BlendOp = CInt
blendOpAdd, blendOpSubtract, blendOpRevSubtract, blendOpMin, blendOpMax :: BlendOp
blendOpAdd = 1
blendOpSubtract = 2
blendOpRevSubtract = 3
blendOpMin = 4
blendOpMax = 5
type LogicOp = CInt
data RenderTargetBlendDesc = RenderTargetBlendDesc
  { blendEnable :: Bool
  , blendLogicOpEnable :: Bool
  , blendSrc :: Blend
  , blendDest :: Blend
  , blendOp :: BlendOp
  , blendAlphaSrc :: Blend
  , blendAlphaDest :: Blend
  , blendOpAlpha :: BlendOp
  , blendLogicOp :: LogicOp
  , blendRendertTargetWriteMask :: Word8
  }
renderTargetBlendDescDisabled :: RenderTargetBlendDesc
renderTargetBlendDescDisabled = RenderTargetBlendDesc False False 0 0 0 0 0 0 0 0x0f
instance Storable RenderTargetBlendDesc where
  sizeOf _ = 4 * 9 + 1
  alignment _ = 4
  peek p = RenderTargetBlendDesc <$>
    fmap MarshalUtils.toBool (peek (castPtr p) :: IO BOOL) <*>
    fmap MarshalUtils.toBool (peek (plusPtr p 4) :: IO BOOL) <*>
    peek (plusPtr p 8) <*>
    peek (plusPtr p 12) <*>
    peek (plusPtr p 16) <*>
    peek (plusPtr p 20) <*>
    peek (plusPtr p 24) <*>
    peek (plusPtr p 28) <*>
    peek (plusPtr p 32) <*>
    peek (plusPtr p 36)
  poke p (RenderTargetBlendDesc e le src dest op srca desta opa lop wmask) = do
    poke (castPtr p) (MarshalUtils.fromBool e :: BOOL)
    poke (plusPtr p 4) (MarshalUtils.fromBool le :: BOOL)
    poke (plusPtr p 8) src
    poke (plusPtr p 12) dest
    poke (plusPtr p 16) op
    poke (plusPtr p 20) srca
    poke (plusPtr p 24) desta
    poke (plusPtr p 28) opa
    poke (plusPtr p 32) lop
    poke (plusPtr p 36) wmask
emptyRenderTargetBlendDesc :: RenderTargetBlendDesc
emptyRenderTargetBlendDesc = RenderTargetBlendDesc False False 0 0 0 0 0 0 0 0
data BlendDesc = BlendDesc Bool Bool [RenderTargetBlendDesc]
instance Storable BlendDesc where
  sizeOf _ = 4 * (2 + 8 * 10)
  alignment _ = 4
  peek p = BlendDesc <$>
    fmap MarshalUtils.toBool (peek (castPtr p) :: IO BOOL) <*>
    fmap MarshalUtils.toBool (peek (plusPtr p 4) :: IO BOOL) <*>
    peekArray 8 (plusPtr p 8)
  poke p (BlendDesc cov ind rts) = do
    poke (castPtr p) (MarshalUtils.fromBool cov :: BOOL)
    poke (plusPtr p 4) (MarshalUtils.fromBool ind :: BOOL)
    pokeArray (plusPtr p 8) $ take 8 $ rts ++ repeat emptyRenderTargetBlendDesc
type FillMode = CInt
fillModeWireframe, fillModeSolid :: FillMode
fillModeWireframe = 2
fillModeSolid = 3
type CullMode = CInt
cullModeNone, cullModeFront, cullModeBack :: CullMode
cullModeNone = 1
cullModeFront = 2
cullModeBack = 3
type ConservativeRasterizationMode = CInt
conservativeRasterizationModeOff, conservativeRasterizationModeOn :: ConservativeRasterizationMode
conservativeRasterizationModeOff = 0
conservativeRasterizationModeOn = 1
data RasterizerState = RasterizerState
  { rasterizerFillMode :: FillMode
  , rasterizerCullMode :: CullMode
  , rasterizerFrontCounterClockwise :: Bool
  , rasterizerDepthBias :: CInt
  , rasterizerDepthBiasClamp :: CFloat
  , rasterizerSlopeScaledDepthBias :: CFloat
  , rasterizerDepthClipEnable :: Bool
  , rasterizerMultisampleEnable :: Bool
  , rasterizerAntialiasedListEnable :: Bool
  , rasterizerForcedSampleCount :: CUInt
  , rasterizerConservativeRaster :: ConservativeRasterizationMode
  }
defaultRasterizerState :: RasterizerState
defaultRasterizerState = RasterizerState fillModeSolid cullModeBack False 0 0.0 0.0 False False False 0 conservativeRasterizationModeOff
instance Storable RasterizerState where
  sizeOf _ = 4 * 11
  alignment _ = 4
  peek p = RasterizerState <$>
    peek (castPtr p) <*>
    peek (plusPtr p 4) <*>
    fmap MarshalUtils.toBool (peek $ plusPtr p 8 :: IO BOOL) <*>
    peek (plusPtr p 12) <*>
    peek (plusPtr p 16) <*>
    peek (plusPtr p 20) <*>
    fmap MarshalUtils.toBool (peek $ plusPtr p 24 :: IO BOOL) <*>
    fmap MarshalUtils.toBool (peek $ plusPtr p 28 :: IO BOOL) <*>
    fmap MarshalUtils.toBool (peek $ plusPtr p 32 :: IO BOOL) <*>
    peek (plusPtr p 36) <*>
    peek (plusPtr p 40)
  poke p r = do
    poke (castPtr p) $ rasterizerFillMode r
    poke (plusPtr p 4) $ rasterizerCullMode r
    poke (plusPtr p 8) (MarshalUtils.fromBool $ rasterizerFrontCounterClockwise r :: BOOL)
    poke (plusPtr p 12) $ rasterizerDepthBias r
    poke (plusPtr p 16) $ rasterizerDepthBiasClamp r
    poke (plusPtr p 20) $ rasterizerSlopeScaledDepthBias r
    poke (plusPtr p 24) (MarshalUtils.fromBool $ rasterizerDepthClipEnable r :: BOOL)
    poke (plusPtr p 28) (MarshalUtils.fromBool $ rasterizerMultisampleEnable r :: BOOL)
    poke (plusPtr p 32) (MarshalUtils.fromBool $ rasterizerAntialiasedListEnable r :: BOOL)
    poke (plusPtr p 36) $ rasterizerForcedSampleCount r
    poke (plusPtr p 40) $ rasterizerConservativeRaster r
type DepthWriteMask = CInt
depthWriteMaskZero, depthWriteMaskAll :: DepthWriteMask
depthWriteMaskZero = 0
depthWriteMaskAll = 1
type StencilOp = CInt
stencilOpKeep, stencilOpZero, stencilOpReplace, stencilOpIncrSat, stencilOpDecrSat, stencilOpInvert, stencilOpIncr, stencilOpDecr :: StencilOp
stencilOpKeep = 1
stencilOpZero = 2
stencilOpReplace = 3
stencilOpIncrSat = 4
stencilOpDecrSat = 5
stencilOpInvert = 6
stencilOpIncr = 7
stencilOpDecr = 8
data StencilOpDesc = StencilOpDesc StencilOp StencilOp StencilOp ComparisonFunc
instance Storable StencilOpDesc where
  sizeOf _ = 4 * 4
  alignment _ = 4
  peek p = StencilOpDesc <$> peek (castPtr p) <*> peek (plusPtr p 4) <*> peek (plusPtr p 8) <*> peek (plusPtr p 12)
  poke p (StencilOpDesc fail dfail pass comp) =
    poke (castPtr p) fail *> poke (plusPtr p 4) dfail *> poke (plusPtr p 8) pass *> poke (plusPtr p 12) comp
noopStencilOpDesc :: StencilOpDesc
noopStencilOpDesc = StencilOpDesc stencilOpKeep stencilOpKeep stencilOpKeep comparisonFuncNever
data DepthStencilDesc = DepthStencilDesc
  { depthStencilDepthEnable :: Bool
  , depthStencilDepthWriteMask :: DepthWriteMask
  , depthStencilDepthFunc :: ComparisonFunc
  , depthStencilStencilEnable :: Bool
  , depthStencilStencilReadMask :: Word8
  , depthStencilStencilWriteMask :: Word8
  , depthStencilFrontFace :: StencilOpDesc
  , depthStencilBackFace :: StencilOpDesc
  }
depthStencilDescDisabled :: DepthStencilDesc
depthStencilDescDisabled = DepthStencilDesc False depthWriteMaskZero comparisonFuncAlways False 0 0 noopStencilOpDesc noopStencilOpDesc
instance Storable DepthStencilDesc where
  sizeOf _ = 4 * (5 + 4 * 2)
  alignment _ = 4
  peek p = DepthStencilDesc <$>
    fmap MarshalUtils.toBool (peek $ castPtr p :: IO BOOL) <*>
    peek (plusPtr p 4) <*>
    peek (plusPtr p 8) <*>
    fmap MarshalUtils.toBool (peek $ plusPtr p 12 :: IO BOOL) <*>
    peek (plusPtr p 16) <*>
    peek (plusPtr p 17) <*>
    peek (plusPtr p 20) <*>
    peek (plusPtr p $ 20 + sizeOf (undefined :: StencilOpDesc))
  poke p s = do
    poke (castPtr p) (MarshalUtils.fromBool $ depthStencilDepthEnable s :: BOOL)
    poke (plusPtr p 4) $ depthStencilDepthWriteMask s
    poke (plusPtr p 8) $ depthStencilDepthFunc s
    poke (plusPtr p 12) (MarshalUtils.fromBool $ depthStencilStencilEnable s :: BOOL)
    poke (plusPtr p 16) $ depthStencilStencilReadMask s
    poke (plusPtr p 17) $ depthStencilStencilWriteMask s
    poke (plusPtr p 20) $ depthStencilFrontFace s
    poke (plusPtr p $ 20 + sizeOf (undefined :: StencilOpDesc)) $ depthStencilBackFace s
type PrimitiveTopologyType = CInt
primitiveTopologyTypeUndefined, primitiveTopologyTypePoint, primitiveTopologyTypeLine, primitiveTopologyTypeTriangle, primitiveTopologyTypePatch :: PrimitiveTopologyType
primitiveTopologyTypeUndefined = 0
primitiveTopologyTypePoint = 1
primitiveTopologyTypeLine = 2
primitiveTopologyTypeTriangle = 3
primitiveTopologyTypePatch = 4
type InputClassification = CInt
inputClassificationPerVertex, inputClassificationPerInstance :: InputClassification
inputClassificationPerVertex = 0
inputClassificationPerInstance = 1
data InputElementDesc = InputElementDesc
  { inputElementSemanticName :: LPCSTR
  , inputElementSemanticIndex :: CUInt
  , inputElementFormat :: DxgiConst.Format
  , inputElementInputSlot :: CUInt
  , inputElementAlignedByteOffset :: CUInt
  , inputElementInputSlotClass :: InputClassification
  , inputElementInstanceDataStepRate :: CUInt
  }
instance Storable InputElementDesc where
  sizeOf _ = 4 * 8
  alignment _ = 8
  peek p = InputElementDesc <$>
    peek (castPtr p) <*>
    peek (plusPtr p 8) <*>
    peek (plusPtr p 12) <*>
    peek (plusPtr p 16) <*>
    peek (plusPtr p 20) <*>
    peek (plusPtr p 24) <*>
    peek (plusPtr p 28)
  poke p e = do
    poke (castPtr p) $ inputElementSemanticName e
    poke (plusPtr p 8) $ inputElementSemanticIndex e
    poke (plusPtr p 12) $ inputElementFormat e
    poke (plusPtr p 16) $ inputElementInputSlot e
    poke (plusPtr p 20) $ inputElementAlignedByteOffset e
    poke (plusPtr p 24) $ inputElementInputSlotClass e
    poke (plusPtr p 28) $ inputElementInstanceDataStepRate e
data InputLayoutDesc = InputLayoutDesc (Ptr InputElementDesc) CUInt
instance Storable InputLayoutDesc where
  -- 構造体のサイズもalignされる
  sizeOf _ = sizeOf (undefined :: Ptr a) + 8
  alignment _ = alignment (undefined :: Ptr a)
  peek p = InputLayoutDesc <$> peek (castPtr p) <*> peek (plusPtr p $ sizeOf (undefined :: Ptr a))
  poke p (InputLayoutDesc pp len) = poke (castPtr p) pp *> poke (plusPtr p $ sizeOf (undefined :: Ptr a)) len
type IndexBufferStripCutValue = CInt
indexBufferStripCutValueDisabled, indexBufferStripCutValue16, indexBufferStripCutValue32 :: IndexBufferStripCutValue
indexBufferStripCutValueDisabled = 0
indexBufferStripCutValue16 = 1
indexBufferStripCutValue32 = 2
data CachedPipelineState = CachedPipelineState (Ptr ()) WordPtr
emptyCachedPipelineState :: CachedPipelineState
emptyCachedPipelineState = CachedPipelineState nullPtr 0
instance Storable CachedPipelineState where
  sizeOf _ = sizeOf (undefined :: WordPtr) * 2
  alignment _ = alignment (undefined :: WordPtr)
  peek p = CachedPipelineState <$> peek (castPtr p) <*> peek (plusPtr p $ sizeOf (undefined :: WordPtr))
  poke p (CachedPipelineState bp sz) = poke (castPtr p) bp *> poke (plusPtr p $ sizeOf (undefined :: WordPtr)) sz
type PipelineStateFlags = CInt
data GraphicsPipelineStateDesc = GraphicsPipelineStateDesc
  { gpsDescRootSignature :: Ptr ID3D12RootSignature
  , gpsDescVS :: ShaderBytecode
  , gpsDescPS :: ShaderBytecode
  , gpsDescDS :: ShaderBytecode
  , gpsDescHS :: ShaderBytecode
  , gpsDescGS :: ShaderBytecode
  , gpsDescStreamOutput :: StreamOutputDesc
  , gpsDescBlendState :: BlendDesc
  , gpsDescSampleMask :: CUInt
  , gpsDescRasterizerState :: RasterizerState
  , gpsDescDepthStencilState :: DepthStencilDesc
  , gpsDescInputLayout :: InputLayoutDesc
  , gpsDescIBStripCutValue :: IndexBufferStripCutValue
  , gpsDescPrimitiveTopologyType :: PrimitiveTopologyType
  , gpsDescNumRenderTargets :: CUInt
  , gpsDescRTVFormats :: [DxgiConst.Format]
  , gpsDescDSVFormat :: DxgiConst.Format
  , gpsDescSampleDesc :: DxgiStruct.SampleDesc
  , gpsDescNodeMask :: CUInt
  , gpsDescCachedPSO :: CachedPipelineState
  , gpsDescFlags :: PipelineStateFlags
  }
align :: Storable a => a -> Int -> Int
align v base = let a = alignment v in (base + (a - 1)) .&. complement (a - 1)
_GPSD_OFFSET_ROOT_SIGNATURE = 0
_GPSD_OFFSET_VS = align (undefined :: ShaderBytecode) $ _GPSD_OFFSET_ROOT_SIGNATURE + sizeOf (undefined :: Ptr ID3D12RootSignature)
_GPSD_OFFSET_PS = align (undefined :: ShaderBytecode) $ _GPSD_OFFSET_VS + sizeOf (undefined :: ShaderBytecode)
_GPSD_OFFSET_DS = align (undefined :: ShaderBytecode) $ _GPSD_OFFSET_PS + sizeOf (undefined :: ShaderBytecode)
_GPSD_OFFSET_HS = align (undefined :: ShaderBytecode) $ _GPSD_OFFSET_DS + sizeOf (undefined :: ShaderBytecode)
_GPSD_OFFSET_GS = align (undefined :: ShaderBytecode) $ _GPSD_OFFSET_HS + sizeOf (undefined :: ShaderBytecode)
_GPSD_OFFSET_STREAM_OUTPUT_DESC = align (undefined :: StreamOutputDesc) $ _GPSD_OFFSET_GS + sizeOf (undefined :: ShaderBytecode)
_GPSD_OFFSET_BLEND_STATE = align (undefined :: BlendDesc) $ _GPSD_OFFSET_STREAM_OUTPUT_DESC + sizeOf (undefined :: StreamOutputDesc)
_GPSD_OFFSET_SAMPLE_MASK = align (undefined :: CUInt) $ _GPSD_OFFSET_BLEND_STATE + sizeOf (undefined :: BlendDesc)
_GPSD_OFFSET_RASTERIZER_STATE = align (undefined :: RasterizerState) $ _GPSD_OFFSET_SAMPLE_MASK + sizeOf (undefined :: CUInt)
_GPSD_OFFSET_DEPTH_STENCIL_STATE = align (undefined :: DepthStencilDesc) $ _GPSD_OFFSET_RASTERIZER_STATE + sizeOf (undefined :: RasterizerState)
_GPSD_OFFSET_INPUT_LAYOUT = align (undefined :: InputLayoutDesc) $ _GPSD_OFFSET_DEPTH_STENCIL_STATE + sizeOf (undefined :: DepthStencilDesc)
_GPSD_OFFSET_IB_STRIP_CUT_VALUE = align (undefined :: IndexBufferStripCutValue) $ _GPSD_OFFSET_INPUT_LAYOUT + sizeOf (undefined :: InputLayoutDesc)
_GPSD_OFFSET_PRIMITIVE_TOPOLOGY_TYPE = align (undefined :: PrimitiveTopologyType) $ _GPSD_OFFSET_IB_STRIP_CUT_VALUE + sizeOf (undefined :: IndexBufferStripCutValue)
_GPSD_OFFSET_NUM_RENDER_TARGETS = align (undefined :: CUInt) $ _GPSD_OFFSET_PRIMITIVE_TOPOLOGY_TYPE + sizeOf (undefined :: PrimitiveTopologyType)
_GPSD_OFFSET_RTV_FORMATS = align (undefined :: DxgiConst.Format) $ _GPSD_OFFSET_NUM_RENDER_TARGETS + sizeOf (undefined :: CUInt)
_GPSD_OFFSET_DSV_FORMAT = align (undefined :: DxgiConst.Format) $ _GPSD_OFFSET_RTV_FORMATS + (sizeOf (undefined :: DxgiConst.Format) * 8)
_GPSD_OFFSET_SAMPLE_DESC = align (undefined :: DxgiStruct.SampleDesc) $ _GPSD_OFFSET_DSV_FORMAT + sizeOf (undefined :: DxgiConst.Format)
_GPSD_OFFSET_NODE_MASK = align (undefined :: CUInt) $ _GPSD_OFFSET_SAMPLE_DESC + sizeOf (undefined :: DxgiStruct.SampleDesc)
_GPSD_OFFSET_CACHED_PSO = align (undefined :: CachedPipelineState) $ _GPSD_OFFSET_NODE_MASK + sizeOf (undefined :: CUInt)
_GPSD_OFFSET_FLAGS = align (undefined :: PipelineStateFlags) $ _GPSD_OFFSET_CACHED_PSO + sizeOf (undefined :: CachedPipelineState)
instance Storable GraphicsPipelineStateDesc where
  sizeOf _ = _GPSD_OFFSET_FLAGS + sizeOf (undefined :: PipelineStateFlags)
  alignment _ = 8
  peek p = GraphicsPipelineStateDesc <$>
    peek (plusPtr p _GPSD_OFFSET_ROOT_SIGNATURE) <*>
    peek (plusPtr p _GPSD_OFFSET_VS) <*>
    peek (plusPtr p _GPSD_OFFSET_PS) <*>
    peek (plusPtr p _GPSD_OFFSET_DS) <*>
    peek (plusPtr p _GPSD_OFFSET_HS) <*>
    peek (plusPtr p _GPSD_OFFSET_GS) <*>
    peek (plusPtr p _GPSD_OFFSET_STREAM_OUTPUT_DESC) <*>
    peek (plusPtr p _GPSD_OFFSET_BLEND_STATE) <*>
    peek (plusPtr p _GPSD_OFFSET_SAMPLE_MASK) <*>
    peek (plusPtr p _GPSD_OFFSET_RASTERIZER_STATE) <*>
    peek (plusPtr p _GPSD_OFFSET_DEPTH_STENCIL_STATE) <*>
    peek (plusPtr p _GPSD_OFFSET_INPUT_LAYOUT) <*>
    peek (plusPtr p _GPSD_OFFSET_IB_STRIP_CUT_VALUE) <*>
    peek (plusPtr p _GPSD_OFFSET_PRIMITIVE_TOPOLOGY_TYPE) <*>
    peek (plusPtr p _GPSD_OFFSET_NUM_RENDER_TARGETS) <*>
    peekArray 8 (plusPtr p _GPSD_OFFSET_RTV_FORMATS) <*>
    peek (plusPtr p _GPSD_OFFSET_DSV_FORMAT) <*>
    peek (plusPtr p _GPSD_OFFSET_SAMPLE_DESC) <*>
    peek (plusPtr p _GPSD_OFFSET_NODE_MASK) <*>
    peek (plusPtr p _GPSD_OFFSET_CACHED_PSO) <*>
    peek (plusPtr p _GPSD_OFFSET_FLAGS)
  poke p gp = do
    poke (plusPtr p _GPSD_OFFSET_ROOT_SIGNATURE) $ gpsDescRootSignature gp
    poke (plusPtr p _GPSD_OFFSET_VS) $ gpsDescVS gp
    poke (plusPtr p _GPSD_OFFSET_PS) $ gpsDescPS gp
    poke (plusPtr p _GPSD_OFFSET_DS) $ gpsDescDS gp
    poke (plusPtr p _GPSD_OFFSET_HS) $ gpsDescHS gp
    poke (plusPtr p _GPSD_OFFSET_GS) $ gpsDescGS gp
    poke (plusPtr p _GPSD_OFFSET_STREAM_OUTPUT_DESC) $ gpsDescStreamOutput gp
    poke (plusPtr p _GPSD_OFFSET_BLEND_STATE) $ gpsDescBlendState gp
    poke (plusPtr p _GPSD_OFFSET_SAMPLE_MASK) $ gpsDescSampleMask gp
    poke (plusPtr p _GPSD_OFFSET_RASTERIZER_STATE) $ gpsDescRasterizerState gp
    poke (plusPtr p _GPSD_OFFSET_DEPTH_STENCIL_STATE) $ gpsDescDepthStencilState gp
    poke (plusPtr p _GPSD_OFFSET_INPUT_LAYOUT) $ gpsDescInputLayout gp
    poke (plusPtr p _GPSD_OFFSET_IB_STRIP_CUT_VALUE) $ gpsDescIBStripCutValue gp
    poke (plusPtr p _GPSD_OFFSET_PRIMITIVE_TOPOLOGY_TYPE) $ gpsDescPrimitiveTopologyType gp
    poke (plusPtr p _GPSD_OFFSET_NUM_RENDER_TARGETS) $ gpsDescNumRenderTargets gp
    pokeArray (plusPtr p _GPSD_OFFSET_RTV_FORMATS) $ take 8 $ gpsDescRTVFormats gp ++ repeat DxgiConst.formatUnknown
    poke (plusPtr p _GPSD_OFFSET_DSV_FORMAT) $ gpsDescDSVFormat gp
    poke (plusPtr p _GPSD_OFFSET_SAMPLE_DESC) $ gpsDescSampleDesc gp
    poke (plusPtr p _GPSD_OFFSET_NODE_MASK) $ gpsDescNodeMask gp
    poke (plusPtr p _GPSD_OFFSET_CACHED_PSO) $ gpsDescCachedPSO gp
    poke (plusPtr p _GPSD_OFFSET_FLAGS) $ gpsDescFlags gp

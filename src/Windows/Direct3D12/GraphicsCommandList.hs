{-# LANGUAGE TypeFamilies #-}

module Windows.Direct3D12.GraphicsCommandList
  ( ID3D12GraphicsCommandList, close, copyBufferRegion, setGraphicsRootSignature, setPrimitiveTopology, drawInstanced, setVertexBuffers
  , resourceBarrier, setViewports, setScissorRects, setRenderTargets, clearRenderTargetView, clearEntireRenderTargetView
  , ResourceBarrier(..)
  ) where

import Windows.Types (HRESULT, BOOL)
import qualified Windows.Const.HResult as HR
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr, nullPtr, WordPtr(..))
import Foreign.C.Types (CLong(..), CUInt(..), CInt(..), CFloat(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Data.Ix (Ix)
import Data.Array.Base (getNumElements)
import Data.Array.Storable (StorableArray, withStorableArray)
import Data.Word (Word64)
import Windows.Direct3D12.Resource (ID3D12Resource, ResourceStates)
import Windows.Direct3D12.RootSignature (ID3D12RootSignature)
import Windows.Const.Direct3D12 (PrimitiveTopology)
import Windows.Struct.Direct3D12 (Viewport, CPUDescriptorHandle(..), VertexBufferView)
import Windows.Struct.Rect (Rect)
import Windows.Com.Monad (ComT, handleHRESULT)
import Control.Monad.IO.Class (liftIO)

data ID3D12GraphicsCommandListVtbl
newtype ID3D12GraphicsCommandList = ID3D12GraphicsCommandList (Ptr ID3D12GraphicsCommandListVtbl)
instance Storable ID3D12GraphicsCommandList where
  sizeOf _ = 8
  alignment _ = 8
  peek p = ID3D12GraphicsCommandList <$> peek (castPtr p)
  poke p (ID3D12GraphicsCommandList vp) = castPtr p `poke` vp
instance ComInterface ID3D12GraphicsCommandList where
  type VTable ID3D12GraphicsCommandList = ID3D12GraphicsCommandListVtbl
  guid _ = GUID 0x5b160d0f 0xac1b 0x4185 0x8ba8b3ae42a5a455

type ResourceBarrierFlags = CUInt
data ResourceBarrier =
  ResourceTransitionBarrier ResourceBarrierFlags (Ptr ID3D12Resource) CUInt ResourceStates ResourceStates |
  ResourceAliasingBarrier ResourceBarrierFlags (Ptr ID3D12Resource) (Ptr ID3D12Resource) |
  ResourceUAVBarrier ResourceBarrierFlags (Ptr ID3D12Resource)
instance Storable ResourceBarrier where
  sizeOf _ = 7 * 4
  alignment _ = 8
  peek p = do
    ty <- peek $ castPtr p :: IO CUInt
    flags <- peek $ plusPtr p 4
    case ty of
      0 -> ResourceTransitionBarrier flags <$> peek (plusPtr p 8) <*> peek (plusPtr p 16) <*> peek (plusPtr p 20) <*> peek (plusPtr p 24)
      1 -> ResourceAliasingBarrier flags <$> peek (plusPtr p 8) <*> peek (plusPtr p 16)
      2 -> ResourceUAVBarrier flags <$> peek (plusPtr p 8)
  poke p (ResourceTransitionBarrier flags res subres before after) = do
    poke (castPtr p) (0 :: CUInt)
    poke (plusPtr p 4) flags
    poke (plusPtr p 8) res
    poke (plusPtr p 16) subres
    poke (plusPtr p 20) before
    poke (plusPtr p 24) after
  poke p (ResourceAliasingBarrier flags before after) = do
    poke (castPtr p) (1 :: CUInt)
    poke (plusPtr p 4) flags
    poke (plusPtr p 8) before
    poke (plusPtr p 16) after
  poke p (ResourceUAVBarrier flags res) = poke (castPtr p) (2 :: CUInt) *> poke (plusPtr p 4) flags *> poke (plusPtr p 8) res

_VTBL_INDEX_CLOSE = 9
type PFN_Close = Ptr ID3D12GraphicsCommandList -> IO HRESULT
foreign import ccall "dynamic" dcall_this :: FunPtr (Ptr ID3D12GraphicsCommandList -> IO HRESULT) -> Ptr ID3D12GraphicsCommandList -> IO HRESULT
close :: Ptr ID3D12GraphicsCommandList -> ComT IO ()
close this = liftIO (getFunctionPtr _VTBL_INDEX_CLOSE this >>= flip dcall_this this) >>= handleHRESULT

_VTBL_INDEX_DRAW_INSTANCED = 12
type PFN_DrawInstanced = Ptr ID3D12GraphicsCommandList -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()
foreign import ccall "dynamic" dcall_drawInstanced :: FunPtr PFN_DrawInstanced -> PFN_DrawInstanced
drawInstanced :: Ptr ID3D12GraphicsCommandList -> Int -> Int -> Int -> Int -> IO ()
drawInstanced this vertexCount instanceCount startVertex startInstance = do
  fn <- dcall_drawInstanced <$> getFunctionPtr _VTBL_INDEX_DRAW_INSTANCED this
  fn this (fromIntegral vertexCount) (fromIntegral instanceCount) (fromIntegral startVertex) (fromIntegral startInstance)

_VTBL_COPY_BUFFER_REGION = 15
type PFN_CopyBufferRegion = Ptr ID3D12GraphicsCommandList -> Ptr ID3D12Resource -> Word64 -> Ptr ID3D12Resource -> Word64 -> Word64 -> IO ()
foreign import ccall "dynamic" dcall_copyBufferRegion :: FunPtr PFN_CopyBufferRegion -> PFN_CopyBufferRegion
copyBufferRegion :: Ptr ID3D12GraphicsCommandList -> Ptr ID3D12Resource -> Word64 -> Ptr ID3D12Resource -> Word64 -> Word64 -> IO ()
copyBufferRegion this dstBuffer dstOffset srcBuffer srcOffset numBytes =
  getFunctionPtr _VTBL_COPY_BUFFER_REGION this >>= \f -> dcall_copyBufferRegion f this dstBuffer dstOffset srcBuffer srcOffset numBytes

_VTBL_INDEX_SET_PRIMITIVE_TOPOLOGY = 20
type PFN_IASetPrimitiveTopology = Ptr ID3D12GraphicsCommandList -> PrimitiveTopology -> IO ()
foreign import ccall "dynamic" dcall_setPrimitiveTopology :: FunPtr PFN_IASetPrimitiveTopology -> PFN_IASetPrimitiveTopology
setPrimitiveTopology :: Ptr ID3D12GraphicsCommandList -> PrimitiveTopology -> IO ()
setPrimitiveTopology this pt = getFunctionPtr _VTBL_INDEX_SET_PRIMITIVE_TOPOLOGY this >>= \f -> dcall_setPrimitiveTopology f this pt

type PFN_MultipleSetter s = Ptr ID3D12GraphicsCommandList -> CUInt -> Ptr s -> IO ()
foreign import ccall "dynamic" dcall_multipleSetter :: FunPtr (PFN_MultipleSetter s) -> (PFN_MultipleSetter s)
_VTBL_INDEX_RS_SET_VIEWPORTS = 21
_VTBL_INDEX_RS_SET_SCISSOR_RECTS = 22
setViewports :: Ix i => Ptr ID3D12GraphicsCommandList -> StorableArray i Viewport -> IO ()
setScissorRects :: Ix i => Ptr ID3D12GraphicsCommandList -> StorableArray i Rect -> IO ()
setViewports this viewports = withStorableArray viewports $ \vpref -> do
  fn <- dcall_multipleSetter <$> getFunctionPtr _VTBL_INDEX_RS_SET_VIEWPORTS this
  count <- fromIntegral <$> getNumElements viewports
  fn this count vpref
setScissorRects this rects = withStorableArray rects $ \ref -> do
  fn <- dcall_multipleSetter <$> getFunctionPtr _VTBL_INDEX_RS_SET_SCISSOR_RECTS this
  count <- fromIntegral <$> getNumElements rects
  fn this count ref

_VTBL_INDEX_RESOURCE_BARRIER = 26
type PFN_ResourceBarrier = Ptr ID3D12GraphicsCommandList -> CUInt -> Ptr ResourceBarrier -> IO ()
foreign import ccall "dynamic" dcall_resourceBarrier :: FunPtr PFN_ResourceBarrier -> PFN_ResourceBarrier
resourceBarrier :: Ix i => Ptr ID3D12GraphicsCommandList -> StorableArray i ResourceBarrier -> IO ()
resourceBarrier this barriers = withStorableArray barriers $ \barriersRef -> do
  fn <- dcall_resourceBarrier <$> getFunctionPtr _VTBL_INDEX_RESOURCE_BARRIER this
  barrierCount <- fromIntegral <$> getNumElements barriers
  fn this barrierCount barriersRef

_VTBL_INDEX_SET_GRAPHICS_ROOT_SIGNATURE = 30
type PFN_SetGraphicsRootSignature = Ptr ID3D12GraphicsCommandList -> Ptr ID3D12RootSignature -> IO ()
foreign import ccall "dynamic" dcall_setGraphicsRootSignature :: FunPtr PFN_SetGraphicsRootSignature -> PFN_SetGraphicsRootSignature
setGraphicsRootSignature :: Ptr ID3D12GraphicsCommandList -> Ptr ID3D12RootSignature -> IO ()
setGraphicsRootSignature this rs = getFunctionPtr _VTBL_INDEX_SET_GRAPHICS_ROOT_SIGNATURE this >>= \f -> dcall_setGraphicsRootSignature f this rs

_VTBL_INDEX_SET_VERTEX_BUFFERS = 44
type PFN_IASetVertexBuffers = Ptr ID3D12GraphicsCommandList -> CUInt -> CUInt -> Ptr VertexBufferView -> IO ()
foreign import ccall "dynamic" dcall_setVertexBuffers :: FunPtr PFN_IASetVertexBuffers -> PFN_IASetVertexBuffers
setVertexBuffers :: Ix i => Ptr ID3D12GraphicsCommandList -> Int -> StorableArray i VertexBufferView -> IO ()
setVertexBuffers this startSlot views = withStorableArray views $ \viewRef -> do
  fn <- dcall_setVertexBuffers <$> getFunctionPtr _VTBL_INDEX_SET_VERTEX_BUFFERS this
  viewCount <- fromIntegral <$> getNumElements views
  fn this (fromIntegral startSlot) viewCount viewRef

_VTBL_INDEX_SET_RENDER_TARGETS = 46
type PFN_OMSetRenderTargets = Ptr ID3D12GraphicsCommandList -> CUInt -> Ptr CPUDescriptorHandle -> BOOL -> Ptr CPUDescriptorHandle -> IO ()
foreign import ccall "dynamic" dcall_setRenderTargets :: FunPtr PFN_OMSetRenderTargets -> PFN_OMSetRenderTargets
setRenderTargets :: Ix i => Ptr ID3D12GraphicsCommandList -> StorableArray i CPUDescriptorHandle -> Maybe CPUDescriptorHandle -> IO ()
setRenderTargets this rts Nothing = withStorableArray rts $ \renderTargetsRef -> do
  fn <- dcall_setRenderTargets <$> getFunctionPtr _VTBL_INDEX_SET_RENDER_TARGETS this
  count <- fromIntegral <$> getNumElements rts
  fn this count renderTargetsRef 0 nullPtr
setRenderTargets this rts (Just dsv) =
  withStorableArray rts $ \renderTargetsRef ->
  alloca $ \dsvref -> do
    fn <- dcall_setRenderTargets <$> getFunctionPtr _VTBL_INDEX_SET_RENDER_TARGETS this
    count <- fromIntegral <$> getNumElements rts
    poke dsvref dsv
    fn this count renderTargetsRef 0 dsvref

_VTBL_INDEX_CLEAR_RENDER_TARGET_VIEW = 48
type PFN_ClearRenderTargetView = Ptr ID3D12GraphicsCommandList -> CPUDescriptorHandle -> Ptr CFloat -> CUInt -> Ptr Rect -> IO ()
foreign import ccall "dynamic" dcall_clearRenderTargetView :: FunPtr PFN_ClearRenderTargetView -> PFN_ClearRenderTargetView
clearEntireRenderTargetView :: Ptr ID3D12GraphicsCommandList -> CPUDescriptorHandle -> (Float, Float, Float, Float) -> IO ()
clearRenderTargetView :: Ix i => Ptr ID3D12GraphicsCommandList -> CPUDescriptorHandle -> (Float, Float, Float, Float) -> StorableArray i Rect -> IO ()
clearEntireRenderTargetView this rtv (r, g, b, a) = withArray (map realToFrac [r, g, b, a]) $ \cref -> do
  fn <- dcall_clearRenderTargetView <$> getFunctionPtr _VTBL_INDEX_CLEAR_RENDER_TARGET_VIEW this
  fn this rtv cref 0 nullPtr
clearRenderTargetView this rtv (r, g, b, a) rects =
  withArray (map realToFrac [r, g, b, a]) $ \cref ->
  withStorableArray rects $ \rectsRef -> do
    fn <- dcall_clearRenderTargetView <$> getFunctionPtr _VTBL_INDEX_CLEAR_RENDER_TARGET_VIEW this
    count <- fromIntegral <$> getNumElements rects
    fn this rtv cref count rectsRef

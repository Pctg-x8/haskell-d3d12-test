module Windows.Struct.Direct3D12 where

import Foreign.C.Types (CFloat(..), CUInt)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr, plusPtr, WordPtr)
import Data.Word (Word64, Word8)
import Windows.Const.Dxgi (Format)

data Viewport = Viewport
  { viewportTopLeftX :: CFloat, viewportTopLeftY :: CFloat
  , viewportWidth :: CFloat, viewportHeight :: CFloat
  , viewportMinDepth :: CFloat, viewportMaxDepth :: CFloat
  } deriving Show
instance Storable Viewport where
  sizeOf _ = 4 * 6
  alignment _ = 4
  peek p = Viewport <$>
    peek (castPtr p) <*> peek (plusPtr p 4) <*>
    peek (plusPtr p 8) <*> peek (plusPtr p 12) <*>
    peek (plusPtr p 16) <*> peek (plusPtr p 20)
  poke p (Viewport topLeftX topLeftY width height minDepth maxDepth) =
    poke (castPtr p) topLeftX *> poke (plusPtr p 4) topLeftY *>
    poke (plusPtr p 8) width *> poke (plusPtr p 12) height *>
    poke (plusPtr p 16) minDepth *> poke (plusPtr p 20) maxDepth

newtype CPUDescriptorHandle = CPUDescriptorHandle WordPtr deriving Show
newtype GPUDescriptorHandle = GPUDescriptorHandle Word64 deriving Show
instance Storable CPUDescriptorHandle where
  sizeOf (CPUDescriptorHandle a) = sizeOf a
  alignment (CPUDescriptorHandle a) = alignment a
  peek p = CPUDescriptorHandle <$> peek (castPtr p)
  poke p (CPUDescriptorHandle v) = castPtr p `poke` v
instance Storable GPUDescriptorHandle where
  sizeOf (GPUDescriptorHandle a) = sizeOf a
  alignment (GPUDescriptorHandle a) = alignment a
  peek p = GPUDescriptorHandle <$> peek (castPtr p)
  poke p (GPUDescriptorHandle v) = castPtr p `poke` v

plusCPUDescriptorHandle :: CPUDescriptorHandle -> Int -> CPUDescriptorHandle
plusCPUDescriptorHandle (CPUDescriptorHandle base) offset = CPUDescriptorHandle $ base + fromIntegral offset

data ClearValue = ClearValueColor Format CFloat CFloat CFloat CFloat | ClearValueDepthStencil Format CFloat Word8
instance Storable ClearValue where
  sizeOf _ = 20
  alignment _ = 4
  peek p = error "Cannot peek ClearValue: both color or depthStencil cannot be estimated from data"
  poke p (ClearValueColor fmt r g b a) = do
    poke (castPtr p) fmt
    poke (plusPtr p 4) r
    poke (plusPtr p 8) g
    poke (plusPtr p 12) b
    poke (plusPtr p 16) a
  poke p (ClearValueDepthStencil fmt d s) = poke (castPtr p) fmt *> poke (plusPtr p 4) d *> poke (plusPtr p 8) s
  
data Range = Range WordPtr WordPtr
instance Storable Range where
  sizeOf _ = sizeOf (undefined :: WordPtr) * 2
  alignment _ = alignment (undefined :: WordPtr)
  peek p = Range <$> peek (castPtr p) <*> peek (plusPtr p $ sizeOf (undefined :: WordPtr))
  poke p (Range begin end) = poke (castPtr p) begin *> poke (plusPtr p $ sizeOf (undefined :: WordPtr)) end

data VertexBufferView = VertexBufferView Word64 CUInt CUInt
instance Storable VertexBufferView where
  sizeOf _ = 8 * 2
  alignment _ = 8
  peek p = VertexBufferView <$> peek (castPtr p) <*> peek (plusPtr p 8) <*> peek (plusPtr p 12)
  poke p (VertexBufferView loc size stride) = poke (castPtr p) loc *> poke (plusPtr p 8) size *> poke (plusPtr p 12) stride

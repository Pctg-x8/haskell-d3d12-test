module Windows.Struct.Direct3D12 where

import Foreign.C.Types (CFloat(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr, plusPtr, WordPtr)
import Data.Word (Word64)

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

module Windows.Struct.Dxgi where

import Windows.Types (BOOL)
import Foreign.C.Types (CUInt)
import qualified Windows.Const.Dxgi as DxgiConst
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, plusPtr)

data SampleDesc = SampleDesc { sampleCount :: CUInt, sampleQuality :: CUInt }
instance Storable SampleDesc where
  sizeOf _ = 8
  alignment _ = 4
  peek p = SampleDesc <$> peek (castPtr p) <*> peek (plusPtr p 4)
  poke p (SampleDesc c q) = castPtr p `poke` c *> plusPtr p 4 `poke` q
defaultSampleDesc :: SampleDesc
defaultSampleDesc = SampleDesc 1 0

data SwapChainDesc1 = SwapChainDesc1
  { swapChain1Width :: CUInt, swapChain1Height :: CUInt, swapChain1Format :: DxgiConst.Format
  , swapChain1Stereo :: BOOL, swapChain1SampleDesc :: SampleDesc, swapChain1BufferUsage :: DxgiConst.Usage
  , swapChain1BufferCount :: CUInt, swapChain1Scaling :: DxgiConst.Scaling, swapChain1SwapEffect :: DxgiConst.SwapEffect
  , swapChain1AlphaMode :: DxgiConst.AlphaMode, swapChain1Flags :: CUInt
  }
instance Storable SwapChainDesc1 where
  sizeOf _ = 4 * 12
  alignment _ = 4
  peek p = SwapChainDesc1 <$>
    peek (castPtr p) <*>
    peek (plusPtr p 4) <*>
    peek (plusPtr p 8) <*>
    peek (plusPtr p 12) <*>
    peek (plusPtr p 16) <*> -- sampleDesc
    peek (plusPtr p 24) <*>
    peek (plusPtr p 28) <*>
    peek (plusPtr p 32) <*>
    peek (plusPtr p 36) <*>
    peek (plusPtr p 40) <*>
    peek (plusPtr p 44)
  poke p d = castPtr p `poke` swapChain1Width d *>
    plusPtr p 4 `poke` swapChain1Height d *>
    plusPtr p 8 `poke` swapChain1Format d *>
    plusPtr p 12 `poke` swapChain1Stereo d *>
    plusPtr p 16 `poke` swapChain1SampleDesc d *>
    plusPtr p 24 `poke` swapChain1BufferUsage d *>
    plusPtr p 28 `poke` swapChain1BufferCount d *>
    plusPtr p 32 `poke` swapChain1Scaling d *>
    plusPtr p 36 `poke` swapChain1SwapEffect d *>
    plusPtr p 40 `poke` swapChain1AlphaMode d *>
    plusPtr p 44 `poke` swapChain1Flags d
defaultSwapChainDesc1 :: (Int, Int) -> DxgiConst.Format -> DxgiConst.Usage -> Int -> DxgiConst.SwapEffect -> SwapChainDesc1
defaultSwapChainDesc1 (w, h) format bufferUsage bufferCount swapEffect = SwapChainDesc1
  (fromIntegral w) (fromIntegral h) format 0 defaultSampleDesc bufferUsage (fromIntegral bufferCount)
  DxgiConst.scalingStretch swapEffect DxgiConst.alphaModePremultiplied 0


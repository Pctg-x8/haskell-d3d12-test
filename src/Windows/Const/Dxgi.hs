module Windows.Const.Dxgi where

import Foreign.C.Types (CUInt)

type Format = CUInt
formatR8G8B8A8Unorm :: Format
formatR8G8B8A8Unorm = 28

type Usage = CUInt
usageRenderTargetOutput :: Usage
usageRenderTargetOutput = 0x00000020

type Scaling = CUInt
scalingStretch, scalingNone, scalingAspectRatioStretch :: Scaling
scalingStretch = 0
scalingNone = 1
scalingAspectRatioStretch = 2

type SwapEffect = CUInt
swapEffectDiscard, swapEffectSequential, swapEffectFlipSequential, swapEffectFlipDiscard :: SwapEffect
swapEffectDiscard = 0
swapEffectSequential = 1
swapEffectFlipSequential = 3
swapEffectFlipDiscard = 4

type AlphaMode = CUInt
alphaModeUnspecified, alphaModePremultiplied, alphaModeStraight, alphaModeIgnore :: AlphaMode
alphaModeUnspecified = 0
alphaModePremultiplied = 1
alphaModeStraight = 2
alphaModeIgnore = 3

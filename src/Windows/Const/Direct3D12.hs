module Windows.Const.Direct3D12 where

import Foreign.C.Types (CInt)

type Filter = CInt
filterMinMagMipPoint :: Filter
filterMinMagMipPoint = 0

type TextureAddressMode = CInt
textureAddressModeWrap, textureAddressModeMirror, textureAddressModeClamp, textureAddressModeBorder, textureAddressModeMirrorOnce :: TextureAddressMode
textureAddressModeWrap = 1
textureAddressModeMirror = 2
textureAddressModeClamp = 3
textureAddressModeBorder = 4
textureAddressModeMirrorOnce = 5

type ComparisonFunc = CInt
comparisonFuncNever, comparisonFuncLess, comparisonFuncEqual, comparisonFuncLessEqual :: ComparisonFunc
comparisonFuncGreater, comparisonFuncNotEqual, comparisonFuncGreaterEqual, comparisonFuncAlways :: ComparisonFunc
comparisonFuncNever = 1
comparisonFuncLess = 2
comparisonFuncEqual = 3
comparisonFuncLessEqual = 4
comparisonFuncGreater = 5
comparisonFuncNotEqual = 6
comparisonFuncGreaterEqual = 7
comparisonFuncAlways = 8

type StaticBorderColor = CInt
staticBorderColorTransparentBlack, staticBorderColorOpaqueBlack, staticBorderColorOpaqueWhite :: StaticBorderColor
staticBorderColorTransparentBlack = 0
staticBorderColorOpaqueBlack = staticBorderColorTransparentBlack + 1
staticBorderColorOpaqueWhite = staticBorderColorOpaqueBlack + 1

type PrimitiveTopology = CInt
primitiveTopologyTriangleList :: PrimitiveTopology
primitiveTopologyTriangleList = 4

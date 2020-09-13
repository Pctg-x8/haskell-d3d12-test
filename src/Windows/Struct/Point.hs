module Windows.Struct.Point where

import Windows.Types (LONG)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (plusPtr, castPtr)

data POINT = POINT { x :: LONG, y :: LONG }
instance Storable POINT where
  sizeOf _ = 4 * 2
  alignment _ = 4
  peek p = POINT <$> (peek $ castPtr p) <*> (peek $ plusPtr p 4)
  poke p (POINT x y) = poke (castPtr p) x >> poke (plusPtr p 4) y

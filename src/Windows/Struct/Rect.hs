module Windows.Struct.Rect where

import Foreign.C.Types (CLong)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr, plusPtr)

data Rect = Rect { left :: CLong, top :: CLong, right :: CLong, bottom :: CLong }
instance Storable Rect where
  sizeOf _ = 4 * 4
  alignment _ = 4
  peek p = Rect <$> peek (castPtr p) <*> peek (plusPtr p 4) <*> peek (plusPtr p 8) <*> peek (plusPtr p 12)
  poke p (Rect l t r b) = poke (castPtr p) l *> poke (plusPtr p 4) t *> poke (plusPtr p 8) r *> poke (plusPtr p 12) b

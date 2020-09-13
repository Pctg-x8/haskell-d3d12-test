module Windows.Struct.GUID where

import Foreign.C.Types (CULong, CUShort, CUChar)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Data.Array.Storable (StorableArray)
import Data.Array.MArray (newArray_)
import Data.Word (Word64)
import Data.Memory.Endian (toBE, fromBE)

-- data4はBEで格納する(もとはunsigned char[8]なので)
data GUID = GUID { data1 :: CULong, data2 :: CUShort, data3 :: CUShort, data4 :: Word64 }
instance Storable GUID where
  sizeOf _ = 16
  alignment _ = 4
  peek p = GUID <$> peek (castPtr p) <*> peek (plusPtr p 4) <*> peek (plusPtr p 6) <*> (fromBE <$> peek (plusPtr p 8))
  poke p (GUID d1 d2 d3 d4) = poke (castPtr p) d1 >> poke (plusPtr p 4) d2 >> poke (plusPtr p 6) d3 >> poke (plusPtr p 8) (toBE d4)

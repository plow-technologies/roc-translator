{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType174 where

import Data.Binary.Get    (getByteString,
                           getWord8,
                           getWord16le,
                           Get)
import Data.ByteString    (ByteString)
import Data.Word          (Word8,Word16)
import Prelude            (($),
                           return,
                           Eq,
                           Float,
                           Read,
                           Show)
import Protocol.ROC.Float (getIeeeFloat32)
import Protocol.ROC.Utils (getTLP)

data PointType174 = PointType174 {
 
 pointType174ExportedParameterTag          :: !PointType174ExportedParameterTag
,pointType174ExportedParameterTLP          :: !PointType174ExportedParameterTLP
,pointType174NetworkID                     :: !PointType174NetworkID
,pointType174UniqueIdForTLP                :: !PointType174UniqueIdForTLP
,pointType174ExportedTLPCurrentValue       :: !PointType174ExportedTLPCurrentValue

} deriving (Eq,Read,Show)                       

type PointType174ExportedParameterTag      = ByteString
type PointType174ExportedParameterTLP      = [Word8]
type PointType174NetworkID                 = Word8
type PointType174UniqueIdForTLP            = Word16
type PointType174ExportedTLPCurrentValue   = Float
  
pointType174Parser :: Get PointType174
pointType174Parser = do 

  exportedParameterTag <- getByteString 10
  exportedParameterTLP <- getTLP
  networkID <- getWord8
  uniqueIdForTLP <- getWord16le
  exportedTLPCurrentValue <- getIeeeFloat32
  
  return $ PointType174 exportedParameterTag exportedParameterTLP networkID uniqueIdForTLP exportedTLPCurrentValue   

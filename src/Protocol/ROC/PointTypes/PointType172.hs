{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType172 where

import Data.Binary.Get    (getByteString,
                           getWord8,
                           getWord32le,
                           Get)
import Data.ByteString    (ByteString)
import Data.Word          (Word8,Word32)
import Prelude            (($),
                           return,
                           Eq,
                           Read,
                           Show)

data PointType172 = PointType172 {
 
 pointType172RemoteRTUTag                 :: !PointType172RemoteRTUTag
,pointType172ROCDeviceID                  :: !PointType172ROCDeviceID
,pointType172ComissionListIndex           :: !PointType172ComissionListIndex
,pointType172ComissionFlag                :: !PointType172ComissionFlag

} deriving (Eq,Read,Show)

type PointType172RemoteRTUTag             = ByteString            
type PointType172ROCDeviceID              = Word32            
type PointType172ComissionListIndex       = Word8            
type PointType172ComissionFlag            = Word8            
  
pointType172Parser :: Get PointType172
pointType172Parser = do 

  remoteRTUTag <- getByteString 20
  rOCDeviceID <- getWord32le
  comissionListIndex <- getWord8
  comissionFlag <- getWord8
  
  return $ PointType172 remoteRTUTag rOCDeviceID comissionListIndex comissionFlag

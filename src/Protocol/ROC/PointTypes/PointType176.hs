{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType176 where

import Data.Binary.Get    (getByteString,
                           getWord8,
                           getWord16le,
                           getWord32le,
                           Get)
import Data.ByteString    (ByteString)
import Data.Word          (Word8,Word16,Word32)
import Prelude            (($),
                           return,
                           Bool,
                           Eq,
                           Read,
                           Show)
import Protocol.ROC.Utils (anyButNull)

data PointType176 = PointType176 {
 
 pointType176DeviceTag                   :: !PointType176DeviceTag
,pointType176DeviceID                    :: !PointType176DeviceID
,pointType176ManufaturerID               :: !PointType176ManufaturerID
,pointType176DeviceType                  :: !PointType176DeviceType
,pointType176CommissionedListIndex       :: !PointType176CommissionedListIndex
,pointType176CommissionedFlag            :: !PointType176CommissionedFlag

} deriving (Eq,Read,Show)                       

type PointType176DeviceTag               = ByteString                      
type PointType176DeviceID                = Word32                      
type PointType176ManufaturerID           = Word16                      
type PointType176DeviceType              = Word16                      
type PointType176CommissionedListIndex   = Word8                      
type PointType176CommissionedFlag        = Bool                      
  
pointType176Parser :: Get PointType176
pointType176Parser = do 

  deviceTag <- getByteString 10  
  deviceID <- getWord32le  
  manufaturerID <- getWord16le  
  deviceType <- getWord16le  
  commissionedListIndex <- getWord8  
  commissionedFlag <- anyButNull  
  
  return $ PointType176 deviceTag deviceID manufaturerID deviceType commissionedListIndex commissionedFlag

{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType58 where

import Data.Binary.Get (getByteString,getWord8,Get)
import Data.ByteString (ByteString)
import Data.Word       (Word8)
import Prelude         (($),
                        return,
                        Eq,
                        Read,
                        Show)

data PointType58 = PointType58 {
 
 pointType58DeviceFirmwareDesc               :: !PointType58DeviceFirmwareDesc
,pointType58PartNumber                       :: !PointType58PartNumber
,pointType58Version                          :: !PointType58Version
,pointType58InformationPresentFlag           :: !PointType58InformationPresentFlag

} deriving (Read,Eq, Show)                       

type PointType58DeviceFirmwareDesc           = ByteString                                                                                                    
type PointType58PartNumber                   = ByteString                                                                                 
type PointType58Version                      = ByteString                                                                          
type PointType58InformationPresentFlag       = Word8                                                                            
                                
pointType58Parser :: Get PointType58
pointType58Parser = do

  deviceFirmwareDesc <- getByteString 20                                                       
  partNumber <- getByteString 10                                                 
  version <- getByteString 10                                                    
  informationPresentFlag <- getWord8                                
                         
  return $ PointType58 deviceFirmwareDesc partNumber version informationPresentFlag  
  
  
  
  

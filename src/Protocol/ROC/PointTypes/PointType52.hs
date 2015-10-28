{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType52 where

import Data.Binary.Get    (getByteString,getWord8,Get)
import Data.ByteString    (ByteString)
import Data.Word          (Word8)
import Prelude            (($),
                           return,
                           Eq,
                           Float,
                           Read,
                           Show)
import Protocol.ROC.Float (getIeeeFloat32)

data PointType52 = PointType52 {
 
 pointType52PointTag                         :: !PointType52PointTag
,pointType52BatteryVoltage                   :: !PointType52BatteryVoltage
,pointType52VoltageInput                     :: !PointType52VoltageInput
,pointType52BatteryTemp                      :: !PointType52BatteryTemp
,pointType52LowBatteryDropout                :: !PointType52LowBatteryDropout
,pointType52BatteryTurnOnVoltage             :: !PointType52BatteryTurnOnVoltage
,pointType52ReferenceVoltage                 :: !PointType52ReferenceVoltage
,pointType52UnitsFlag                        :: !PointType52UnitsFlag
,pointType52BatteryStatus                    :: !PointType52BatteryStatus
,pointType52DutyCycle                        :: !PointType52DutyCycle
,pointType52BatteryActivity                  :: !PointType52BatteryActivity
,pointType52Task                             :: !PointType52Task
,pointType52SleepFlag                        :: !PointType52SleepFlag

} deriving (Read,Eq, Show) 

type PointType52PointTag                      = ByteString                                      
type PointType52BatteryVoltage                = Float                                            
type PointType52VoltageInput                  = Float                                    
type PointType52BatteryTemp                   = Float                                      
type PointType52LowBatteryDropout             = Float                                        
type PointType52BatteryTurnOnVoltage          = Float                                            
type PointType52ReferenceVoltage              = Float                                    
type PointType52UnitsFlag                     = Word8                                      
type PointType52BatteryStatus                 = Word8                                      
type PointType52DutyCycle                     = Word8                                            
type PointType52BatteryActivity               = Word8                                    
type PointType52Task                          = Word8                                      
type PointType52SleepFlag                     = Word8                                        
  
pointType52Parser :: Get PointType52
pointType52Parser = do 

  pointTag <- getByteString 10                            
  batteryVoltage <- getIeeeFloat32                 
  voltageInput <- getIeeeFloat32                   
  batteryTemp <- getIeeeFloat32                    
  lowBatteryDropout <- getIeeeFloat32              
  batteryTurnOnVoltage <- getIeeeFloat32           
  referenceVoltage <- getIeeeFloat32               
  unitsFlag <- getWord8                      
  batteryStatus <- getWord8                  
  dutyCycle <- getWord8                      
  batteryActivity <- getWord8                
  task <- getWord8                          
  sleepFlag <- getWord8                      
  
  return $ PointType52 pointTag batteryVoltage voltageInput batteryTemp lowBatteryDropout batteryTurnOnVoltage referenceVoltage unitsFlag batteryStatus dutyCycle batteryActivity task sleepFlag  
  
  

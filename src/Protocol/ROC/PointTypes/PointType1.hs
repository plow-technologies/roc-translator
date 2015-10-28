{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType1 where

import Data.Binary.Get     (Get,
                            getByteString,
                            getWord8,
                            getWord16le,
                            getWord32le)
import Data.ByteString     (ByteString)
import Data.Int            (Int16)
import Data.Word           (Word8,
                            Word16,
                            Word32)
import Prelude             (($),
                            return,
                            Bool,
                            Eq,
                            Float,
                            Read,
                            Show)
import Protocol.ROC.Float  (getIeeeFloat32)
import Protocol.ROC.Utils  (anyButNull,getInt16)

data PointType1 = PointType1 {
 pointType1PointTag             :: !PointType1PointTag          
,pointType1Filter               :: !PointType1Filter                   
,pointType1Status               :: !PointType1Status                    
,pointType1BitfieldHigh         :: !PointType1BitfieldHigh                   
,pointType1BitfieldLow          :: !PointType1BitfieldLow                   
,pointType1AccumulatedValues    :: !PointType1AccumulatedValues                   
,pointType1OnCounter            :: !PointType1OnCounter                   
,pointType1OffCounter           :: !PointType1OffCounter                   
,pointType1PulseWidth0          :: !PointType1PulseWidth0                   
,pointType1PulseWidth100        :: !PointType1PulseWidth100                   
,pointType1MaxTimePulse         :: !PointType1MaxTimePulse                   
,pointType1Units                :: !PointType1Units                   
,pointType1ScanPeriod           :: !PointType1ScanPeriod                   
,pointType1LowReading           :: !PointType1LowReading                   
,pointType1HighReading          :: !PointType1HighReading                   
,pointType1LowAlarm             :: !PointType1LowAlarm                   
,pointType1HighAlarm            :: !PointType1HighAlarm                   
,pointType1LowLowAlarm          :: !PointType1LowLowAlarm                   
,pointType1HiHiAlarm            :: !PointType1HiHiAlarm                   
,pointType1RateAlarm            :: !PointType1RateAlarm                   
,pointType1AlarmDeadband        :: !PointType1AlarmDeadband                   
,pointType1EUValue              :: !PointType1EUValue                   
,pointType1TDICount             :: !PointType1TDICount                    
} deriving (Read,Eq, Show)                       

type PointType1PointTag              = ByteString
type PointType1Filter                = Word8
type PointType1Status                = Bool
type PointType1BitfieldHigh          = Word8
type PointType1BitfieldLow           = Word8
type PointType1AccumulatedValues     = Word32
type PointType1OnCounter             = Word32
type PointType1OffCounter            = Word32
type PointType1PulseWidth0           = Int16
type PointType1PulseWidth100         = Int16
type PointType1MaxTimePulse          = Word16
type PointType1Units                 = ByteString
type PointType1ScanPeriod            = Word16
type PointType1LowReading            = Float
type PointType1HighReading           = Float
type PointType1LowAlarm              = Float
type PointType1HighAlarm             = Float
type PointType1LowLowAlarm           = Float
type PointType1HiHiAlarm             = Float
type PointType1RateAlarm             = Float
type PointType1AlarmDeadband         = Float
type PointType1EUValue               = Float
type PointType1TDICount              = Word16                       

pointType1Parser :: Get PointType1 
pointType1Parser = do 
  pointId <- getByteString 10 
  fltr <- getWord8
  sts <- anyButNull
  cfg <- getWord8
  alarmcode <- getWord8
  accumulatedvalue <- getWord32le
  onCounter <- getWord32le
  offCounter <- getWord32le
  pulseWidth0 <- getInt16 
  pulseWidth100 <- getInt16
  maxpulsewidth <- getWord16le
  units <- getByteString 10
  scanPeriod <- getWord16le
  lowReading <- getIeeeFloat32
  highReading <- getIeeeFloat32
  lowAlarm <- getIeeeFloat32
  highAlarm <- getIeeeFloat32
  lowlowAlarm <- getIeeeFloat32
  highhighAlarm <- getIeeeFloat32
  rateAlarm <- getIeeeFloat32
  alarmDeadband <- getIeeeFloat32
  euValue <- getIeeeFloat32
  tdiCount <- getWord16le
  
  return $ PointType1 pointId fltr sts cfg alarmcode accumulatedvalue onCounter offCounter pulseWidth0 pulseWidth100 maxpulsewidth units scanPeriod lowReading highReading
                   lowAlarm highAlarm lowlowAlarm highhighAlarm rateAlarm alarmDeadband euValue tdiCount

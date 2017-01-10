{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Protocol.ROC where

import System.Hardware.Serialport
import CRC16.Calculator
import Data.Serialize
import qualified Data.ByteString as BS
import Protocol.ROC.ROCConfig
import Protocol.ROC.OpCodes
import Data.Word
import Data.Int


----------------- Default Configuration to talk to the ROC -------------------------------
--------- The RocAddress [240,240] is a generic address that lets us talk to any ROC.
--------- The RocAddress [1,3] is random and it is for designating us a a host.

testRocConfig :: RocConfig
testRocConfig = RocConfig "/dev/ttyUSB0" [240,240] [1,3] CS19200 "LOI" 1000
                
-----------------------------------------------------------------------------------------
    
----------------- Completely Defined Functions for Poll/Login --------------------------------

------------------ If you get a security error run this --------------
loginToROC :: String -> Word16 -> RocConfig -> IO (Either String String)
loginToROC userName passWrd cfg = do
  responsebytes <- runOpCodeRaw cfg opCode17
  if checkCRC16 (IStrictBS responsebytes) standardConfig
  then case runGet parseResponse responsebytes of
         Left err -> return $ Left err
         Right responseinfo -> return $ parseResponseInfo responseinfo
  else return $ Left "CRC check failed"

---------------------------------------------------------------------

------------------ These will get you specific data points ----------
       
getMinutesToday :: RocConfig -> IO (Either String Float)
getMinutesToday = runGetDataPoint 47 0 41

getHWUncorrectedFlowRate :: RocConfig -> IO (Either String Float)
getHWUncorrectedFlowRate = runGetDataPoint 46 0 51

getPFStaticPressure :: RocConfig -> IO (Either String Float)
getPFStaticPressure = runGetDataPoint 46 0 52

getTFTemperature :: RocConfig -> IO (Either String Float)
getTFTemperature = runGetDataPoint 46 0 53

getIMVBMV :: RocConfig -> IO (Either String Float)
getIMVBMV = runGetDataPoint 47 0 16

getPressureExtensionUncorrectedFlowRate :: RocConfig -> IO (Either String Float)
getPressureExtensionUncorrectedFlowRate = runGetDataPoint 47 0 4

getFlowRatePerDay :: RocConfig -> IO (Either String Float)
getFlowRatePerDay = runGetDataPoint 47 0 0

getEnergyRatePerDay :: RocConfig -> IO (Either String Float)
getEnergyRatePerDay = runGetDataPoint 47 0 1
-----------------------------------------------------------------------------------------------


-------------------------- General Function for getting data from ROC -------------------------

---- PointType = Word8 and is associated with things like Discrete Outputs, Analog Inputs, Meter Flow Values, etc.
---- PointNumber = Word8 and (also called Logical Number) is associated with which PointType you wish to talk like Analog Input 1, AnalogInput 2, Analog Input 3, etc. (it is 0 based so Analog Input 1 = Word8 0
-- ParameterNumber = Word8 and is associated with which parameter you wish to retrieve from the PointType and PointNumber
runGetDataPoint :: RocType a => PointType -> PointNumber -> ParameterNumber -> RocConfig -> IO (Either String a)
runGetDataPoint pointtype pointnumber parameternumber rocConfig = do
  responsebytes <- runOpCodeRaw rocConfig (opCode167 pointtype pointnumber 1 parameternumber)
  if checkCRC16 (IStrictBS responsebytes) standardConfig
  then case runGet parseResponse responsebytes of
         Left err -> return $ Left err
         Right responseinfo -> return $ parseResponseInfo responseinfo
  else return $ Left "CRC check failed"


-------------- Parser to get to the Opcode to decide how to proceed ----------
 
parseResponse :: Get ResponseInfo
parseResponse = do
  responseLocalHostUnitId <- getWord8
  responseLocalHostGroupId <- getWord8
  responseDeviceUnitId <- getWord8
  responseDeviceGroupId <- getWord8
  responseOpcodeId <- getWord8
  bytesLength <- getWord8
  responseDataBytes <- getByteString $ fromIntegral bytesLength
  skip 2
  return $ ResponseInfo responseLocalHostUnitId
                        responseLocalHostGroupId
                        responseDeviceUnitId
                        responseDeviceGroupId
                        responseOpcodeId
                        responseDataBytes

data ResponseInfo = ResponseInfo { hostUnitId     :: Word8
                                 , hostGroupId    :: Word8
                                 , deviceUnitId   :: Word8
                                 , deviceGroupId  :: Word8
                                 , opcodeId       :: Word8
                                 , dataByteString :: BS.ByteString
                                 } deriving (Eq,Ord,Show)
                        
-------------------- Now that the Opcode has been parsed we can parse accordingly ------------------
                        
parseResponseInfo :: RocType a => ResponseInfo -> Either String a
parseResponseInfo (ResponseInfo _ _ _ _ 167 databytes) = runGet parse167 databytes
parseResponseInfo (ResponseInfo _ _ _ _ 17  _) = runGet parse17 "Login Successful"
parseResponseInfo (ResponseInfo _ _ _ _ 255 databytes) = runGet parseError databytes
parseResponseInfo response = Left $ "Unknown Response " ++ show response

--------------------------------------------------------------------------------------------------
                             
---------------- For errors from the device------------------------
                             
parseError :: Get a
parseError = do
  errorcode <- getWord8
  opcodeWithError <- getWord8
  byteWithError <- getWord8
  let errorInfo = " for Opcode " ++ show opcodeWithError ++ " at byte number " ++ show byteWithError
  case errorcode of
     1 -> fail $ "Invalid Opcode request" ++ errorInfo
     2 -> fail $ "Invalid Parameter Number" ++ errorInfo
     3 -> fail $ "Invalid Logical Number/Point Number" ++ errorInfo
     4 -> fail $ "Invalid Point Type" ++ errorInfo
     5 -> fail $ "Received too many data bytes" ++ errorInfo
     6 -> fail $ "Received too few data bytes" ++ errorInfo
     7 -> fail $ "Did not receive 1 data byte" ++ errorInfo
     8 -> fail $ "Did not receive 2 data byte" ++ errorInfo
     9 -> fail $ "Did not receive 3 data byte" ++ errorInfo
     10 -> fail $ "Did not receive 4 data byte" ++ errorInfo
     11 -> fail $ "Did not receive 5 data byte" ++ errorInfo
     12 -> fail $ "Did not receive 16 data" ++ errorInfo
     13 -> fail $ "Outside valid address range" ++ errorInfo
     14 -> fail $ "Invalid history request" ++ errorInfo
     15 -> fail $ "Invalid FST request" ++ errorInfo
     16 -> fail $ "Invalid event entry" ++ errorInfo
     17 -> fail $ "Requested too many alarms" ++ errorInfo
     18 -> fail $ "Requested too many events" ++ errorInfo
     19 -> fail $ "Write to read only parameter" ++ errorInfo
     20 -> fail $ "Security error" ++ errorInfo
     21 -> fail $ "Invalid security logon" ++ errorInfo
     22 -> fail $ "Invalid store and forward path" ++ errorInfo
     23 -> fail $ "Flash programming error" ++ errorInfo
     24 -> fail $ "History configuration in progress" ++ errorInfo
     63 -> fail $ "Requested security level too hight" ++ errorInfo
     _ -> fail $ "Unknown error" ++ errorInfo

----------------------------------------------------------------------

------------ Where the magic happens so that we can read any value (almost) -----------------
          
class (RocGet a) => RocType a where

class RocGet a where
    rocget :: Get a

instance RocType Float
instance RocGet Float where
    rocget = getFloat32le

instance RocType Word8
instance RocGet Word8 where
    rocget = getWord8

instance RocType Word16
instance RocGet Word16 where
    rocget = getWord16le

instance RocType Word32
instance RocGet Word32 where
    rocget = getWord32le

instance RocType Int8
instance RocGet Int8 where
    rocget = getInt8

instance RocType Int16
instance RocGet Int16 where
    rocget = getInt16le

instance RocType Int32
instance RocGet Int32 where
    rocget = getInt32le

instance RocType String
instance RocGet String where
    rocget = do
      bs <- getByteString 16
      return $ show bs

         

--------------------------------------------------------------------------------------------------

----------------------------- Opcode parsers ---------------------------------------------------
parse167 :: RocType a =>  Get a
parse167 = do
  skip 4
  rocget

parse17 :: RocType a => Get a
parse17 = rocget
-------------------------------------------------------------------------------------------------

-------------------------- Sending and Receiving the ByteStrings ----------------------------
runOpCodeRaw :: RocConfig -> (RocConfig -> BS.ByteString) -> IO BS.ByteString
runOpCodeRaw cfg opCode = do
  let port = rocConfigPort cfg
      commRate = rocCommSpeed cfg            
  s <- openSerial port defaultSerialSettings { commSpeed = commRate }
  _ <- send s $ appendCRC16 (IStrictBS $ opCode cfg) standardConfig
  receivebs <- recvAllBytes s 255
  closeSerial s
  return receivebs
---------------------------------------------------------------------------------------------                                   



{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Protocol.ROC where

import System.Hardware.Serialport
import Conduit                     (($$),sourceLazy)
import CRC16.Calculator
import Data.Binary                 (get,put,decode,encode,Binary,Get)
import Data.Binary.Get             (getLazyByteString,getWord8)
import Data.Binary.Put             (putByteString,putLazyByteString,putWord8,runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Conduit.Serialization.Binary  (sinkGet)
import Data.Conduit.Network        (appSink,appSource,clientSettings,runTCPClient)
import Data.List 
import Data.IntMap.Strict          (minViewWithKey,size,Key)
import Data.Text.Encoding          (encodeUtf8)
import Data.Word                   (Word8,Word16)
import Roc.Protocol.Types
import Protocol.ROC.Utils
import Protocol.ROC.PointTypes
import Protocol.ROC.FullyDefinedPointType 
import Protocol.ROC.ROCConfig
import Protocol.ROC.OpCodes
import Protocol.ROC.RocSerialize
import System.IO.Error             (tryIOError)

--import Numeric                                            


-- | getPointType is designed for retrieving and entire PointType for serial comms only

-- getPointType :: RocConfig -> DefaultPointType -> PointNumber -> IO ()
-- getPointType cfg fdpt pn = do
--   let fdataBytes = fdptRxProtocol fdpt
--       ptid = fdptPointTypeID fdpt
--       pc = fdptParameterCount fdpt
--       sp = fdptStartParameter fdpt
--   dataBytes <- fdataBytes cfg pn ptid pc sp    
--   let fetchedPointType = fetchPointType ptid (LB.fromStrict dataBytes)
--   print fetchedPointType

-- | writePointType is designed to write to a specific field in a PointType for serial comms only

-- writePointType :: RocSerialize a => RocConfig -> DefaultPointType -> PointNumber -> ParameterNumber -> a -> IO ()
-- writePointType cfg fdpt pn prn pdata = do
--   let port = rocConfigPort cfg
--       commRate = rocCommSpeed cfg
--       ptid = fdptPointTypeID fdpt
--       pt = decodePTID ptid
--       databytes = BS.append (opCode166 pt pn prn pdata cfg) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode166 pt pn prn pdata cfg)
--   s <- openSerial port defaultSerialSettings { commSpeed = commRate } 
--   print $ showInt <$> BS.unpack databytes <*> [""]
--   _ <- send s databytes
--   receivebs <- recvAllBytes s 255
--   closeSerial s
--   print $ showInt <$> BS.unpack receivebs <*> [""]


-- | runOpCodeRaw is designed for running an opcode on the roc
  
-- runOpCodeRaw :: RocConfig -> (RocConfig -> BS.ByteString) -> IO BS.ByteString
-- runOpCodeRaw cfg opCode = do
--   let port = rocConfigPort cfg
--       commRate = rocCommSpeed cfg            
--   s <- openSerial port defaultSerialSettings { commSpeed = commRate }
--   _ <- send s $ BS.append (opCode cfg) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode cfg)
--   receivebs <- recvAllBytes s 255
--   closeSerial s
--   print $ showInt <$> BS.unpack receivebs <*> [""]
--   return receivebs


requestOpCodeTableData :: HostAddress -> ModemConfig -> RocAccessConfig -> OpCodeTable l -> IO (Either String RocOpCodeResponse)
requestOpCodeTableData hostAdd modemCfg accessCfg opTable = do    
  let req = sourceLazy $ runPut.put $ buildOpTableRequest hostAdd accessCfg opTable
      sendRequest server = do
        req $$ appSink server
        appSource server $$ sinkGet getRoutine
  eitherRocOpCodeResponse <- tryIOError (runTCPClient (clientSettings (_port modemCfg) (encodeUtf8 $ _url modemCfg)) sendRequest)
  case eitherRocOpCodeResponse of
    Left err -> return.Left $ show err
    Right resp -> return.Right $ resp

buildOpTableRequest :: HostAddress -> RocAccessConfig -> OpCodeTable l -> Either String RocOpCodeRequest
buildOpTableRequest hostAdd cfg table = (\ dataBytes ->  RocOpCodeRequest {reqRocAddress = RocAdd { rocUnitNumber = _unitNumber cfg, rocGroupNumber = _groupNumber cfg},
                                                                           reqHostAddress = HostAddress { hostUnitNumber = hostUnitNumber hostAdd, hostGroupNumber = hostGroupNumber hostAdd},
                                                                           reqOpCodeNumber = 10 ,
                                                                           reqDataByteString = dataBytes}) <$> eitherDataBytes
    where
      maybeMinKey :: Maybe Word8
      maybeMinKey = fromIntegral . fst . fst <$> (minViewWithKey .  _unOpCodeData . _opCodeData $ table)
      numberOfKeys = fromIntegral . size . _unOpCodeData . _opCodeData $ table
      eitherDataBytes = case (\ minKey ->  LB.pack [3, _unOpCodeTableId . _opCodeTableId $ table, minKey, numberOfKeys]) <$> maybeMinKey of
                          Nothing -> Left "No minkey found in OpCodeData IntMap"
                          Just bs -> Right bs



getRoutine :: Get RocOpCodeResponse
getRoutine = get


rocCRC16Config :: CRC16Config
rocCRC16Config = standardConfig 

data RocAdd = RocAdd { rocUnitNumber  :: !Word8,
                       rocGroupNumber :: !Word8
                     } deriving (Eq,Show)

data HostAddress = HostAddress { hostUnitNumber  :: !Word8,
                                 hostGroupNumber :: !Word8
                               } deriving (Eq,Show)


data RocOpCodeResponse = RocOpCodeResponse { respRocAddress        :: RocAdd,
                                             respHostAddress       :: HostAddress,
                                             respOpCodeNumber      :: Word8,
                                             respDataBytesCount    :: Word8,
                                             respDataByteString    :: LazyByteString
                                           } deriving (Eq,Show)

data RocOpCodeRequest = RocOpCodeRequest { reqRocAddress     :: RocAdd,
                                           reqHostAddress    :: HostAddress,
                                           reqOpCodeNumber   :: Word8,
                                           reqDataByteString :: LazyByteString
                                         } deriving (Eq,Show)

--data OpCode = OpCode10 deriving Show

instance Binary RocOpCodeRequest where
    get = do
      rocUnit           <- getWord8
      rocGroup          <- getWord8
      hostUnit          <- getWord8
      hostGroup         <- getWord8
      opCode            <- getWord8
      numberOfDataBytes <- getWord8
      dataByteString    <- getLazyByteString (2 + fromIntegral numberOfDataBytes)
      let repackBytes = LB.pack [rocUnit,rocGroup,hostUnit,hostGroup,opCode,numberOfDataBytes]
      if checkCRC16 (ILazyBS $ LB.append repackBytes dataByteString) rocCRC16Config
      then return RocOpCodeRequest { reqRocAddress = RocAdd { rocUnitNumber = rocUnit, rocGroupNumber = rocGroup},
                                     reqHostAddress = HostAddress { hostUnitNumber = hostUnit, hostGroupNumber = hostGroup},
                                     reqOpCodeNumber = opCode,
                                     reqDataByteString = LB.take (fromIntegral numberOfDataBytes) dataByteString}
      else fail "CRC check faild"

    put request = putByteString $ appendCRC16 (ILazyBS $ runPut putHeader) rocCRC16Config
        where
          putHeader = do
            putWord8 . rocUnitNumber. reqRocAddress $ request
            putWord8 . rocGroupNumber . reqRocAddress $ request
            putWord8 . hostUnitNumber . reqHostAddress $ request
            putWord8 . hostGroupNumber . reqHostAddress $ request
            putWord8 . reqOpCodeNumber $ request
            putWord8 . fromIntegral . LB.length . reqDataByteString $ request
            putLazyByteString . reqDataByteString $ request

instance Binary RocOpCodeResponse where
    get = do
      hostUnit          <- getWord8
      hostGroup         <- getWord8
      rocUnit           <- getWord8
      rocGroup          <- getWord8
      opCode            <- getWord8
      numberOfDataBytes <- getWord8
      dataByteString    <- getLazyByteString (2 + fromIntegral numberOfDataBytes)
      let repackBytes = LB.pack [rocUnit,rocGroup,hostUnit,hostGroup,opCode,numberOfDataBytes] 
      if checkCRC16 (ILazyBS $ LB.append repackBytes dataByteString) rocCRC16Config
      then return RocOpCodeResponse { respRocAddress = RocAdd { rocUnitNumber = rocUnit, rocGroupNumber = rocGroup},
                                      respHostAddress = HostAddress { hostUnitNumber = hostUnit, hostGroupNumber = hostGroup},
                                      respOpCodeNumber = opCode,
                                      respDataBytesCount = numberOfDataBytes,
                                      respDataByteString = LB.take (fromIntegral numberOfDataBytes) dataByteString}
                                               
      else fail "CRC check failed"
    put response = do
      putWord8 $ hostUnitNumber . respHostAddress $ response
      putWord8 $ hostGroupNumber . respHostAddress $ response
      putWord8 $ rocUnitNumber . respRocAddress $ response
      putWord8 $ rocGroupNumber . respRocAddress $ response
      putWord8 $ respOpCodeNumber response
      putWord8 $ respDataBytesCount response
      putLazyByteString $ respDataByteString response


----------------------------- Testing Stuff -----------------------------------------------

testHostAddress :: HostAddress 
testHostAddress = HostAddress 1 3

testRocOpCodeRequest :: RocOpCodeRequest
testRocOpCodeRequest = RocOpCodeRequest (RocAdd 240 240) (HostAddress 1 3) 10 $ LB.pack [0,0,3]

testRocOpCodeRequestByteString :: LazyByteString
testRocOpCodeRequestByteString = "\240\240\SOH\ETX\n\ETX\NUL\NUL\ETX\195\247"

testRocConfig :: RocConfig
testRocConfig = RocConfig "/dev/ttyUSB0" [240,240] [1,3] CS19200 "LOI" 1000
 
testingEncode :: IO ()
testingEncode = do
    let manualByteString = BS.append (opCode10 testRocConfig) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode10 testRocConfig)
        encodedByteString = encode testRocOpCodeRequest
    if encodedByteString == LB.fromStrict manualByteString
    then print ("Successfully encoded RocOpCodeRequest" :: String)
    else print ("Failed to encode RopOpCodeRequest" :: String)

testingDecodingByteString :: IO ()
testingDecodingByteString = 
    if testRocOpCodeRequest == (decode testRocOpCodeRequestByteString :: RocOpCodeRequest)
    then print ("Successfully decoded RocOpCodeRequest" :: String)
    else print ("Failed to decode RocOpCodeRequest" :: String)


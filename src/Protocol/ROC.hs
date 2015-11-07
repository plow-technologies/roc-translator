{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Protocol.ROC where
import           Conduit                           (sourceLazy, ($$))
import           Control.Monad
import           CRC16.Calculator
import           Data.Bimap                        (Bimap, empty, insert)
import           Data.Binary                       (Binary, Get, decode, encode,
                                                    get, put)
import           Data.Binary.Get                   (getLazyByteString, getWord8,
                                                    runGet)
import           Data.Binary.Put                   (putByteString,
                                                    putLazyByteString, putWord8,
                                                    runPut)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LB
import           Data.Conduit.Network              (appSink, appSource,
                                                    clientSettings,
                                                    runTCPClient)
import           Data.Conduit.Serialization.Binary (sinkGet)
import           Data.IntMap.Strict                (IntMap, foldrWithKey,
                                                    fromList, minViewWithKey,
                                                    singleton, size)
import           Data.List                         ()
import           Data.Text                         (Text, unpack)
import           Data.Text.Encoding                (encodeUtf8)
import           Data.Word                         (Word8)
-- import           Protocol.ROC.FullyDefinedPointType
import           Protocol.ROC.OpCodes
-- import           Protocol.ROC.PointTypes
import           Protocol.ROC.ROCConfig
-- import           Protocol.ROC.RocSerialize
import           Protocol.ROC.Utils
import           Roc.Protocol.Types
import           System.Hardware.Serialport
import           System.IO.Error                   (tryIOError)


import           Numeric

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

runOpCodeRaw :: RocConfig -> (RocConfig -> BS.ByteString) -> IO BS.ByteString
runOpCodeRaw cfg opCode = do
  let port = rocConfigPort cfg
      commRate = rocCommSpeed cfg
      bs = BS.append (opCode cfg) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode cfg)
  s <- openSerial port defaultSerialSettings { commSpeed = commRate }
  _ <- send s $ bs
  receivebs <- recvAllBytes s 255
  closeSerial s
  print $ showInt <$> BS.unpack bs <*> [""]
  print $ showInt <$> BS.unpack receivebs <*> [""]
  return receivebs


requestOpCodeTableData :: HostAddress -> ModemConfig -> RocAccessConfig -> OpCodeTable l -> IO (Either String RocOpCodeResponse)
requestOpCodeTableData hostAdd modemCfg accessCfg opTable =
  case buildOpTableRequest hostAdd accessCfg opTable of
    Left str -> return.Left $ str
    Right (rocRequest:: RocOpCodeRequest) -> do
      print $ showInt <$> BS.unpack (LB.toStrict.encode $ rocRequest) <*> [""]
      print rocRequest
      let req = sourceLazy $ runPut.put $ rocRequest
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
      eitherDataBytes = case (\ minKey ->  LB.pack [ _unOpCodeTableId . _opCodeTableId $ table, minKey - 1 , numberOfKeys]) <$> maybeMinKey of
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


data RocOpCodeResponse = RocOpCodeResponse { respRocAddress     :: RocAdd,
                                             respHostAddress    :: HostAddress,
                                             respOpCodeNumber   :: Word8,
                                             respDataBytesCount :: Word8,
                                             respDataByteString :: LazyByteString
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
      let repackBytes = LB.pack [hostUnit,hostGroup,rocUnit,rocGroup,opCode,numberOfDataBytes]
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


testQuery :: IO (Either String (OpCodeTable TlpDataResponse))
testQuery = do
  let eitherOpCodeTable@(Right opCodeTable) = makeOpCodeTable
  doubleEither <- requestOpCodeTableData testHostAddress testModemConfig testRocAccessConfig `traverse` eitherOpCodeTable
  case join doubleEither of
         Left s -> return . Left $ s
         Right response -> do
             print response
             let opCodeTableGet = buildOpCodeTableGet opCodeTable
                 result = runGet opCodeTableGet (respDataByteString response)
             return . Right $ result

testHostAddress :: HostAddress
testHostAddress = HostAddress 1 3

testRocOpCodeRequest :: RocOpCodeRequest
testRocOpCodeRequest = RocOpCodeRequest (RocAdd 240 240) (HostAddress 1 3) 10 $ LB.pack [0,0,44]

testRocOpCodeRequestByteString :: LazyByteString
testRocOpCodeRequestByteString = "\240\240\SOH\ETX\n\ETX\NUL\NUL\ETX\195\247"

testRocConfig :: RocConfig
testRocConfig = RocConfig "/dev/ttyUSB0" [240,240] [1,3] CS19200 "LOI" 1000

testRocAccessConfig :: RocAccessConfig
testRocAccessConfig = RocAccessConfig 240 240 "LOI" 1000

testModemConfig :: ModemConfig
testModemConfig = ModemConfig "166.131.38.15" 20090

makeOpCodeTable :: Either String (OpCodeTable TlpDataRequest)
makeOpCodeTable = case (\ opTable -> OpCodeTable (OpCodeTableId 0) opTable makeBimap) <$> makeDataMap of
                    Left txt -> Left $ unpack txt
                    Right bs -> Right bs

makeBimap :: Bimap Int OnPingId
makeBimap = foldrWithKey bimapFcn empty myDataMap
    where
      bimapFcn k _ = insert k (OnPingId k)

makeDataMap :: Either Text (OpCodeData TlpDataRequest)
makeDataMap = makeOpCodeData myDataMap

myDataMap :: IntMap (TlpDataRequest)
myDataMap = TlpDataRequest <$> simpleIntMap
  where
    simpleIntMap = fromList [( 1  , TlpUnsignedInt8 (Left ())),
                                  ( 2  , TlpUndefined32 (Left ())),
                                  ( 3  , TlpUnsignedInt8 (Left ())),
                                  ( 4  , TlpFloat (Left ())),
                                  ( 5  , TlpFloat (Left ())),
                                  ( 6  , TlpFloat (Left ())),
                                  ( 7  , TlpUnsignedInt8 (Left ())),
                                  ( 8  , TlpFloat (Left ())),
                                  ( 9  , TlpFloat (Left ())),
                                  ( 10 , TlpFloat (Left ())),
                                  ( 11 , TlpFloat (Left ())),
                                  ( 12 , TlpFloat (Left ())),
                                  ( 13 , TlpFloat (Left ())),
                                  ( 14 , TlpInt16 (Left ())),
                                  ( 15 , TlpInt16 (Left ())),
                                  ( 16 , TlpFloat (Left ())),
                                  ( 17 , TlpFloat (Left ())),
                                  ( 18 , TlpFloat (Left ())),
                                  ( 19 , TlpInt16 (Left ())),
                                  ( 20 , TlpFloat (Left ())),
                                  ( 21 , TlpInt16 (Left ())),
                                  ( 22 , TlpFloat (Left ())),
                                  ( 23 , TlpFloat (Left ())),
                                  ( 24 , TlpFloat (Left ())),
                                  ( 25 , TlpFloat (Left ())),
                                  ( 26 , TlpInt16 (Left ())),
                                  ( 27 , TlpFloat (Left ())),
                                  ( 28 , TlpFloat (Left ())),
                                  ( 29 , TlpFloat (Left ())),
                                  ( 30 , TlpFloat (Left ())),
                                  ( 31 , TlpFloat (Left ())),
                                  ( 32 , TlpFloat (Left ())),
                                  ( 33 , TlpFloat (Left ())),
                                  ( 34 , TlpFloat (Left ())),
                                  ( 35 , TlpFloat (Left ())),
                                  ( 36 , TlpFloat (Left ())),
                                  ( 37 , TlpFloat (Left ())),
                                  ( 38 , TlpFloat (Left ())),
                                  ( 39 , TlpFloat (Left ())),
                                  ( 40 , TlpFloat (Left ())),
                                  ( 41 , TlpFloat (Left ())),
                                  ( 42 , TlpFloat (Left ())),
                                  ( 43 , TlpFloat (Left ())),
                                  ( 44 , TlpFloat (Left ()))]

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


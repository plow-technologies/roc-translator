{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol.ROC where
import           Conduit                       (sourceLazy, ($$))
import           Control.Monad
import           CRC16.Calculator
import           Data.Bimap                    (Bimap, empty, insert)
import qualified Data.ByteString.Lazy          as BL
import           Data.Serialize                (Get, Serialize, decode, encode,
                                                get, put)
-- import           Data.Binary                       (Binary, Get, decode, encode,
--                                                     get, put)
import           Data.Serialize.Get            (getByteString, getWord32le,
                                                getWord8, runGet)
import           Data.Serialize.IEEE754        (getFloat32le,putFloat32le)
import           Data.Serialize.Put            (Put, putByteString, putWord8, runPut)
-- import           Data.Binary.Get                   (getLazyByteString, getWord8,
--                                                     getWord32le, runGet)
-- import           Data.Binary.Put                   (putByteString,
--                                                     putLazyByteString, putWord8,
--                                                    runPut)
import qualified Data.ByteString               as BS
-- import qualified Data.ByteString.Lazy              as LB
import           Data.Conduit.Cereal           (sinkGet)
import           Data.Conduit.Network          (appSink, appSource,
                                                clientSettings, runTCPClient)
import           Data.IntMap.Strict            (IntMap, foldrWithKey, fromList,
                                                minViewWithKey, singleton, size)
import           Data.List                     ()
import           Data.Text                     (Text, unpack)
import           Data.Text.Encoding            (encodeUtf8)
import           Data.Word                     (Word8)
-- import           Protocol.ROC.FullyDefinedPointType
-- import           Protocol.ROC.OpCodes
-- import           Protocol.ROC.PointTypes
-- import           Protocol.ROC.ROCConfig
-- import           Protocol.ROC.RocSerialize
-- import           Protocol.ROC.Utils
import           SingleWell.Roc.Protocol.Types
import           System.Hardware.Serialport
import           System.IO.Error               (tryIOError)


import           Numeric

type StrictByteString = BS.ByteString

-- | getPointType is designed for retrieving and entire PointType for serial comms only

-- getPointType :: RocConfig -> DefaultPointType -> PointNumber -> IO ()
-- getPointType cfg fdpt pn = do
--   let fdataBytes = fdptRxProtocol fdpt
--       ptid = fdptPointTypeID fdpt
--       pc = fdptParameterCount fdpt
--       sp = fdptStartParameter fdpt
--   dataBytes <- fdataBytes cfg pn ptid pc sp
--   let fetchedPointType = fetchPointType ptid (BS.fromStrict dataBytes)
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
--       bs = BS.append (opCode cfg) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode cfg)
--   s <- openSerial port defaultSerialSettings { commSpeed = commRate }
--   _ <- send s $ bs
--   receivebs <- recvAllBytes s 255
--   closeSerial s
--   print $ showInt <$> BS.unpack bs <*> [""]
--   print $ showInt <$> BS.unpack receivebs <*> [""]
--   return receivebs


requestOpCodeTableData :: HostAddress -> ModemConfig -> RocAccessConfig -> OpCodeTable l -> IO (Either String RocOpCodeResponse)
requestOpCodeTableData hostAdd modemCfg accessCfg opTable =
  case buildOpTableRequest hostAdd accessCfg opTable of
    Left str -> return.Left $ str
    Right (rocRequest:: RocOpCodeRequest) -> do
      let req = sourceLazy $ BL.fromStrict.runPut.put $ rocRequest
          sendRequest server = do
                          req $$ appSink server
                          appSource server $$ sinkGet getRoutine
      eitherRocOpCodeResponse <- tryIOError (runTCPClient (clientSettings (_port modemCfg) (encodeUtf8 $ _url modemCfg)) sendRequest)
      case eitherRocOpCodeResponse of
        Left err -> return.Left $ show err
        Right resp -> return.Right $ resp

buildOpTableRequest :: HostAddress -> RocAccessConfig -> OpCodeTable l -> Either String RocOpCodeRequest
buildOpTableRequest hostAdd cfg table = (\ dataBytes ->  RocOpCodeRequest {reqRocAddress = RocAddress { rocUnitNumber = _unitNumber cfg, rocGroupNumber = _groupNumber cfg},
                                                                           reqHostAddress = HostAddress { hostUnitNumber = hostUnitNumber hostAdd, hostGroupNumber = hostGroupNumber hostAdd},
                                                                           reqOpCodeNumber = OpCodeNumber 10 ,
                                                                           reqDataByteString = dataBytes}) <$> eitherDataBytes
    where
      maybeMinKey :: Maybe Word8
      maybeMinKey = fromIntegral . fst . fst <$> (minViewWithKey .  _unOpCodeData . _opCodeData $ table)
      numberOfKeys = fromIntegral . size . _unOpCodeData . _opCodeData $ table
      eitherDataBytes = case (\ minKey ->  BS.pack [ _unOpCodeTableId . _opCodeTableId $ table, minKey - 1 , numberOfKeys]) <$> maybeMinKey of
                          Nothing -> Left "No minkey found in OpCodeData IntMap"
                          Just bs -> Right bs



getRoutine :: Get RocOpCodeResponse
getRoutine = get


rocCRC16Config :: CRC16Config
rocCRC16Config = standardConfig

data RocAddress = RocAddress { rocUnitNumber  :: !Word8,
                               rocGroupNumber :: !Word8
                             } deriving (Eq,Show)

data HostAddress = HostAddress { hostUnitNumber  :: !Word8,
                                 hostGroupNumber :: !Word8
                               } deriving (Eq,Show)


data RocOpCodeResponse = RocOpCodeResponse { respRocAddress         :: RocAddress,
                                             respHostAddress        :: HostAddress,
                                             respOpCodeNumber       :: OpCodeNumber,
                                             respDataBytesCount     :: BytesCount,
                                             respOpCodeDataResponse :: OpCodeDataResponse
                                           } deriving (Eq,Show)

data RocOpCodeRequest = RocOpCodeRequest { reqRocAddress     :: RocAddress,
                                           reqHostAddress    :: HostAddress,
                                           reqOpCodeNumber   :: OpCodeNumber,
                                           reqDataByteString :: StrictByteString
                                         } deriving (Eq,Show)


data OpCodeDataResponse = OpCode10DataResponse { _opCodeTableNumber   :: Word8,
                                                 _tableStartingNumber :: Word8,
                                                 _numberOfTableValues :: Word8,
                                                 _tableVersionNumber  :: Float,
                                                 _opCode10DataBytes   :: StrictByteString
                                               } 
                        | OpCode255DataResponse { _errorCode :: Word8, _errorOpCode :: OpCodeNumber, _errorByte :: Word8}
                          deriving (Eq,Show)
                        

newtype OpCodeNumber = OpCodeNumber { _unOpCodeNumber :: Word8} deriving (Eq,Show)
newtype BytesCount = BytesCount { _unBytesCount :: Word8} deriving (Eq,Show)

data RocMessageInfo = RocMessageInfo { unit1  :: Word8,
                                       group1 :: Word8,
                                       unit2  :: Word8,
                                       group2 :: Word8,
                                       opCode :: OpCodeNumber,
                                       bCount :: BytesCount,
                                       dataBS :: StrictByteString
                                     } deriving (Eq,Show)

getOpCodeDataResponse ::  OpCodeNumber -> BytesCount -> Get OpCodeDataResponse
getOpCodeDataResponse opNumber count =
    case _unOpCodeNumber opNumber of
      10 -> opCode10Get count 
      otherwise -> fail "Opocode Not implemented yet"

putOpCodeDataResponse :: OpCodeDataResponse -> Put
putOpCodeDataResponse (OpCode10DataResponse table idx count version bs) = 
  putWord8 table >> putWord8 idx >> putWord8 count >> putFloat32le version >> putByteString bs
putOpCodeDataResponse (OpCode255DataResponse code opcode byte) = putWord8 code >> putWord8 (_unOpCodeNumber opcode) >> putWord8 byte
putOpCodeDataResponse (something) = fail (show something ++ " not implemented yet")

opCode10Get :: BytesCount -> Get OpCodeDataResponse
opCode10Get count = do
  tableNumber  <- getWord8
  startNumber  <- getWord8
  numberOfVals <- getWord8
  tableVersion <- getFloat32le
  databytes    <- getByteString (fromIntegral $ _unBytesCount count - 7)
  return OpCode10DataResponse {_opCodeTableNumber   = tableNumber,
                               _tableStartingNumber = startNumber,
                               _numberOfTableValues = numberOfVals,
                               _tableVersionNumber  = tableVersion,
                               _opCode10DataBytes   = databytes}

instance Serialize RocOpCodeRequest where
    get = do
      info <- getRocMessageInfo
      return RocOpCodeRequest { reqRocAddress     = RocAddress { rocUnitNumber = unit1 info, rocGroupNumber = group1 info},
                                reqHostAddress    = HostAddress { hostUnitNumber = unit2 info, hostGroupNumber = group2 info},
                                reqOpCodeNumber   = opCode info,
                                reqDataByteString = dataBS info }

    put request = putByteString $ appendCRC16 (IStrictBS $ runPut putRequest) rocCRC16Config
        where
          putRequest = do
            putWord8 . rocUnitNumber   . reqRocAddress $ request
            putWord8 . rocGroupNumber  . reqRocAddress $ request
            putWord8 . hostUnitNumber  . reqHostAddress $ request
            putWord8 . hostGroupNumber . reqHostAddress $ request
            putWord8 . _unOpCodeNumber . reqOpCodeNumber $ request
            putWord8 . fromIntegral . BS.length . reqDataByteString $ request
            putByteString . reqDataByteString $ request

instance Serialize RocOpCodeResponse where
    get = do
      info <- getRocMessageInfo
      case runGet (getOpCodeDataResponse (opCode info) (bCount info)) $ dataBS info of
            Left err -> fail err
            Right resp -> return RocOpCodeResponse { respRocAddress         = RocAddress { rocUnitNumber = unit2 info, rocGroupNumber = group2 info},
                                                     respHostAddress        = HostAddress { hostUnitNumber = unit1 info , hostGroupNumber = group2 info},
                                                     respOpCodeNumber       = opCode info,
                                                     respDataBytesCount     = bCount info,
                                                     respOpCodeDataResponse = resp}

    put response = putByteString $ appendCRC16 (IStrictBS $ runPut putResponse) rocCRC16Config
        where 
          putResponse = do
            putWord8 $ hostUnitNumber  . respHostAddress $ response
            putWord8 $ hostGroupNumber . respHostAddress $ response
            putWord8 $ rocUnitNumber   . respRocAddress $ response
            putWord8 $ rocGroupNumber  . respRocAddress $ response
            putWord8 $ _unOpCodeNumber . respOpCodeNumber $ response
            putWord8 $ _unBytesCount   . respDataBytesCount $ response
            putOpCodeDataResponse $ respOpCodeDataResponse response


getRocMessageInfo :: Get RocMessageInfo
getRocMessageInfo = do
  unitNumber1  <- getWord8
  groupNumber1 <- getWord8
  unitNumber2  <- getWord8
  groupNumber2 <- getWord8
  opCodeNumber <- getWord8
  bytesCount   <- getWord8
  dataByteStr  <- getByteString $ fromIntegral bytesCount
  crc          <- getByteString 2
  let repackBytes = BS.pack [unitNumber1,groupNumber1,unitNumber2,groupNumber2,opCodeNumber,bytesCount]
  if checkCRC16 (IStrictBS $ BS.append repackBytes $ BS.append dataByteStr crc) rocCRC16Config
  then return RocMessageInfo {unit1  = unitNumber1,
                              group1 = groupNumber1,
                              unit2  = unitNumber2,
                              group2 = groupNumber2,
                              opCode = OpCodeNumber opCodeNumber,
                              bCount = BytesCount bytesCount,
                              dataBS = dataByteStr}
  else fail "CRC check failed"

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
                 result = runGet opCodeTableGet (_opCode10DataBytes . respOpCodeDataResponse $ response)
             return  result

testHostAddress :: HostAddress
testHostAddress = HostAddress 1 3

testRocOpCodeRequest :: RocOpCodeRequest
testRocOpCodeRequest = RocOpCodeRequest (RocAddress 240 240) (HostAddress 1 3) (OpCodeNumber 10) $ BS.pack [0,0,44]

testRocOpCodeRequestByteString :: StrictByteString
testRocOpCodeRequestByteString = "\240\240\SOH\ETX\n\ETX\NUL\NUL\ETX\195\247"


-- OLD
-- testRocConfig :: RocConfig
-- testRocConfig = RocConfig "/dev/ttyUSB0" [240,240] [1,3] CS19200 "LOI" 1000

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

myDataMap :: IntMap TlpDataRequest
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

-- testingEncode :: IO ()
-- testingEncode = do
--     let manualByteString = BS.append (opCode10 testRocConfig) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode10 testRocConfig)
--         encodedByteString = encode testRocOpCodeRequest
--     if encodedByteString == BS.fromStrict manualByteString
--     then print ("Successfully encoded RocOpCodeRequest" :: String)
--     else print ("Failed to encode RopOpCodeRequest" :: String)

testingDecodingByteString :: IO ()
testingDecodingByteString =
    if (Right testRocOpCodeRequest)  == (decode testRocOpCodeRequestByteString :: Either String RocOpCodeRequest)
    then print ("Successfully decoded RocOpCodeRequest" :: String)
    else print ("Failed to decode RocOpCodeRequest" :: String)


{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Protocol.ROC.ROCConfig where

import System.Hardware.Serialport
import Data.Word

type BlockNumber  = Word8                                                 
type RocAddress   = [Word8]

data RocConfig = RocConfig {  rocConfigPort         :: FilePath
                             ,rocConfigRocAddress   :: RocAddress
                             ,rocConfigHostAddress  :: RocAddress                              
                             ,rocCommSpeed          :: CommSpeed 
                             ,rocLogin              :: String 
                             ,rocPassword           :: Word16
                             }
                 

data FullyDefinedPointType cfg pn rt pt pc sp rp = FDPT { fdptROCType                  :: rt
                                                        ,fdptPointTypeID               :: pt
                                                        ,fdptParameterCount            :: pc                                                                               
                                                        ,fdptStartParameter            :: sp                                                 
                                                        ,fdptRxProtocol                :: (cfg -> pn -> pt -> pc -> sp -> rp)
                                                        } 
                                       

type ROCType                 = Word8               
--type PointTypeID a           = PointTypes a
type PointNumber             = Word8
type ParameterCount          = Word8          
type StartParameter          = Word8                
type ParameterNumber         = Word8
type PointType               = Word8

--type DefaultPointType = FullyDefinedPointType RocConfig PointNumber ROCType (PointTypes ()) ParameterCount StartParameter (IO BS.ByteString)

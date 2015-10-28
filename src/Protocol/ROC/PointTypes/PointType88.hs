{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType88 where

import Data.Binary.Get    (getByteString,Get)
import Data.ByteString    (ByteString)
import Data.Word          (Word8)
import Prelude            (($),
                           return,
                           Eq,
                           Read,
                           Show)
import Protocol.ROC.Utils (getTLP)

data PointType88 = PointType88 {
 
 pointType88TagID            :: !PointType88TagID                    
,pointType88UnitsString      :: !PointType88UnitsString                    
,pointType88DataTLP          :: !PointType88DataTLP                    

} deriving (Read,Eq, Show)                       

type PointType88TagID        = ByteString           
type PointType88UnitsString  = ByteString           
type PointType88DataTLP      = [Word8]           


pointType88Parser :: Get PointType88
pointType88Parser = do 

  tagID <- getByteString 10                   
  unitsString <- getByteString 10             
  dataTLP <- getTLP                  
  
  return $ PointType88 tagID unitsString dataTLP  

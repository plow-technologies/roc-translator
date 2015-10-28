{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType89 where

import Data.Binary.Get    (getByteString,getWord8,Get)
import Data.ByteString    (ByteString)
import Data.Word          (Word8)
import Prelude            (($),
                           return,
                           Bool,
                           Eq,
                           Float,
                           Read,
                           Show)
import Protocol.ROC.Float (getIeeeFloat32)
import Protocol.ROC.Utils (anyButNull,getTLP)

data PointType89 = PointType89 {
 
 pointType89ChartType                   :: !PointType89ChartType
,pointType89HistPntNumber               :: !PointType89HistPntNumber
,pointType89DynamicPointDataTLPRef      :: !PointType89DynamicPointDataTLPRef
,pointType89TextString                  :: !PointType89TextString
,pointType89UnitsString                 :: !PointType89UnitsString
,pointType89ScalingOption               :: !PointType89ScalingOption
,pointType89UserUpperScaleRange         :: !PointType89UserUpperScaleRange
,pointType89UserLowerScaleRange         :: !PointType89UserLowerScaleRange

} deriving (Read,Eq, Show)                       

type PointType89ChartType               = Word8                        
type PointType89HistPntNumber           = Word8                        
type PointType89DynamicPointDataTLPRef  = [Word8]                        
type PointType89TextString              = ByteString                        
type PointType89UnitsString             = ByteString                        
type PointType89ScalingOption           = Bool                                                        
type PointType89UserUpperScaleRange     = Float                        
type PointType89UserLowerScaleRange     = Float                        

pointType89Parser :: Get PointType89
pointType89Parser = do 

  chartType <- getWord8
  histPntNumber <- getWord8
  dynamicPointDataTLPRef <- getTLP
  textString <- getByteString 10 
  unitsString <- getByteString 10 
  scalingOption <- anyButNull 
  userUpperScaleRange <- getIeeeFloat32 
  userLowerScaleRange <- getIeeeFloat32 
  
  
  return $ PointType89 chartType histPntNumber dynamicPointDataTLPRef textString unitsString scalingOption userUpperScaleRange userLowerScaleRange  
  
  
  

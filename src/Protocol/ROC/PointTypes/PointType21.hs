{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType21 where

import Data.Binary.Get (getByteString,
                        getWord8,
                        getWord32le,
                        Get)
import Data.ByteString (ByteString)
import Data.Word       (Word8,Word32)
import Prelude         (($),
                        return,
                        Eq,
                        Read,
                        Show)

data PointType21 = PointType21 {
 pointType21PointTypeDesc        :: !PointType21PointTypeDesc                      
,pointType21TemplatePointer      :: !PointType21TemplatePointer                   
,pointType21NumParameters        :: !PointType21NumParameters                     
,pointType21DisplayNum           :: !PointType21DisplayNum                        

} deriving (Read,Eq, Show)                       

type PointType21PointTypeDesc    = ByteString           
type PointType21TemplatePointer  = Word32          
type PointType21NumParameters    = Word8             
type PointType21DisplayNum       = Word8             
  
pointType21Parser :: Get PointType21
pointType21Parser = do 

  pointTypeDesc <- getByteString 20     
  templatePointer <- getWord32le   
  numParameters <- getWord8     
  displayNum <- getWord8        
  
  return $ PointType21 pointTypeDesc templatePointer numParameters displayNum



{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType9 where

import Data.Binary.Get    (getByteString,Get)
import Data.ByteString    (ByteString)
import Data.Word          (Word8)
import Prelude            (($),
                           return,
                           Eq,
                           Read,
                           Show)
import Protocol.ROC.Utils (getTLP)

data PointType9 = PointType9 {
 pointType9Line1Text       :: !PointType9Line1Text                                            
,pointType9Line2Text       :: !PointType9Line2Text                              
,pointType9Line3Text       :: !PointType9Line3Text                                       
,pointType9Line1TLP        :: !PointType9Line1TLP                                    
,pointType9Line2TLP        :: !PointType9Line2TLP                                       
,pointType9Line3TLP        :: !PointType9Line3TLP                                      
                      
} deriving (Read, Eq, Show)                       

type PointType9Line1Text   = ByteString    
type PointType9Line2Text   = ByteString            
type PointType9Line3Text   = ByteString           
type PointType9Line1TLP    = [Word8]               
type PointType9Line2TLP    = [Word8]             
type PointType9Line3TLP    = [Word8]            

pointType9Parser :: Get PointType9
pointType9Parser = do 
  line1Text <- getByteString 10
  line2Text <- getByteString 10
  line3Text <- getByteString 10
  line1TLP <- getTLP
  line2TLP <- getTLP
  line3TLP <- getTLP
  
  return $ PointType9 line1Text line2Text line3Text line1TLP line2TLP line3TLP                    

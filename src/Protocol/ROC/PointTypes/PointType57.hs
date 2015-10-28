{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType57 where

import Data.Binary.Get (getByteString,
                        getWord8,
                        getWord16le,
                        Get)
import Data.ByteString (ByteString)
import Data.Word       (Word8,Word16)
import Prelude         (($),
                        return,
                        Eq,
                        Read,
                        Show)

data PointType57 = PointType57 {
 
 pointType57OperatorId             :: !PointType57OperatorId                                
,pointType57ListSecurity           :: !PointType57ListSecurity                                                            
,pointType57KeypadSecurity         :: !PointType57KeypadSecurity                                                                    
,pointType57LCDCFGUserList         :: !PointType57LCDCFGUserList                                                                  
,pointType57Password               :: !PointType57Password                                                                
,pointType57UserTimeout            :: !PointType57UserTimeout                                                                   

} deriving (Read,Eq, Show)          

type PointType57OperatorId          = ByteString                                                                                    
type PointType57ListSecurity        = Word8                                                                         
type PointType57KeypadSecurity      = Word8                                                                  
type PointType57LCDCFGUserList      = Word8                                                                    
type PointType57Password            = Word16                                                                      
type PointType57UserTimeout         = Word16                                                                          
                                
pointType57Parser :: Get PointType57
pointType57Parser = do

  operatorid <- getByteString 3                                            
  listsecurity <- getWord8                                   
  keypadsecurity <- getWord8                                 
  lcdcfguserlist <- getWord8                                 
  password <- getWord16le                                       
  userTimeout <- getWord16le                                      
                              
  return $ PointType57 operatorid listsecurity keypadsecurity lcdcfguserlist password userTimeout  

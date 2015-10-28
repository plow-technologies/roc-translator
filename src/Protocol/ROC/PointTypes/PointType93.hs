{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType93 where

import Data.Binary.Get       (getByteString,getWord8,getWord16le,Get)
import Data.ByteString       (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word             (Word8,Word16)
import Prelude               (($),
                              return,
                              Bool,
                              Eq,
 --                           Read,
                              Show)
import Protocol.ROC.Utils     (anyButNull,getPosixTime)

data PointType93 = PointType93 {
 
 pointType93LicenseInstallationStatus                :: !PointType93LicenseInstallationStatus
,pointType93LicenseNumber                            :: !PointType93LicenseNumber
,pointType93ApplicationName                          :: !PointType93ApplicationName
,pointType93ApplicationProvider                      :: !PointType93ApplicationProvider
,pointType93ApplicationCode                          :: !PointType93ApplicationCode
,pointType93ApplicationVersion                       :: !PointType93ApplicationVersion
,pointType93QuantityTotal                            :: !PointType93QuantityTotal
,pointType93QuantityRemaining                        :: !PointType93QuantityRemaining
,pointType93ExpirationData                           :: !PointType93ExpirationData
,pointType93LicenseValidityState                     :: !PointType93LicenseValidityState
,pointType93LicenseCreationDate                       :: !PointType93LicenseCreationDate

} deriving (Eq,Show) --Read                       

type PointType93LicenseInstallationStatus            = Bool                  
type PointType93LicenseNumber                        = Word8                  
type PointType93ApplicationName                      = ByteString                    
type PointType93ApplicationProvider                  = ByteString                          
type PointType93ApplicationCode                      = Word16                          
type PointType93ApplicationVersion                   = ByteString                                                 
type PointType93QuantityTotal                        = Word8                  
type PointType93QuantityRemaining                    = Word8                  
type PointType93ExpirationData                       = POSIXTime                                                 
type PointType93LicenseValidityState                 = Word8                  
type PointType93LicenseCreationDate                   = POSIXTime                    

pointType93Parser :: Get PointType93
pointType93Parser = do 

  licenseInstallationStatus <- anyButNull
  licenseNumber <- getWord8 
  applicationName <- getByteString 20
  applicationProvider <- getByteString 20
  applicationCode <- getWord16le
  applicationVersion <- getByteString 10 
  quantityTotal <- getWord8 
  quantityRemaining <- getWord8 
  expirationData <- getPosixTime 
  licenseValidityState <- getWord8 
  licenseCreationDate <- getPosixTime 
  
  
  return $ PointType93 licenseInstallationStatus licenseNumber applicationName applicationProvider applicationCode applicationVersion quantityTotal quantityRemaining 
    expirationData licenseValidityState licenseCreationDate  

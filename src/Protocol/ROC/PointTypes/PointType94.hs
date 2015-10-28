{-# LANGUAGE NoImplicitPrelude #-}

module Protocol.ROC.PointTypes.PointType94 where

import Data.Binary.Get       (getByteString,getWord8,getWord32le,Get)
import Data.ByteString       (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word             (Word8,Word32)
import Prelude               (($),
                              return,
                              Bool,
                              Eq,
--                           Read,
                              Show)
import Protocol.ROC.Utils    (anyButNull,getPosixTime)

data PointType94 = PointType94 {
 
 pointType94ProgramName                     :: !PointType94ProgramName
,pointType94ProgramVerNum                   :: !PointType94ProgramVerNum
,pointType94UserProgramCreationDate         :: !PointType94UserProgramCreationDate
,pointType94UserCLibraryVerNum              :: !PointType94UserCLibraryVerNum
,pointType94ProgramEnable                   :: !PointType94ProgramEnable
,pointType94ClearProgram                    :: !PointType94ClearProgram
,pointType94ProgramStatus                   :: !PointType94ProgramStatus
,pointType94ProgramDiskSpaceUsed            :: !PointType94ProgramDiskSpaceUsed
,pointType94ProgramDRAMUse                  :: !PointType94ProgramDRAMUse
,pointType94ProgramAutoRestartCounter       :: !PointType94ProgramAutoRestartCounter
,pointType94ProgramEntryPnt                 :: !PointType94ProgramEntryPnt
,pointType94ProgramCRC                      :: !PointType94ProgramCRC

} deriving (Eq, Show) --Read                       

type PointType94ProgramName                 = ByteString         
type PointType94ProgramVerNum               = ByteString         
type PointType94UserProgramCreationDate     = POSIXTime         
type PointType94UserCLibraryVerNum          = ByteString         
type PointType94ProgramEnable               = Bool         
type PointType94ClearProgram                = Bool         
type PointType94ProgramStatus               = Word8         
type PointType94ProgramDiskSpaceUsed        = Word32         
type PointType94ProgramDRAMUse              = Word32         
type PointType94ProgramAutoRestartCounter   = Word32         
type PointType94ProgramEntryPnt             = Word32         
type PointType94ProgramCRC                  = Word32         
  
pointType94Parser :: Get PointType94
pointType94Parser = do 

  programName <- getByteString 20
  programVerNum <- getByteString 12
  userProgramCreationDate <- getPosixTime
  userCLibraryVerNum <- getByteString 12
  programEnable <- anyButNull
  clearProgram <- anyButNull
  programStatus <- getWord8
  programDiskSpaceUsed <- getWord32le
  programDRAMUse <- getWord32le
  programAutoRestartCounter <- getWord32le
  programEntryPnt <- getWord32le
  programCRC <- getWord32le
  
  return $ PointType94 programName programVerNum userProgramCreationDate userCLibraryVerNum programEnable clearProgram programStatus programDiskSpaceUsed programDRAMUse 
    programAutoRestartCounter programEntryPnt programCRC  
  
  
  
  

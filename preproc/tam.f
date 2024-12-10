
      SUBROUTINE PREPROC_TAM(NETWORK_ID,FILELIST,BYEAR,VERBOSE)
      
C***********************************************************************
C     *    Preprocess Texas A&M Mesonet data
C***********************************************************************
      INCLUDE 'preproc.par'
      
      CHARACTER*2  CYR
      INTEGER*4    YR,JDAY,DAY,HR,MIN,YEAR,F1,HRMIN,FLAG_YR
      REAL*4       RAIN,HRAIN(366,0:23),ARAIN,SEC
      
C***  Open filelist
      
      OPEN(UNIT=2,FILE=FILELIST,STATUS = 'OLD')
 2    READ(2,10,END=9999) IFILE
      DO I = 1,366
         DO J = 0,23
            HRAIN(I,J) = 0
         ENDDO
      ENDDO
      TIP1 = 0
      TIP2 = 0
      TIP3 = 0
 
C***  Get sitenumber (4 characters) and call namefile for naming
C***  output filename.
      
      CALL GET_SITE(NETWORK_ID,IFILE,G_NUMBER,IFIND)
      IF(IFIND.EQ.0)GOTO 2
      CALL NAMEFILE(NETWORK_ID,IFILE,G_NUMBER,BYEAR,OFILE)
      
C***  Open input and output data files for each gauge

      WRITE(6,3)IFILE
 3    FORMAT('Opening raw data file: ',A60)
      OPEN(UNIT = 3,FILE=IFILE,STATUS='OLD')
      OPEN(UNIT = 4,FILE = OFILE,STATUS = 'UNKNOWN')
      
      NREC = 1
      READ(BYEAR,4)YR
 4    FORMAT(I2)
 5    READ(3,*,END = 999) F1
      IF(F1.EQ.4)THEN
         BACKSPACE(3)
         READ(3,*,END = 999)F1,JDAY,HRMIN,SEC,RAIN
         print*,f1,jday,hrmin,sec,rain
         RAIN = .01
         HR  = HRMIN/100.
         MIN = HRMIN - (HR*100)

C*** Compute hourly accumulation from tip records
         
         HRAIN(JDAY,HR) = HRAIN(JDAY,HR) + RAIN

         IF(YEAR.GT.0)THEN
            YEAR = YEAR - 1900
         ELSE
            READ(BYEAR,6)YR
 6          FORMAT(I2)
         ENDIF
         
C***  Convert Julian day to calendar day

C*** Check for anomalous tip pattern (.01,.02,.03)


         CALL  GET_DATE(JDAY,DAY,MON,YR)
         IF(YR.GT.LASTYR)THEN
            CLOSE(4)
            WRITE(CYR,8) YR
 8          FORMAT(I2)
            CALL NAMEFILE(NETWORK_ID,IFILE,G_NUMBER,CYR,OFILE)
            
            OPEN(UNIT = 4,FILE = OFILE,STATUS = 'UNKNOWN')
         ENDIF
         ISEC = SEC
         WRITE(4,16)YR,MON,DAY,HR,MIN,ISEC,RAIN
 16      FORMAT(6(I2.2,1X),1X,F7.2)
         
         LASTYR = YR
         TIP1 = TIP2
         TIP2 = TIP3
         NREC = NREC + 1
      ELSE
         BACKSPACE(3)
         READ(3,*,END = 999)F1,YEAR,JDAY,HRMIN,SEC,ARAIN
         YEAR = YEAR -1900
         IF(FLAG_YR.EQ.0)THEN
          IF(YEAR.NE.YR)THEN
            print*,year,yr
            STOP 'Year does not match!'
           ENDIF
          FLAG_YR = 1
       ENDIF
 
         HR = HRMIN/100.
         MIN = HRMIN - (HR*100)
         ABSDIFF= ABS(ARAIN - HRAIN(JDAY,HR-1))
         IF(ABSDIFF.GE..01)THEN
            WRITE(6,*) 'HOURLY ACCUMULATION RECORD DOE NOT EQUAL INTEGRATED
     +TIP RECORD'
            WRITE(6,*)
            WRITE(6,*)'========================================='
            WRITE(6,*)'JDAY  HR |  HOURLY_ACC  TIP_ACC | D(ACC-TIP)'
            WRITE(6,*)'=========================================='
            WRITE(6,17)JDAY,HR,ARAIN,HRAIN(JDAY,HR-1),ARAIN - 
     +      HRAIN(JDAY,HR-1)
 17         FORMAT(1x,I3,2X,I2,2X,"|",1x,F6.2,3X,F6.2,6x,"|",1x,f6.2)
            WRITE(6,*)
            ARAIN = 0
         ENDIF
      ENDIF
      GOTO 5
 999  CONTINUE
      GOTO 2
 9999 CONTINUE
      
      
 10   FORMAT(A60)
      WRITE(6,10)OFILE
      RETURN
      END
      

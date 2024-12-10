       SUBROUTINE PREPROC_TFT(NETWORK_ID,FILELIST,BYEAR,VERBOSE)

C***********************************************************************
C*    Preprocess TFT (from Tokay) rain gauge network
C***********************************************************************

      INCLUDE 'preproc.par'
C***  MODULE FOR PREPROCESSING TFT (from Tokay) RAW DATA
      PARAMETER (ADIM = 20000,MINPERYEAR = 528480)
      CHARACTER*3 CYR
      CHARACTER*4 G_NUMBER
      INTEGER*4 NTIPMIN(MINPERYEAR),BEVENT(500)
      INTEGER*4 TIME(ADIM),DELTAT(ADIM),YEAR(ADIM)
      INTEGER*4 IBYR,YR,MON,DAY,JDAY,HR,MIN,SEC,FLAG_YR
      DOUBLE PRECISION T


      FLAG_YR = 0
     
C***  OPEN FILE LIST CONTAINING INPUT FILES TO BE PROCESSED
      READ(BYEAR,1) IBYR
 1    FORMAT(I2)

      OPEN(UNIT=2,FILE = FILELIST,STATUS = 'OLD')
 2    READ(2,10,END=9999) IFILE
      
C***  SUBROUTINE GET_SITE RETURNS GAUGE NUMBER.  THIS SUBROUTINE IS CALLED
C***  WHEN GAUGE NUMBER CANNOT BE OBTAINED FROM FILENAME.  IN THIS CASE,
C***  THE MODULE GETS G_NUMBER FROM SITELIST FILE BY KEYING ON GNAME.
      
      CALL GET_SITE(NETWORK_ID,IFILE,G_NUMBER,IFIND)
      IF(IFIND.EQ.0)GOTO 2
C***  INTERNAL WRITE OF OUTPUT FILE NAME
      
      
 10   FORMAT(A60)
      
      DO I =1,MINPERYEAR
         NTIPMIN(I) = 0
      ENDDO
      
      OPEN(UNIT=3,FILE=IFILE,STATUS='OLD')
      LEAP = 0
      MTIP = 1
      NREC = 1
 5    READ(3,15,END = 999)MON,DAY,YR,HR,MIN,SEC,RAIN
c      print*,MON,DAY,YR,HR,MIN,SEC,RAIN
 15   FORMAT(6(I2,1X),1X,F7.2)

       IF(FLAG_YR.EQ.0)THEN
          IF(YR.NE.IBYR)THEN
             STOP 'Year does not match! '
          ENDIF
          FLAG_YR = 1
       ENDIF
      
C***  COMPUTE TIME IN SECONDS OF NUMBER FOR EACH TIP.  DATA SAMPLING IS EVERY
C***  5 SECONDS. MULTIPLE TIPS ARE DISTRIBUTED ACROSS EVEN TIME INTERVALS OVER 
C***  A 5 SECOND PERIOD
      
      JDAY = JULDAY(YR,MON,DAY)
      NTIP = RAIN*100           ! # TIPS PER RECORD
      IF(NTIP.GT.1)THEN
         DTIP = 5./NTIP        ! TIME INTERVAL BETWEEN TIPS I < 5S
         CURSECOND_OF_MIN = SEC
         DO I =1,NTIP
            ISEC = DTIP*(I -.5)
            SEC =   CURSECOND_OF_MIN - 5 + ISEC
            TIME(NREC) = JDAY*24*3600 + HR*3600 + MIN*60 + SEC
         ENDDO
      ELSE
         TIME(NREC) = JDAY*24*3600 + HR*3600 + MIN*60 + SEC
      ENDIF
      YEAR(NREC) = YR
      NMIN = JDAY*24*60 + HR*60 + MIN
      NTIPMIN(NMIN) = NTIPMIN(NMIN)  + 1 ! #TIPS/MINUTE INDEXED BY MINUTE
      
C     BEVENT is the beginning record of an event for which ntip > 5
C     the record number is relative to the beginning of the file, and
c     consistent with indexing of time and year. MTIP is an index representing
c     the sequential number of events satisfying the above criteria.  The
c     purpose of mtip is to simply the recall of only events satisfying the 
c     above condition. 
      
      
      IF(LASTMIN.NE.NMIN.AND.IFIRST.NE.0)THEN
         IF(NTIPMIN(LASTMIN).GT.5)THEN
            BEVENT(MTIP) = NREC - NTIPMIN(LASTMIN)
            MTIP = MTIP + 1
         ENDIF
      ENDIF
      
      IFIRST = 1
      LASTTIME = TIME(NREC)
      LASTMIN  = NMIN
      NREC = NREC + 1
      GOTO 5
      
 999  CONTINUE
      IF(MOD(IBYR,4).EQ.0)LEAP = 1
      CALL RECOM_DT(TIME,LEAP,DELTAT,NREC)
      CALL NAMEFILE(NETWORK_ID,IFILE,G_NUMBER,BYEAR,OFILE)
 7    FORMAT(A3,I4.4,"_",A2,'.gag')
      WRITE(6,'("Writing to file: ",A60)')OFILE
      OPEN(UNIT = 4,FILE = OFILE,STATUS = 'UNKNOWN')
      DO I = 1,NREC-1
         T = TIME(I)
         YR = YEAR(I)
         IF(YR.GT.LASTYR)THEN
            CLOSE(4)
            WRITE(CYR,8)YR
 8          FORMAT(I2)
            CALL NAMEFILE(NETWORK_ID,IFILE,G_NUMBER,CYR,OFILE)
            OPEN(UNIT = 4,FILE = OFILE,STATUS = 'UNKNOWN')
         ENDIF
         CALL GET_TIME(T,JDAY,IHR,IMIN,ISEC)
         WRITE(4,53) YEAR(I),JDAY,IHR,IMIN,ISEC,TIME(I),DELTAT(I)
 53      FORMAT(I2,1X,I3,3(1X,I2),1X,I8,1X,I7)
         LASTYR = YR
      ENDDO
      CLOSE(4)
      GOTO 2
 9999 CONTINUE
      
      WRITE(6,*) 'END OF INPUT'
      RETURN
      END

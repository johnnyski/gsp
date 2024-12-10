      SUBROUTINE PREPROC_HAR(NETWORK_ID,FILELIST,BYEAR,VERBOSE)
      
C**************************************************************************
C     Preprocess Harris County Data
C**************************************************************************
      
      INCLUDE 'preproc.par'
      PARAMETER    (ADIM = 5000,INCH2MM = 25.4)
      CHARACTER*4  G_NUMBER
      CHARACTER*8  TIME(ADIM)
      CHARACTER*11 DATE(ADIM)
      REAL*4       RAIN(ADIM),TIP(ADIM)
      
      INDX  = 1
      NREC  = 1
      IUNIT = 7      ! UNIT NUMBER OF OUTPUT FILE
      LUNIT = 3      ! UNIT NUMBER OF LOGFILE
      TIME_REC = 0
      NR = 0
      NEWNR = 0
      READ(BYEAR,1) IBYR
 1    FORMAT(I2)
      OPEN(UNIT = 1,FILE = FILELIST,STATUS = 'OLD') 
c     OPEN(UNIT = 3,FILE = LOGFILE, STATUS = 'UNKNOWN')     
 4    READ(1,10,END = 999)IFILE
      POS = INDEX(IFILE,'.dat')
      IF(POS.EQ.0) POS = INDEX(IFILE,'.DAT')
      IF(POS.EQ.0) THEN
         WRITE(6,9) 'Data was not found! '
         STOP 'Bye! '
      ENDIF
      IF(IFILE(POS-4:POS-4).NE.'0') THEN
         WRITE(6,'("FILE TYPE: ",A1)')IFILE(POS-4:POS-4)
         WRITE(6,9) IFILE
 9       FORMAT(' ','File does not contain rain gauge data: ',a40)
         GOTO 4
      ENDIF
 10   FORMAT(A60)
      OPEN(UNIT = 2,FILE = IFILE,STATUS = 'OLD')
      NR = 1
 5    READ(2,17,END= 99) DATE(NR),TIME(NR),RAIN(NR)
 17   FORMAT(A8,1X,A8,1X,F10.8)
c     write(6,17) DATE(NR),TIME(NR),RAIN(NR)
      IF(RAIN(NR).GT.1000)GOTO 5
      IF(RAIN(NR).LT..0001)RAIN(NR) = 10**8*RAIN(NR)

      NR = NR + 1
      GOTO 5
 99   CONTINUE
      NR = NR - 1
      CALL GET_TIP(NR, RAIN, TIP)

C*** First Argument of CHECK_TIME_SEPAR passes a data flag indicating whether
C      or not the date is being passed in year 2000 compatible format 
C      (e.g. 12/25/1997).  If IDATA = 1, THEN YEAR IS CHAR*2, ELSE CHAR*4)
      CALL GET_SITE(NETWORK_ID, IFILE, G_NUMBER, IFIND)
c  these are useful for STJ     
c     WRITE(LUNIT,*)
c     WRITE(LUNIT,*)'_________________________________________'
c     WRITE(LUNIT,21) G_NUMBER
c21   FORMAT('Preprocessing data from gauge number: ', A44)      

      CALL CHECK_TIME_SEPAR(LUNIT,1,NR, DATE, TIME, TIP, NEWNR,IVERBOSE)

      IF(IFIND.EQ.0)GOTO 4
      CALL NAMEFILE(NETWORK_ID,IFILE,G_NUMBER,BYEAR,OFILE)
      NEWNR = NEWNR - 1
      CALL WRITE_HAR(NEWNR,IBYR,DATE,TIME,TIP,IUNIT,OFILE)
      GOTO 4
 999  CONTINUE
      RETURN
      END

      SUBROUTINE WRITE_HAR(NR,IBYR,DATE,TIME,TIP,IUNIT,OFILE)
C****************************************************************************
C*  THIS ROUTINE WRITES Harris County  DATA TO FILE IN RGMIN INPUT FORMAT.
C***************************************************************************

      CHARACTER*60 OFILE
      CHARACTER*8  TIME(NR)
      CHARACTER*11 DATE(NR)
      REAL*4      TIP(NR)
      INTEGER*4   MON,JDAY,DAY,YR,HR,MIN,SEC,YEAR,FLAG_YR
      INTEGER*4   FSEEK,HROFF,MINOFF,DST_FLAG
      INTEGER*4   DAYLIGHT_SAVING_TIME
      FLAG_YR = 0
      HROFF  = 6   !TIME DIFFERENCE BETWEEN EST AND UTC
      MINOFF = 0
      LEAP   = 0
      WRITE(6,'("Writing to  file: ",A60)')OFILE
      OPEN(UNIT = IUNIT,FILE = OFILE, STATUS = 'UNKNOWN')
      IF(FSEEK(IUNIT,0,2).NE.0)THEN
         CALL PERROR(OFILE)
      ENDIF
      DO I = 1,NR
C
C*** ONLY TIPS EXCEEDING 0 MM ARE WRITTEN TO FILE.
C
         IF(TIP(I).GT.0)THEN
            READ(DATE(I),14)MON,DAY,YR
            IF(FLAG_YR.EQ.0)THEN
               IF(YR.NE.IBYR)THEN
                  WRITE(6,*) 'YEAR: ',YR, ' BEG. YEAR: ',IBYR
                  STOP 'Year does not match! '
               ENDIF
               FLAG_YR = 1
            ENDIF
            READ(TIME(I),14)HR,MIN,SEC
 14         FORMAT(I2,1X,I2,1X,I2)
            JDAY = JULDAY(YR,MON,DAY)
            IF(MOD(YR,4).EQ.0)LEAP = 1
            CALL GET_DATE(JDAY,DAY,MON,YR)
            YEAR = 1900 + YR
            DST_FLAG = DAYLIGHT_SAVING_TIME(MON,DAY,YR)
            IF(DST_FLAG.EQ.1)HROFF = 5
            CALL ANY2UTC(HROFF,MINOFF,LEAP,YR,JDAY,HR,MIN)
            CALL GET_DATE(JDAY,DAY,MON,YR) !Calendar dates are also changed using DST
            WRITE(IUNIT,15)YR,JDAY,MON,DAY,HR,MIN,SEC,TIP(I)
C            WRITE(6,15)YR,JDAY,MON,DAY,HR,MIN,SEC,TIP(I)

 15         FORMAT(I2,1X,I3,1X,5(I2.2,1X),F9.3)
            TIP(I) = 0
            TIME(I) = ''
            DATE(I) = ''
         ENDIF
      ENDDO
      CLOSE(IUNIT)
      RETURN
      END
      

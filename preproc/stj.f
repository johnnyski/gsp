C*************************************************************************
C* THIS SUBROUTINE PREPROCESSES THE RAW DATA FROM THE ST JOHNS WMD TELEMETRY
C* NETWORK SO THAT IT CAN BE INGESTED BY THE RGMIN PROGRAM WHICH GENERATES
C* 1-MINUTE RAIN RATES.
C**************************************************************************
      SUBROUTINE PREPROC_STJ(NETWORK_ID,FILELIST,BYEAR,VERBOSE)
      INCLUDE 'preproc.par'
      PARAMETER    (ADIM = 5000)
      CHARACTER*60 DHEADER,LOGFILE
      CHARACTER*11  D(ADIM),DATE(ADIM)
      CHARACTER*5  DATA
      CHARACTER*6  thresholday
      CHARACTER*7  F1,KEY
      CHARACTER*9  DSTRING
      CHARACTER*8  T(ADIM),TIME(ADIM)
      CHARACTER*4  G_NUMBER,FORMAT,KEY2,NEWFILE
      CHARACTER*1  C
      REAL*4       R(ADIM),RAIN(ADIM),TIP(ADIM)
      INTEGER*4    GAUGEN,DFLAG,MON,DAY,YEAR,VERBOSE
      CHARACTER*4 AGAUGEN
C      COMMON /GDATA/ D,T,R
 
      IFLAG = 0
      FIRST_REC = 0
      FORMAT = ''
      IDATA = 0
      IUNIT = 7                 ! UNIT NUMBER OF OUTPUT FILE
      LUNIT = 3                 ! UNIT NUMBER OF LOGFILE

      LOGFILE = ''
      READ(BYEAR,1) IBYR
      CALL GET_CURRENT_DATE(MON,DAY,YEAR)
      WRITE(DSTRING,2)MON,DAY,YEAR
 2    FORMAT(I2.2,I2.2,I4.4)
       LOGFILE(1:3) = NETWORK_ID
       LOGFILE(4:11) = DSTRING
       LOGFILE(12:15) = '.log'
C       write(6,'("LOGFILE: ",A60)')LOGFILE

      PRINT*,'VERBOSE: ', VERBOSE
 1    FORMAT(I2)
      OPEN(UNIT = 1,FILE = FILELIST, STATUS = 'OLD')
      OPEN(UNIT = 3,FILE = LOGFILE, STATUS = 'UNKNOWN')

 3    READ(1,5,END = 999) IFILE
      NEWFILE = 'FALSE'
      FIRST_REC = 0
      
      write(6,5) IFILE

      DATA = 'FALSE'
 5    FORMAT(A60)
      OPEN(UNIT = 2,FILE = IFILE,STATUS = 'OLD')
 6    READ(2,7,END = 99)F1
 7    FORMAT(A6)
      IF(FIRST_REC.EQ.0)THEN
         KEY(1:6) = F1(1:6)
         FIRST_REC = 1
      ELSE
         DO I = 1,6
            C = F1(I:I)
            IF(C.EQ.'/')THEN
               DATA = 'TRUE'
            ELSE
               DATA = 'FALSE'
            ENDIF
            
            IF(C.EQ.'D')THEN
               KEY2(1:4) = F1(I:I+4)
               IF(KEY2.EQ.'DATE'.OR.KEY2.EQ.'Date')NEWFILE = 'TRUE'
            ENDIF
         ENDDO
      ENDIF
      IF(KEY(1:6).EQ.'Sensor')THEN
         FORMAT(1:3) = 'OLD'
         DFLAG = 1
      ELSE
         FORMAT(1:3) = 'NEW'
         DFLAG = 0
      ENDIF

C*** Identify gauge number in header by keying on word 'Sensor'. This
C*** attribute consistent from one raw file to another.  Multiple gauges 
C*** in each raw file requires data for each individual gauge be initially
C*** parsed.

      IF(NEWFILE.EQ.'TRUE')THEN
      
C*** If IFLAG equal one module has reached the next gauge in file.  
C*** Current gauge is subsequently processed by writing out stored record
C*** to output file.

         IF(IFLAG.EQ.1) THEN
            NR = NR -1

C*** DSORT reorders data from most recent record in file to 1st record relative
C*** to chronology of individual gauge.

            CALL DSORT(D,T,R,NR,DATE,TIME,RAIN)
C*** Get_tip computes the amount of rainfall delivered from each record based
C*** on the running accumulation totals.

             CALL GET_TIP(NR,RAIN,TIP)

C*** First Argument of CHECK_TIME_SEPAR passes a data flag indicating whether
C      or not the date is being passed in year 2000 compatible format 
C      (e.g. 12/25/1997).  If IDFLAG = 1, THEN YEAR IS CHAR*2, ELSE CHAR*4)
             CALL CHECK_TIME_SEPAR(LUNIT,DFLAG,NR,DATE,TIME,
     + TIP,NEWNR,VERBOSE)

C*** Write ouput data to file
             NEWNR = NEWNR - 1
             CALL WRITE_STJ(FORMAT,NEWNR,IBYR,DATE,TIME,TIP,
     +            IUNIT,OFILE)
          ENDIF


C*** Having found sensor go back to one record and read gauge number.

          IF(FORMAT.EQ.'NEW')THEN
             BACKSPACE 2
             BACKSPACE 2
             BACKSPACE 2
             READ(2,11)GAUGEN
             READ(2,13)DHEADER
             READ(2,13)DHEADER

          ELSEIF(FORMAT.EQ.'OLD')THEN
             BACKSPACE 2
             BACKSPACE 2
             READ(2,12)AGAUGEN
             READ(2,13)DHEADER
             READ(AGAUGEN,34)GAUGEN
             write(6,*)gaugen
 34          FORMAT(I4)

          ELSE
             STOP 'ERROR: CHECK FILE, Unknown data format,' 
          ENDIF
 11       FORMAT(I4)
 12       FORMAT(9X,A4)
c 12       FORMAT(9X,I4)
 13       FORMAT(A24)
          WRITE(LUNIT,*)
          WRITE(LUNIT,*) '_________________________________________'
          WRITE(LUNIT,21) GAUGEN
 21       FORMAT('Preprocessing data from gauge number: ', I4.4)
          WRITE(G_NUMBER,16)GAUGEN
 16       FORMAT(I4.4)
          
C***    Call NAMEFILE to name output file.

          CALL NAMEFILE(NETWORK_ID,IFILE,G_NUMBER,BYEAR,OFILE)
c          WRITE(OFILE,12) NETWORK_ID,G_NUMBER,BYEAR


          IFLAG = 1
          NR = 1
          NEWFILE = 'FALSE'
          GOTO 6
       ELSEIF(DATA(1:4).EQ.'TRUE')THEN

C***  Backspace 1 record and begin reading data

          BACKSPACE(2)
          IF(FORMAT.EQ.'OLD')THEN
             READ(2,17,END = 99)D(NR),T(NR),R(NR)
C             WRITE(6,17) D(NR),T(NR),R(NR)
 17          FORMAT(A8,1X,A8,1X,F6.2)
C             print*, 'OLD Format'
          ELSE
             READ(2,18,END = 99)D(NR),T(NR),R(NR)
C            WRITE(6,18) D(NR),T(NR),R(NR)
 18          FORMAT(A10,1X,A8,1X,F7.2)
C             print*, 'NEW Format'
          ENDIF
          NR = NR + 1
          IDATA = 0
          GOTO 6
       ELSE
          IF(F1.EQ.'No data in')THEN
             WRITE(3,33) GAUGEN, IFILE
 33          FORMAT('No data available for gauge number', I4.4,
     +            'for file: ',A60)
          ENDIF

             
          IDATA = 0
          GOTO 6
       ENDIF
  99   CONTINUE
       NR = NR -1
       
C***  DSORT reorders data from most recent record in file to 1st record relative
C*** to chronology of individual gauge.
       
       CALL DSORT(D,T,R,NR,DATE,TIME,RAIN)
C***  Get_tip computes the amount of rainfall delivered from each record based
C***  on the running accumulation totals.
       
       CALL GET_TIP(NR,RAIN,TIP)
       
C***  First Argument of CHECK_TIME_SEPAR passes a data flag indicating whether
C     or not the date is being passed in year 2000 compatible format 
C     (e.g. 12/25/1997).  If IDFLAG = 1, THEN YEAR IS CHAR*2, ELSE CHAR*4)
       CALL CHECK_TIME_SEPAR(LUNIT,DFLAG,NR,DATE,TIME,
     +      TIP,NEWNR,VERBOSE)
       
C***  Write ouput data to file
       NEWNR = NEWNR - 1
       CALL WRITE_STJ(FORMAT,NEWNR,IBYR,DATE,TIME,TIP,
     +      IUNIT,OFILE)
       IFLAG =0
c     pos=index(ifile,".")
c     thresholday=IFILE(pos-6:pos-1)
c     read(thresholday,191) Nmon,Nday,Nyr 
c191    format(i2,i2,i2)
c	jthresholday=JULDAY(Nyr,Nmon,Nday)
c        jthresholday = jthresholday -1
      
       GOTO 3

 999   CONTINUE
       CLOSE(IUNIT)
       
       RETURN   
       END

       SUBROUTINE DSORT(D,T,R,NR,DATE,TIME,RAIN)
C****************************************************************************
C*  DSORT SORTS DATA FROM EARLIEST TO LATEST RECORD.  RAW DATA RECEIVED BEGINS
C*  WITH MOST RECENT RECORD AND ENDS WITH EARLIEST, THUS SUBROUTINE SIMPLY
C*  REORDERS THE INPUT ARRAYS, D,T, AND R
C****************************************************************************
       PARAMETER    (ADIM = 5000)
       CHARACTER*11 D(ADIM),DATE(ADIM)
       CHARACTER*8 T(ADIM),TIME(ADIM)
       REAL*4      R(ADIM),RAIN(ADIM)

C*** THE COMMON BLOCK GDATA INCLUDES DATE,TIME, AND PRECIP INFO FOR
C*** EACH GAUGE.  THESE HAVE PARAMETERIZED OF ADIM AND ARE REINITIALIZED
C*** PRIOR TO PROCESSING EACH RAIN GAUGE DATA SET.

C       COMMON /GDATA/ D,T,R

       IF(NR.GT.ADIM)STOP 'PROGRAM EXECUTION TERMINATED: NUMBER OF
     + GAUGE RECORDS EXCEED THE PARAMETERIZED DIMENSIONS ON ARRAY'
       NRS = 1
       LEAP = 0
C*** REORDER DATA SUCH THAT EARLIEST RECORD IS FIRST IN FILE AND MOST RECENT
C*** RECORD IS LAST.

       DO I = NR,1,-1
          DATE(NRS) = D(I)
          TIME(NRS) = T(I)
          RAIN(NRS) = R(I)
          NRS = NRS + 1

       ENDDO

C*** INITIALIZE ARRAYS D,T,AND R      

       DO J = 1,NR
          D(J) = ''
          T(J) = ''
          R(J) = 0
       ENDDO

       RETURN
       END

      SUBROUTINE WRITE_STJ(FORMAT,NR,IBYR,DATE,TIME,TIP,
     +      IUNIT,OFILE)
C****************************************************************************
C*  THIS ROUTINE WRITES STJ JOHNS DATA TO FILE IN RGMIN INPUT FORMAT.
C***************************************************************************

      CHARACTER*60 OFILE
      CHARACTER*11 DATE(NR)
      CHARACTER*8 TIME(NR)
      CHARACTER*4 FORMAT
      REAL*4      TIP(NR)
      INTEGER*4   MON,JDAY,DAY,YR,HR,MIN,SEC,FLAG_YR
      INTEGER*4   FSEEK,HROFF,MINOFF

      FLAG_YR = 0
      HROFF  = 5   !TIME DIFFERENCE BETWEEN EST AND UTC
      MINOFF = 0
      WRITE(6,'("Writing to file: ",A60)')OFILE
      OPEN(UNIT = IUNIT,FILE = OFILE, STATUS = 'UNKNOWN')
      IF(FSEEK(IUNIT,0,2).NE.0)THEN
         CALL PERROR(OFILE)
      ENDIF
      DO I = 1,NR
C
C*** ONLY TIPS EXCEEDING 0 MM ARE WRITTEN TO FILE.
C
         IF(TIP(I).GT.0)THEN
            IF(FORMAT.EQ.'OLD')THEN
               READ(DATE(I),14)MON,DAY,YR
            ELSE
               READ(DATE(I),15)MON,DAY,YR
               YR = YR - 1900
             ENDIF

c             IF(FLAG_YR.EQ.0)THEN
c                  IF(YR.NE.IBYR)THEN
c                     STOP 'Year does not match! '
c                  ENDIF
c                FLAG_YR = 1
c            ENDIF

            READ(TIME(I),14)HR,MIN,SEC
            IF(MOD(YR,4).eq.0)LEAP = 1
 14         FORMAT(I2,1X,I2,1X,I2)
 15         FORMAT(I2,1X,I2,1X,I4)
            JDAY = JULDAY(YR,MON,DAY)
            CALL ANY2UTC(HROFF,MINOFF,LEAP,YR,JDAY,HR,MIN)
            CALL GET_DATE(JDAY,DAY,MON,YR)
           WRITE(IUNIT,16)YR,MON,DAY,HR,MIN,SEC,TIP(I)
c       else
c          WRITE(6,166)YR,MON,DAY,HR,MIN,SEC,TIP(I),JDAY,jthresholday
c       endif
 16         FORMAT(6(I2.2,1X),F7.2)
 166        FORMAT(6(I2.2,1X),F7.2,2I8)
            TIP(I) = 0
         ENDIF
      ENDDO
      RETURN
      END
      

      SUBROUTINE PREPROC_DAR(NETWORK_ID,FILELIST,BYEAR,VERBOSE)
      
C ********************************************
C *    Precprocessing module for Darwin.  
C ********************************************
      INCLUDE 'preproc.par'
      PARAMETER (ADIM = 20000,MINPERYEAR = 528480)
      CHARACTER*4  G_NUMBER
      INTEGER*4    NTIPMIN(MINPERYEAR),BEVENT(500)
      INTEGER*4    DELTAT(ADIM),YEAR(ADIM),TIME(ADIM)
      INTEGER*4    YR,JDAY,HR,MIN,SEC,IBYR,FLAG_YR,LEAP
      DOUBLE PRECISION  T
      
C***  Open filelist

      FLAG_YR = 0
      READ(BYEAR,1) IBYR
 1    FORMAT(I2)
      write(6,*) 'IBYR=',IBYR,'    BYEAR=', BYEAR
      OPEN(UNIT=2,FILE=FILELIST,STATUS = 'OLD')
 2    READ(2,10,END=9999) IFILE
      CALL  GET_SITE(NETWORK_ID,IFILE,G_NUMBER,IFIND)
      IF(IFIND.EQ.0)GOTO 2
 10   FORMAT(A60)
      
      DO I =1,MINPERYEAR
         NTIPMIN(I) = 0
      ENDDO

      
C***  Open data input file
      
       OPEN(UNIT=3,FILE=IFILE,STATUS='OLD')

C*** Initialize variables

       LEAP = 0
       MTIP = 1
       NREC = 1
  5    READ(3,15,END = 999)YR,JDAY,HR,MIN,SEC
  15   FORMAT(I2,I3,I2,I2,I2)
c
       IF(FLAG_YR.EQ.0)THEN
          IF(YR.LT.IBYR)THEN
             STOP 'ERROR: Year does not match'
          ELSEIF((YR.EQ.IBYR+1).AND.(JDAY.GT.180))THEN
             STOP 'ERROR: Year does not match'
          ELSEIF(YR.GT.IBYR+1)THEN
             STOP 'ERROR: Year does not match'
          ENDIF
          FLAG_YR = 1
       ENDIF
 

       TIME(NREC) = JDAY*24*3600 + HR*3600 + MIN*60 + SEC
       YEAR(NREC) = YR
       NMIN = JDAY*24*60 + HR*60 + MIN    !minute of year
       NTIPMIN(NMIN) = NTIPMIN(NMIN)  + 1  !number of tips for minute of year

C*** Check number of tips per minute.  Flag if greater than 5.

       IF(LASTMIN.NE.NMIN.AND.IFIRST.NE.0)THEN
          IF(NTIPMIN(LASTMIN).GT.5)THEN
             BEVENT(MTIP) = NREC - NTIPMIN(LASTMIN)
             MTIP = MTIP + 1
          ENDIF
       ENDIF
       
       IFIRST = 1
       LASTMIN  = NMIN
       LASTTIME = TIME(NREC)
       NREC = NREC + 1
       GOTO 5

  999  CONTINUE
       print*,'There were ', mtip, ' multiple records read from file.'
       WRITE(6,*)
       WRITE(6,*) 'There were ',NREC,' records read from file.'
       WRITE(6,*)

C*** Call subroutines for treating special situations that were flagged 
C*** during the reading of data records. See subroutine descriptions.
       IF(MOD(IBYR,4).EQ.0)LEAP = 1
       CALL CHK_TIME(TIME,NTIPMIN,BEVENT,MTIP)
       CALL CHK_2TIP(TIME,LEAP,NREC)
       CALL RECOM_DT(TIME,LEAP,DELTAT,NREC)
       CALL NAMEFILE(NETWORK_ID,IFILE,G_NUMBER,BYEAR,OFILE)
       DO I = 1,MTIP
          BEVENT(I) = 0
       ENDDO

       WRITE(6,'(" Opening Output: ",A60)')OFILE
       OPEN(UNIT = 4,FILE = OFILE,STATUS = 'UNKNOWN')
       DO I = 1,NREC-1
          T = TIME(I)
          ID = DELTAT(I)
          CALL GET_TIME(T,JDAY,IHR,IMIN,ISEC)
          WRITE(4,53) YEAR(I),JDAY,IHR,IMIN,ISEC,TIME(I),DELTAT(I)
  53      FORMAT(I2,1X,I3,3(1X,I2),1X,I8,1X,I7)
       ENDDO
       CLOSE(4)
       GOTO 2
 9999 CONTINUE
       END

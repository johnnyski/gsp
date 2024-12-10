      SUBROUTINE JULDAY(IYEAR,IMON,IDAY,JDAY,LEAPYR)
C     *******************************************************
C     * CONVERT FROM YEAR, MONTH AND DAY TO JULIAN DAY	*
C     *******************************************************

      CHARACTER*3	AA(12)

      DIMENSION 	MM(12),JJ(12)

      DATA AA /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     +     'SEP','OCT','NOV','DEC'/
      DATA MM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/ ! # OF DAYS IN MONTH
      DATA JJ /  1, 32, 60, 91,121,152,182,213,244,274,305,335/ ! JULIAN DAY OF FIRST DAY OF MONTH
C     
C     ****** DECLARE SOME CONSTANTS
C     
      IPUNIT = 9                ! UNIT FOR WRITING TO THE SCREEN
C     
C     ****** MAKE SURE THAT ALL TIME PARAMETERS ARE VALID
C     
      IF(MOD(IYEAR,4).EQ.0)THEN
         LEAPYR=1
      ELSE
         LEAPYR=0
      ENDIF 

      IERROR = 0
      IF(IYEAR.GT.2000) THEN
         WRITE(IPUNIT,*) 'ERROR IN VALUE: YEAR',IYEAR
         IERROR=1
      ENDIF
      IF(IMON.GT.12) THEN	
         WRITE(IPUINIT,*) 'ERROR IN VALUE:MONTH',MONTH
         IERROR=1
      ENDIF
      IF(IMON.EQ.1) THEN
         IF(IDAY.GT.31) THEN
		    WRITE(IPUNIT,*) 'ERROR IN VALUE: DAY'
		    IERROR=1
         ENDIF
      ELSE
         IF(IDAY.GT.MM(IMON)+LEAPYR) THEN
		    WRITE(IPUNIT,*) 'ERROR IN VALUE: DAY',IDAY,IMON,MM(IMON),LEAPYR
		    IERROR=1
         ENDIF
      ENDIF
      IF(IERROR.EQ.1) THEN
         PAUSE
         STOP 'LOOK AT TIME VALUES FOR ERROR.....'
      ENDIF
C     
C     ****** NOW, COMPUTE JULIAN DAY FROM THE YEAR, MONTH AND DAY
C     
      IADD = 0
      IF(LEAPYR.EQ.1) THEN
         IF(IMON.GT.2) IADD = 1 ! IF PAST FEB, THEN 1 MORE DAY IN JDAY
      ENDIF
      JDAY = JJ(IMON) + IDAY + IADD - 1
      RETURN
      END
C**********************************************************************
C*    Return day, month, and year given the Julian day.
C*********************************************************************
      SUBROUTINE GET_DATE(IRDAY,IDDAY,IMON,IYEAR)
      INTEGER*4 IRDAY,IDDAY

      IF(IRDAY.LT.0) GOTO 999

      LEAPYR = 0
      IF(MOD(IYEAR,4).EQ.0) LEAPYR = 1	    

      IF((IRDAY .GE. 1) .AND. (IRDAY .LE. 31))THEN
         IMON  = 1
         IDDAY = IRDAY
      ENDIF
      IF(LEAPYR .EQ. 0)THEN
         IF((IRDAY .GE. 32) .AND. (IRDAY .LE. 59))THEN
            IMON  = 2
            IDDAY = IRDAY - 31
         ELSEIF((IRDAY .GE. 60) .AND. (IRDAY .LE. 90))THEN
            IMON  = 3
            IDDAY = IRDAY - 59
         ELSEIF((IRDAY .GE. 91) .AND. (IRDAY .LE.120))THEN
            IMON  = 4
            IDDAY = IRDAY - 90
         ELSEIF((IRDAY .GE. 121) .AND. (IRDAY .LE. 151))THEN
            IMON  = 5
            IDDAY = IRDAY - 120
         ELSEIF((IRDAY .GE. 152) .AND. (IRDAY .LE. 181))THEN
            IMON  = 6
            IDDAY = IRDAY - 151
         ELSEIF((IRDAY .GE. 182) .AND. (IRDAY .LE. 212))THEN
            IMON  = 7
            IDDAY = IRDAY - 181
         ELSEIF((IRDAY .GE. 213) .AND. (IRDAY .LE. 243))THEN
            IMON  = 8
            IDDAY = IRDAY - 212
         ELSEIF((IRDAY .GE. 244) .AND. (IRDAY .LE. 273))THEN
            IMON  = 9
            IDDAY = IRDAY - 243
         ELSEIF((IRDAY .GE. 274) .AND. (IRDAY .LE. 304))THEN
            IMON  = 10
            IDDAY = IRDAY - 273
         ELSEIF((IRDAY .GE. 305) .AND. (IRDAY .LE. 334))THEN
            IMON  = 11
            IDDAY = IRDAY - 304
         ELSEIF((IRDAY .GE. 335) .AND. (IRDAY .LE. 365))THEN
            IMON  = 12
            IDDAY = IRDAY - 334
         ELSEIF((IRDAY .GT.365) .OR.(IRDAY .LT. 1))THEN
            WRITE(6,*)'Error on input line, Try again'
            GOTO 999
         ENDIF
      ELSE
         IF((IRDAY .GE. 32) .AND. (IRDAY .LE. 60))THEN
            IMON  = 2
            IDDAY = IRDAY - 31
         ELSEIF((IRDAY .GE. 61) .AND. (IRDAY .LE. 91))THEN
            IMON  = 3
            IDDAY = IRDAY - 60
         ELSEIF((IRDAY .GE. 92) .AND. (IRDAY .LE.121))THEN
            IMON  = 4
            IDDAY = IRDAY - 91
         ELSEIF((IRDAY .GE. 122) .AND. (IRDAY .LE. 152))THEN
            IMON  = 5
            IDDAY = IRDAY - 121
         ELSEIF((IRDAY .GE. 153) .AND. (IRDAY .LE. 182))THEN
            IMON  = 6
            IDDAY = IRDAY - 152
         ELSEIF((IRDAY .GE. 183) .AND. (IRDAY .LE. 213))THEN
            IMON  = 7
            IDDAY = IRDAY - 182
         ELSEIF((IRDAY .GE. 214) .AND. (IRDAY .LE. 244))THEN
            IMON  = 8
            IDDAY = IRDAY - 213
         ELSEIF((IRDAY .GE. 245) .AND. (IRDAY .LE. 274))THEN
            IMON  = 9
            IDDAY = IRDAY - 244
         ELSEIF((IRDAY .GE. 275) .AND. (IRDAY .LE. 305))THEN
            IMON  = 10
            IDDAY = IRDAY - 274
         ELSEIF((IRDAY .GE. 306) .AND. (IRDAY .LE. 335))THEN
            IMON  = 11
            IDDAY = IRDAY - 305
         ELSEIF((IRDAY .GE. 336) .AND. (IRDAY .LE. 366))THEN
            IMON  = 12
            IDDAY = IRDAY - 335
         ENDIF
      ENDIF
      IYEAR1 = IYEAR
      IF(IYEAR.GT.1900) IYEAR1 = IYEAR - 1900


 999  CONTINUE
      END

      SUBROUTINE COM_LEAP(IYEAR,LEAP)
C     
C*****THIS SUBROUTINE DETERMINES IF IT IS A LEAP YEAR OR NOT AND RETURNS 0 OR C
C     

      IF(MOD(IYEAR,4).EQ.0)THEN
         LEAP=1
      ELSE
         LEAP=0
      ENDIF
      RETURN
      END

      SUBROUTINE GET_TIME(TIME,JDAY,IHR,IMIN)

      TEMP=DFLOAT(TIME/(24.*3600.))
      JDAY=TEMP
      TEMP=DFLOAT((TEMP-JDAY)*24.)
      IHR=TEMP
      IMIN=(TEMP-IHR)*60.
c      write(6,*)'get_time',time,jday,ihr,imin
      RETURN
      END

      SUBROUTINE CH_TIME(IMIN,IHR,JDAY,IYEAR, LEAP)  
      IMIN=IMIN+1    
      IF(IMIN.EQ.60)THEN
         IMIN=0
         IF(IHR.LT.23)THEN
            IHR=IHR+1
         ELSE
            IHR=0
            IF(JDAY.EQ.365+LEAP)THEN
               JDAY= 1
               IYEAR=IYEAR + 1
            ELSE
               JDAY=JDAY+1
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END
      

      

      SUBROUTINE CH_TIME2(IMIN,IHR,JDAY,IYEAR)
      IF(IMIN.LT.0)THEN
         IMIN = 60 + IMIN
         IHR = IHR - 1
         IF(IHR.LT.0)THEN
            IHR = 24 + IHR
            JDAY = JDAY - 1
         ENDIF
      ENDIF
      RETURN
      END

      SUBROUTINE CHECKYR(TIME0,TIME1,IYEAR,LAST_DAY)

      REAL*4 TIME0, TIME1
      IF(TIME0.GT.TIME1)THEN
         IYEAR = IYEAR + 1
         CALL GET_TIME(TIME1,JDAY,IHR,IMIN)
         LAST_DAY = JDAY
c         print*, 'year change'
c      ELSE
c         IYEAR = IYEAR
      ENDIF
      RETURN
      END


      INTEGER FUNCTION MINOFYR_FROM_SEC(TIME)

      MINOFYR_FROM_SEC = NINT(TIME/60.)
      RETURN
      END

      INTEGER FUNCTION MINOFYR_FROM_DATE(JDAY,IHR,MIN)

      MINOFYR_FROM_DATE = JDAY*24*60 + IHR*60 + MIN -24*60
      RETURN
      END

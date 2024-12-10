C************************************************************************
C*    For events where number of tips in a minute are 6 or more these tips
C*    are set at even time intervals apart within the minute (DAR ONLY).
C*************************************************************************
      SUBROUTINE CHK_TIME(TIME,NTIP,BEVENT,MTIP)
      PARAMETER (ADIM = 20000,MINPERYEAR = 528480)
      INTEGER*4 NTIP(MINPERYEAR) 
      INTEGER*4 BEVENT(500),TIME(ADIM)
      DOUBLE PRECISION T0,TIME1
      
      DO I = 1,MTIP-1
         IREC = BEVENT(I)
         NMIN = TIME(IREC)/(60.)
         NTIPMIN = NTIP(NMIN)
         FTIP = 60./NTIPMIN
         TIME1 = TIME(IREC)
            DO J=1,NTIPMIN
               ISEC =(J - 0.5)*FTIP
               TIME(IREC +J-1) = NMIN*60. + ISEC 
               T0 = TIME(IREC+J-1)
               CALL GET_TIME(T0,JDAY0,IHR0,IMIN0,ISEC0)
               idt0 = TIME(IREC +J-1) - TIME(IREC +J-2)
            ENDDO
         ENDDO

      
      RETURN
      END


C *********************************************************************
C * Check data records for double tips (tips separated by less than
C * 5 sec) time adjustment to record is made in order that cubic spline 
C * interpolation can be applied at next level of processing.
C **********************************************************************
      SUBROUTINE CHK_2TIP(TIME,LEAP,NREC)
      INTEGER*4 LEAP,TIME(NREC-1)
      DOUBLE PRECISION T0,T1,T2,T3

      WRITE(6,*)'NUMBER OF RECORDS',NREC  
      DO I = 3,NREC-1
         T0 = TIME(I-2)
         T1 = TIME(I-1)
         T2 = TIME(I)
         T3 = TIME(I+1)
         CALL GET_TIME(T0,JDAY0,IHR0,IMIN0,ISEC0)
         CALL GET_TIME(T1,JDAY1,IHR1,IMIN1,ISEC1)
         CALL GET_TIME(T2,JDAY2,IHR2,IMIN2,ISEC2)
         CALL GET_TIME(T3,JDAY3,IHR3,IMIN3,ISEC3)
         
         IDT0 = T1 - T0
         IF(IDT0.LT.0)THEN
             IDT0=((365+LEAP)*3600*24 - T0)
     +           + T1
         ENDIF
         IDT1 = T2 - T1
         IDT2 = T3 - T2
         IMIN = TIME(I)/60.

         
         IF(IDT1.LT.5.AND.IDT1.GT.0)THEN
            IF(JDAY3.EQ.322)THEN
C               PRINT*,'0 ',JDAY0,IHR0,IMIN0,ISEC0,IDT0
C               PRINT*,'1 ',JDAY1,IHR1,IMIN1,ISEC1,IDT1
C               PRINT*,'2 ',JDAY2,IHR2,IMIN2,ISEC2,IDT2
C               PRINT*,'3 ',JDAY3,IHR3,IMIN3,ISEC3
            ENDIF
            IF(IDT0.LT.180.AND.IDT0.GT.5)THEN
               TIME(I-1) = TIME(I-1) - NINT(IDT0/2.5)
            ELSEIF(IDT2.LT.180.AND.IDT2.GE.5)THEN
               TIME(I)   = TIME(I)   + NINT(IDT2/2.5)
            ELSEIF(IDT0.GE.180)THEN
               TIME(I-1) = TIME(I-1) - 60.0
               TIME(I)   = TIME(I)   
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END

C*************************************************************************
C*    This subroutine computes the time difference between consecutive tip
C*    records (in seconds).  This data is stored in an array and later
C*    written out as a data field in the gag output (DAR, RMI, KWA ONLY).
C*************************************************************************
      SUBROUTINE RECOM_DT(TIME,LEAP,DELTAT,NREC)
      INTEGER*4 DELTAT(NREC-1),LEAP,TIME(NREC-1)

      
      DELTAT(1) = 0
      DO I =2,NREC-1
         DELTAT(I) = 0
         DELTAT(I) = TIME(I) - TIME(I-1)
         IF(DELTAT(I).LT.0)THEN
            DELTAT(I)=((365+LEAP)*3600*24 - TIME(I-1))
     +           + TIME(I)
         ENDIF 
      ENDDO
      RETURN
      END


C **********************************************************************
C * JULDAY: CONVERT FROM YEAR, MONTH AND DAY TO JULIAN DAY	           *
C **********************************************************************
C * Function written by: David B. Wolff                                *
C **********************************************************************
C * Version last modified: Tuesday, October 21, 1997                   *
C **********************************************************************
      INTEGER FUNCTION JULDAY(IYEAR, IMON, IDAY)
	  INTEGER  MONTH(12), LMONTH(12)
	  DATA   MONTH/1,32,60,91,121,152,182,213,244,274,305,335/ 
	  DATA  LMONTH/1,32,61,92,122,153,183,214,245,275,306,336/ 
C
C *** Determine if the given year is a leap year.
C
      IF((MOD(IYEAR,4).EQ.0 .AND. MOD(IYEAR,100).NE.0) .OR. 
     +		MOD(IYEAR,400).EQ.0) THEN
		JULDAY = LMONTH(IMON) + IDAY - 1
	  ELSE
  	  	JULDAY =  MONTH(IMON) + IDAY - 1
	  ENDIF
      RETURN 
      END

            
C **********************************************************************
C * Given the second of the year, GET_TIME returns the Julian 
C * day, hour,  minute and second.
C***********************************************************************
      SUBROUTINE GET_TIME(TIME,JDAY,IHR,IMIN,ISEC)
      DOUBLE PRECISION TIME,TEMP

      TEMP=DFLOAT(TIME/(24.*3600.))
      JDAY=TEMP
      TEMP=DFLOAT((TEMP-JDAY)*24.)
      IHR=TEMP
      IMIN=(TEMP-IHR)*60.
      TEMP = DFLOAT(((TEMP-IHR)*60. - IMIN)*60)
      ISEC = TEMP+0.5
      RETURN
      END
      
C **********************************************************************
C * GET_DATE: Return day and month, given the Julian day and year      *
C **********************************************************************
C * Routine written by: David B. Wolff                                 *
C **********************************************************************
C * Version last modified: Thursday, October 9, 1997                   *
C **********************************************************************
      SUBROUTINE GET_DATE2(JDAY,IDDAY,IMON,IYEAR)
	  INTEGER*4  MONTH(12)
      INTEGER*4  IDAY
	  DATA  MONTH/1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 
     +            335/ 
C
C *** Do some error checking
C
 	  IF(IDAY.LT.0) THEN
	  	WRITE(6,*) "ERROR: Invalid jday in GET_DATE! "
		RETURN
	  ENDIF
C
C *** Determine if the given year is a leap year.
C
      LEAPYR = 0	! Default is NOT leap year
      IF( (MOD(IYEAR,4).EQ.0 .AND. MOD(IYEAR,100).NE.0) .OR. 
     +		MOD(IYEAR,400).EQ.0) LEAPYR = 1	    
C
C *** Make sure that the given day was valied
C 
	  DO I=1,11
	  	IF(JDAY .GE. MONTH(I) .AND. JDAY .LT. MONTH(I+1)) THEN
	  		IMON = I
			IDAY = MONTH(I) + (MONTH(I)-JDAY+1) + LEAPYR
            print*,imon,iday
			RETURN
		END IF
	  END DO
		
 99	  RETURN
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

C***************************************************************************
C*    THIS SUBROUTINE CONVERTS BETWEEN LOCAL TIME STANDARD AND UTC.   THE
C*    ROUTINE RECEIVES AN HOUR OFFSET WHICH IS USED TO DO CONVERSION.  THE
C*    THE DAY, HOUR AND MINUTE ARE ADJUSTED ACCORDINGLY AND PASSED BACK TO
C*    CALLING ROUTINE.
C*    NOTE - HROFF CAN BE POSITIVE OR NEGATIVE DEPENDING ON LONGITUDE OF SITE.
C***************************************************************************
      SUBROUTINE ANY2UTC(HROFF,MINOFF,LEAP,YR,JDAY,HR,MIN)
      INTEGER*4 JDAY,HR,MIN,HROFF,MINOFF,YR

      IF(MINOFF.NE.0)THEN
         MIN = MIN + MINOFF
         IF(MIN.GT.59)THEN
            MIN = MIN - 60
            HR = HR + 1
            IF(HR.GT.23)THEN
               HR = HR - 24
		JDAY = JDAY + 1
               IF(JDAY.GT.365 + LEAP) THEN
		    JDAY = 1
		    YR=YR+1
               ENDIF
            ENDIF
         ELSEIF(MIN.LT.0)THEN
            MIN = 60 + MIN
            HR = HR - 1
            IF(HR.LT.0)THEN
               HR = 24 + HR
               JDAY = JDAY - 1
               IF(JDAY.LT.1)  JDAY = 1
               IF(JDAY.LT.0) WRITE(6,*) 'ERROR: CHECK DATE'
            ENDIF
         ENDIF
      ENDIF
         
      HR = HR + HROFF
      IF(HR.GT.23)THEN
         HR = HR - 24
         JDAY = JDAY + 1
         IF(JDAY.GT.365 + LEAP) THEN
		JDAY = 1
		YR=YR+1
         ENDIF
      ELSEIF(HR.LT.0)THEN
         HR = 24 + HR
         JDAY = JDAY - 1
         IF(JDAY.LT.1)  JDAY = 1
         IF(JDAY.LT.0) WRITE(6,*) 'ERROR: CHECK DATE'
      ENDIF
      RETURN
      END

C ***************************************************************
      SUBROUTINE CHECK_TIME_SEPAR(LUNIT,DFLAG,NR,DATE,TIME,TIP,NEWINDX,
     +  VERBOSE)
      PARAMETER   (ADIM = 5000)
      CHARACTER*11 DATE(ADIM)
      CHARACTER*8  TIME(ADIM)
      REAL*4       TIP(ADIM)
      INTEGER*4    JDAY,DELTAT,MON,DAY,YR,HR,MIN,SEC,DFLAG
      INTEGER*4    VERBOSE,TIPTIME(ADIM)

      NEWINDX = 1

      DO I = 1,NR
         IF(DFLAG.EQ.1)THEN
            READ(DATE(I),14) MON,DAY,YR
         ELSE
            READ(DATE(I),18) MON,DAY,YR
         ENDIF
         READ(TIME(I),14) HR,MIN,SEC
         JDAY = JULDAY(YR,MON,DAY)
         IF(MOD(YR,4).EQ.0)THEN
            LEAP = 1
         ELSE
            LEAP = 0
         ENDIF
 14      FORMAT(I2,1X,I2,1X,I2)
 18      FORMAT(I2,1X,I2,1X,I4)
         TIPTIME(I) = JDAY*3600*24 + HR*3600 + MIN*60 + SEC
         IF(I.GT.1)THEN
            DELTAT = TIPTIME(I) - TIPTIME(I-1)
            IF(DELTAT.LT.0.AND.JDAY.LT.LASTJDAY)THEN
               DELTAT = ((((365+LEAP)*24*3600)+23*3600 + 
     +              59*60+59) - TIPTIME(I-1)) + TIPTIME(I)
            ENDIF
            IF(TIP(I).GT.0)THEN
               IF(DELTAT.GE.4)THEN
                  DATE(NEWINDX) = DATE(I)
                  TIME(NEWINDX) = TIME(I)
                  TIP(NEWINDX)  = TIP(I)
                  NEWINDX       = NEWINDX + 1
               ELSE
                  RATE = 25.4*3600*(TIP(I)/FLOAT(DELTAT))
                  IF(VERBOSE.EQ.1)THEN
                     PRINT*
                     WRITE(6,16) DELTAT
                     WRITE(6,17) RATE
                     WRITE(6,15) DATE(I),TIME(I)
                  PRINT*

                  ENDIF
                  WRITE(LUNIT,*)
                  WRITE(LUNIT,16) DELTAT
                  WRITE(LUNIT,17) RATE
                  WRITE(LUNIT,15) DATE(I),TIME(I)
                  WRITE(LUNIT,*)
 15               FORMAT('Suspect tip record not processed: ',
     +                 A10,1x,A8,)
               ENDIF
            ENDIF
         ELSE
            DATE(NEWINDX) = DATE(I)
            TIME(NEWINDX) = TIME(I)
            TIP(NEWINDX)  = TIP(I)
            NEWINDX       = NEWINDX + 1
         ENDIF
         LASTJDAY = JDAY
      ENDDO
 16   FORMAT('Time separation between records is ',i2,' sec') 
 17   FORMAT('This difference indicates a rainrate of: ',F10.1,' mm/hr')

      RETURN
      END


         
         










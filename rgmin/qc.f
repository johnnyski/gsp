      SUBROUTINE QCMULTI(TIME,ITDX,INDX,NTIPS)

      INCLUDE 'rgmin.par'
      PARAMETER (TIME_INC = 900)
      REAl*4       NTIPS_PER_SEC
	  INTEGER*4    BAD_DAT(0:366*24*60)
      COMMON /BADQC/ BAD_DAT

      IF(ITDX.GE.1)THEN
         DELTAT = TIME - TIPTIME(ITDX)

C**** The next 3 lines are included for error checking, they are not actually used
C**** in the program, but provide easy access to julian day, hour, minute given
C**** the seconds of the year. To effectively use, need to add write statement.

         TIME0 = TIPTIME(ITDX)
         CALL GET_TIME(TIME,JDAY,IHR,IMIN)
         CALL GET_TIME(TIME0,JDAY0,IHR0,IMIN0)

C****************************************************************************************
         NTIPS_PER_SEC = DELTAT/NTIPS
         IF(NTIPS_PER_SEC.LT.5)THEN
            
            CALL GET_TIME(TIME,JDAY,IHR,IMIN)
            CALL GET_DATE(JDAY,IDDAY,IMON,IYEAR)
            WRITE(6,*)
            WRITE(6,*) 'Rain intensity exceeds acceptable limit:'
            WRITE(6,*) 'Rainfall from this event will not be ',
     +                 ' processed'
            WRITE(6,*) 'DATE: ',IMON,'/',IDDAY
            WRITE(6,*) 'TIME: ', IHR,':',IMIN
            WRITE(6,*)
            RETURN
         ENDIF
         LASTTIME = TIPTIME(ITDX)
         IF(DELTAT.LT.0)THEN
            DELTAT = ((((365+LEAP)*24*3600)+23*3600 + 
     +           59*60+59) - TIPTIME(ITDX-1)) + TIPTIME(ITDX)
         ENDIF
         IF(DELTAT.LE.300)THEN
            DTIP = FLOAT(DELTAT/NTIPS)
            DO I = 1,NTIPS
               ITDX = ITDX + 1
               TIPTIME(ITDX) = LASTTIME + DTIP*I
               IF(TIPTIME(ITDX).GT.TIME) WRITE(6,*)'TIPTIME ',
     +              'exceeds time of record, please check',
     +              ITDX,TIPTIME(ITDX),TIME,DELTAT
            ENDDO
         ELSEIF(DELTAT.GT.300.AND.DELTAT.LE.REVENT)THEN
            IF(NTIPS.LT.50)THEN
               DTIP = FLOAT(DELTAT/NTIPS)
               DO I = 1,NTIPS
                  ITDX = ITDX + 1
                  TIPTIME(ITDX) = LASTTIME + DTIP*I
                  IF(TIPTIME(ITDX).GT.TIME) THEN
                     WRITE(6,*)
                     WRITE(6,*)'TIPTIME ',
     +                    'exceeds time of record, please check',
     +                    ITDX,TIPTIME(ITDX),TIME,DELTAT
                     WRITE(6,*)
                  ENDIF
               ENDDO
               
C***  ASSIGN QC FLAG TO SUSPECT DATA. NOTE CONDITIONS REQUIRE THAT
C***  BEGINNING AND END OF BAD DATA PERIOD CANNOT EXCEED 1 DAY


               TIME0 = LASTTIME + DTIP
               CALL GET_TIME(TIME0,JDAY1,IHR1,IMIN1)               
               CALL GET_TIME(TIME,JDAY2,IHR2,IMIN2)                                  
         
               MIN1 = MINOFYR_FROM_DATE(JDAY1,IHR1,IMIN1)
               MIN2 = MINOFYR_FROM_DATE(JDAY2,IHR2,IMIN2)

               IF(MIN1.GT.MIN2)THEN
                  MIN2 = MINOFYR_FROM_DATE(366,23,59)
                  DO I = MIN1,MIN2
                     BAD_DAT(I) = -1
                  ENDDO 
                  MIN1 = 0
                  MIN2 = MINOFYR_FROM_DATE(JDAY2,IHR2,IMIN2)
                  DO I = MIN1,MIN2
                     BAD_DAT(I) = -1
                  ENDDO 
               ELSE
                  DO I = MIN1,MIN2
                     BAD_DAT(I) = -1
                  ENDDO 
               ENDIF
            ELSE
               CALL GET_TIME(TIME,JDAY,IHR,IMIN)
               WRITE(6,*)
               WRITE(6,*) 'Tips for record # ',ITDX+1,' exceeds 50'
               WRITE(6,10) JDAY,IHR,IMIN
 10            FORMAT('DATE/TIME: ',I3,1X,I2.2,':',I2.2)
               WRITE(6,*)
            ENDIF
         ELSEIF(DELTAT.GT.REVENT)THEN
            NINDX(INDX)   = ITDX + 1
            INDX = INDX + 1
            
            IF(NTIPS.LT.50)THEN
               IF(NTIPS.GT.20)THEN
                  DTIP        = FLOAT((REVENT)/NTIPS)
                  TIME_FACTOR = REVENT
               ELSEIF(NTIPS.GT.10.AND.NTIPS.LE.20)THEN
                  DTIP = FLOAT(REVENT/NTIPS)
                  TIME_FACTOR = REVENT
               ELSE
                  DTIP = FLOAT(REVENT/(NTIPS*2))
                  TIME_FACTOR = REVENT/2.
               ENDIF
               DO I = 1,NTIPS
                  ITDX = ITDX + 1
                  TIPTIME(ITDX) = TIME - TIME_FACTOR + DTIP*I
                  IF(TIPTIME(ITDX).GT.TIME) WRITE(6,*)'TIPTIME ',
     +                 'exceeds time of record, please check',
     +                 I,ITDX,TIPTIME(ITDX),TIME,DTIP,NTIPS
               ENDDO
               TIME0 = TIME - TIME_FACTOR + DTIP
               CALL GET_TIME(TIME0,JDAY1,IHR1,IMIN1)               
               CALL GET_TIME(TIME,JDAY2,IHR2,IMIN2)  
               MIN1 = MINOFYR_FROM_DATE(JDAY1,IHR1,IMIN1)
               MIN2 = MINOFYR_FROM_DATE(JDAY2,IHR2,IMIN2)

               IF(MIN1.GT.MIN2)THEN
                  MIN2 = MINOFYR_FROM_DATE(366,23,59)
                  DO I = MIN1,MIN2
                     BAD_DAT(I) = -1
                  ENDDO 
                  MIN1 = 0
                  MIN2 = MINOFYR_FROM_DATE(JDAY2,IHR2,IMIN2)
                  DO I = MIN1,MIN2
                     BAD_DAT(I) = -1
                  ENDDO 
               ELSE
                  DO I = MIN1,MIN2
                     BAD_DAT(I) = -1
                  ENDDO 
               ENDIF
               
               
            ELSE
               CALL GET_TIME(TIME,JDAY,IHR,IMIN)
               WRITE(6,*)
               WRITE(6,*) 'Tips for record # ',ITDX+1,' exceeds 50'
               WRITE(6,10) JDAY,IHR,IMIN
               WRITE(6,*)
               
            ENDIF
         ENDIF
      ELSE
         IF(NTIPS.LT.50)THEN

C*** if ntips less than 50 and 1st tip in file, spread out rainfall over 15
C*** minutes. 

            DTIP = FLOAT(REVENT/(2.0*NTIPS))
            ITDX = 1
            INDX = 1
            NINDX(INDX)   = ITDX
            INDX = INDX + 1
            DO I = 1,NTIPS
                TIPTIME(ITDX) = TIME - REVENT/2.0 + DTIP*I
               IF(TIPTIME(ITDX).GT.TIME) WRITE(6,*)'TIPTIME ',
     +          'EXCEEDS TIMEOFRECORD, PLEASE CHECK',
     +                ITDX,TIPTIME(ITDX),DELTAT
               ITDX = ITDX + 1
            ENDDO
            ITDX = ITDX - 1
            TIME0 = TIME - REVENT + DTIP
            CALL GET_TIME(TIME0,JDAY1,IHR1,IMIN1)               
            CALL GET_TIME(TIME,JDAY2,IHR2,IMIN2)    
            MIN1 = MINOFYR_FROM_DATE(JDAY1,IHR1,IMIN1)
            MIN2 = MINOFYR_FROM_DATE(JDAY2,IHR2,IMIN2)

            IF(MIN1.GT.MIN2)THEN
               MIN2 = MINOFYR_FROM_DATE(366,23,59)
               DO I = MIN1,MIN2
                  BAD_DAT(I) = -1
               ENDDO 
               MIN1 = 0
               MIN2 = MINOFYR_FROM_DATE(JDAY2,IHR2,IMIN2)
               DO I = MIN1,MIN2
                  BAD_DAT(I) = -1
               ENDDO 
            ELSE
               DO I = MIN1,MIN2
                  BAD_DAT(I) = -1
               ENDDO 
            ENDIF
         ELSE
            CALL GET_TIME(TIME,JDAY,IHR,IMIN)
            WRITE(6,*) 
            WRITE(6,*) 'TIPS FOR RECORD # ',ITDX+1,'EXCEEDS 50'
            WRITE(6,10) JDAY,IHR,IMIN
            WRITE(6,*) 
         ENDIF
      ENDIF
      RETURN
      END
                  

               




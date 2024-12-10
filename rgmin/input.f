C******************************************************************************
C*   THIS SUBROUTINE READS THE KSC DATA.  THIS DATA PROVIDES THE NUMBER OF TIPS
C*   PER MINUTE.  THE TIPS PER MINUTE ARE THEN DIVIDED INTO EQUAL TIME INTERVAL
C*   WITHIN THE MINUTE AND PLACE IN A TIME ORDERED TIME SERIES. SUBROUTINE HAS
C*   BEEN MODIFIED TO READ GEMS FORMAT (MARCH 1, 1996)
C******************************************************************************

      SUBROUTINE READ_TIPS_PER_MIN(TIPFILE)

      INCLUDE 'rgmin.par'
      CHARACTER*80 TIPFILE
      INTEGER*4    TMIN
      INTEGER*2    FLAG_YR



      ITDX = 0   !EVENT1->EVENT2 ETC. ITDX=INDEX COUNTER FOR TIME RECORDS
      INDX = 0   !INDEX COUNTER FOR ARRAY THAT FLAGS BEG AND END OF RAIN EVENT
      INDX=1 
      FLAG_YR = 0
      NR = 0

      DO I=1,TDIM
         TIPTIME(I)=0
         HALFTIP(I)=0
         NINDX(I)=0
      ENDDO
      N = 1

      WRITE(6,20) TIPFILE
 20   FORMAT('OPENING TIP FILE: ',A60)
      OPEN(UNIT=2,FILE=TIPFILE,STATUS='OLD')
 10   FORMAT(A80)

 5    READ(2,*,END=99)IYR,IMON,IDAY,IHR,IMIN,RR,RTOT
      NR = NR + 1
      CALL COM_LEAP(IYR,LEAP)
      IF(FLAG_YR.EQ.0)THEN
         BYEAR = IYR
         FLAG_YR = 0
      ENDIF

      IF(RR.GT.1.0)THEN
         WRITE(6,*) 'RAINFALL FOR MINUTE EXCEEDS 1500 mm/hr'
         WRITE(6,*) 'SKIPPING RECORD # ',NR,' PLEASE CHECK'
         WRITE(6,*)  IYR,IMON,IDAY,IHR,IMIN
         GOTO 5
      ELSEIF(RR.LE.O)THEN
         WRITE(6,*) 'NO RAINFALL IN RECORD'
         WRITE(6,*) 'SKIPPING RECORD # ',NR, ' PLEASE CHECK'
         WRITE(6,*)  IYR,IMON,IDAY,IHR,IMIN
         GOTO 5
      ENDIF
      
      N = N+1

      CALL JULDAY(IYR,IMON,IDAY,JDAY,LEAPYR)
      TMIN = JDAY*24*60 + IHR*60 + IMIN
      IF(TMIN.EQ.LTMIN)GOTO 5
      IDYSEC=JDAY*24*3600
      IHRSEC=IHR*3600
      IMINSEC=IMIN*60
      NTIPS=RR*100

      DTIP=60./NTIPS
      
      
      DO I=1,NTIPS
         ITDX=ITDX+1            !TOTAL TIP COUNTER
         IF(ITDX.GT.TDIM)STOP 'NUMBER OF TIPS EXCEEDS ARRAY DIMENSIONS'
         ISEC = DTIP*(I-.5)
         TIPTIME(ITDX)=IDYSEC+IHRSEC+IMINSEC+ISEC
         IF(ITDX.GT.1)THEN
            IF(TIPTIME(ITDX-1).GT.TIPTIME(ITDX))THEN
               ITT=TIPTIME(ITDX-1)
               JDAY=ITT/(24.*3600.)
               DELTAT=(365+LEAP)*3600*24+24*3600-TIPTIME(ITDX-1)
     +              +TIPTIME(ITDX)-3600*24

            ELSE
               DELTAT=TIPTIME(ITDX)-TIPTIME(ITDX-1)
            ENDIF

            IF(DELTAT.GE.THRESH1.AND.DELTAT.LE.THRESH2)THEN
               ITDX=ITDX + 1
               TIPTIME(ITDX)=TIPTIME(ITDX-1)
               TIPTIME(ITDX-1)=TIPTIME(ITDX)-DELTAT/2
               HALFTIP(ITDX-1)=1
               HALFTIP(ITDX)=1
               HTIP=HTIP + 1
            ELSEIF(DELTAT.GT.REVENT)THEN
               NINDX(INDX)=ITDX
               INDX=INDX + 1
            ENDIF
         ELSE
            NINDX(INDX) = ITDX
            INDX = INDX + 1
         ENDIF

      ENDDO
      LTMIN = TMIN
      GOTO 5

 99   CONTINUE
      NEVENT = INDX 
      WRITE(6,*) 'Total Number of Rain Events:',NEVENT
      WRITE(6,*) 'Total Number of Tips', itdx
      NINDX(INDX) = ITDX      
      WRITE(6,*) 'FINISHED READING DATA' 

      RETURN
      END

C*****************************************************************************
C*  THIS SUBROUTINE READS THE ST JOHNS DATA AND PLACES THE TIME OF TIP IN A
C*  CHRONOLOGICALLY ORDERED TIME SERIES IN UNITS OF SECONDS.  IT ALSO FLAGS
C*  MULTIPLE TIP EVENTS FOR WHICH THE TIME DIFFERENCE BETWEEN CONSECUTIVE TIPS
C*  EXCEEDS 3 MINUTES (180 SECONDS)
C*****************************************************************************

      SUBROUTINE READ_TIME_OF_TIP_MULT(TIPFILE)
      INCLUDE 'rgmin.par'

      CHARACTER*80 TIPFILE
      INTEGER*2    FLAG,FLAG_YR
c      COMMON /RTIP/ TIP


      ITDX = 0    !EVENT1->EVENT2 ETC. ITDX=INDEX COUNTER FOR TIME RECORDS
      INDX = 1    !INDEX COUNTER FOR ARRAY THAT FLAGS BEG AND END OF RAIN EVENT
      NTIPS=0
      FLAG=0
      FLAG_YR = 0


      DO I=1,10000
         TIPTIME(I)  = 0
         HALFTIP(I)  = 0
         NINDX(I)    = 0
      ENDDO

      WRITE(6,20) TIPFILE
 20   FORMAT('OPENING TIP FILE:',A80)
      OPEN(UNIT=2,FILE=TIPFILE,STATUS='OLD')

      WRITE(6,*) 'READING FIRST RECORD'
 5    READ(2,*,END=99)IYEAR,IMON,IDAY,IHR,IMIN,ISEC,RR
 
      CALL JULDAY(IYEAR,IMON,IDAY,JDAY,LEAPYR)
      CALL COM_LEAP(IYEAR,LEAP)
      IF(FLAG_YR.EQ.0)THEN
         BYEAR = IYEAR
         FLAG_YR = 1
      ENDIF
      

C**** ASSIGN A TIME IN SECONDS TO EACH TIP. IN CASE OF MULTIPLE TIPS MUST
C***  SPREAD TIPS OUT EVENLY THROUGHOUT TIME INTERVAL EXTENDING BACK TO PREVIOU
C*** TIP

      IDYSEC   = JDAY*24*3600
      IHRSEC   = IHR*3600
      IMINSEC  = IMIN*60
      MINCHECK = IDYSEC+IHRSEC+IMINSEC

      NTIPS = RR*100.

         
      IF(NTIPS.EQ.1)THEN
         ITDX = ITDX + 1
         IF(ITDX.GT.1)THEN
            TIPTIME(ITDX)=IDYSEC+IHRSEC+IMINSEC+ISEC
            TIME = TIPTIME(ITDX)
            CALL GET_TIME(TIME,JDAY1,IHR1,IMIN1) 
            DELTAT = TIPTIME(ITDX) - TIPTIME(ITDX-1)
            IF(DELTAT.LT.0)THEN
               DELTAT = ((((365+LEAP)*24*3600)+23*3600 + 
     +              59*60+59) - TIPTIME(ITDX-1)) + TIPTIME(ITDX)
            ENDIF
            IF(DELTAT.GE.THRESH1.AND.DELTAT.LE.THRESH2)THEN
               ITDX=ITDX + 1
               TIPTIME(ITDX)=TIPTIME(ITDX-1)
               TIPTIME(ITDX-1)=TIPTIME(ITDX)-DELTAT/2
               HALFTIP(ITDX-1)=1
               HALFTIP(ITDX)=1
               HTIP=HTIP + 1
            ELSEIF(DELTAT.GT.REVENT)THEN
               NINDX(INDX) = ITDX
               CALL GET_TIME(float(tiptime(itdx)),JDAY,IHR,IMIN)
               INDX=INDX + 1
            ELSEIF(DELTAT.LT.0)THEN
               WRITE(6,*) 'READ_TIME_OF_TIP_MULTI ERROR: ',
     +   'CURRENT TIME LESS THAN PREVIOUS'
               WRITE(6,*) 'CHECK RECORD #',ITDX
               WRITE(6,10) JDAY,IHR,IMIN
 10            FORMAT('DATE/TIME: ',I3,1X,I2,':',I2)
            ENDIF
         ELSE 
            ITDX = 1
            TIPTIME(ITDX) = IDYSEC+IHRSEC+IMINSEC+ISEC 
            NINDX(INDX)   = ITDX
            INDX=INDX + 1
         ENDIF
      ELSE         
         TIME = IDYSEC+IHRSEC+IMINSEC+ISEC        !CURRENT TIME IN SECONDS
         CALL GET_TIME(TIME,JDAY1,IHR1,IMIN1)               
         CALL QCMULTI(TIME,ITDX,INDX,NTIPS)
      ENDIF
      GOTO 5
 99   CONTINUE
      NEVENT = INDX 
      WRITE(6,*) 'Total Number of Rain Events:', NEVENT
      WRITE(6,*) 'Total Number of Rain Records:',ITDX
      NINDX(INDX) = ITDX
      RETURN
      END

C*****************************************************************************
C*  THIS SUBROUTINE READS THE ST JOHNS DATA AND PLACES THE TIME OF TIP IN A
C*  CHRONOLOGICALLY ORDERED TIME SERIES IN UNITS OF SECONDS.  IT ALSO FLAGS
C*  MULTIPLE TIP EVENTS FOR WHICH THE TIME DIFFERENCE BETWEEN CONSECUTIVE TIPS
C*  EXCEEDS 3 MINUTES (180 SECONDS)
C*****************************************************************************
      SUBROUTINE READ_HARRIS(TIPFILE)

      INCLUDE 'rgmin.par'
      CHARACTER*80 TIPFILE
      INTEGER*4    DAY,IYEAR,MON,IHR,IMIN,ISEC
      INTEGER*2    FLAG_YR

      ITDX = 0    !EVENT1->EVENT2 ETC. ITDX=INDEX COUNTER FOR TIME RECORDS
      INDX = 1    !INDEX COUNTER FOR ARRAY THAT FLAGS BEG AND END OF RAIN EVENT
      INDX=1 
      FLAG_YR = 0
      NTIPS=0

      DO I=1,10000
         TIPTIME(I)  = 0
         HALFTIP(I)  = 0
         NINDX(I)    = 0
      ENDDO

      WRITE(6,20) TIPFILE
 20   FORMAT('OPENING TIP FILE:',A80)
      OPEN(UNIT=2,FILE=TIPFILE,STATUS='OLD')

      NINDX(1) = INDX
      INDX = INDX + 1
      WRITE(6,*) 'READING FIRST RECORD'

 5    READ(2,*,END=99)IYEAR,JDAY,MON,DAY,IHR,IMIN,ISEC,RR
      NTIPS = NINT(RR*25)
      CALL COM_LEAP(IYEAR,LEAP)
      IF(NTIPS.GT.25)THEN
         write(6,*) 'suspect record: filtering data'
         write(6,*) IYEAR,JDAY,MON,DAY,IHR,IMIN,ISEC,RR
         GOTO 5
      ENDIF
      IF(FLAG_YR.EQ.0)THEN
         BYEAR = IYEAR
         FLAG_YR = 1
      ENDIF


C**** ASSIGN A TIME IN SECONDS TO EACH TIP. IN CASE OF MULTIPLE TIPS MUST
C***  SPREAD TIPS OUT EVENLY THROUGHOUT 1-MIN INTERVAL

      IDYSEC=JDAY*24*3600
      IHRSEC=IHR*3600
      IMINSEC=IMIN*60
      MINCHECK=IDYSEC+IHRSEC+IMINSEC
      IF(NTIPS.EQ.1)THEN
         ITDX=ITDX + 1
         TIPTIME(ITDX)=IDYSEC+IHRSEC+IMINSEC+ISEC
         IF(ITDX.GT.1)THEN
            IF(TIPTIME(ITDX-1).GT.TIPTIME(ITDX))THEN
               ITT=TIPTIME(ITDX-1)
               DAY=ITT/(24.*3600.)
               DELTAT = (365 + LEAP)*3600*24+24*3600-TIPTIME(ITDX-1)
     +              + TIPTIME(ITDX)-3600*24
            ELSE
               DELTAT=TIPTIME(ITDX)-TIPTIME(ITDX-1)
            ENDIF
            IF(DELTAT.GE.THRESH1.AND.DELTAT.LE.THRESH2)THEN
               ITDX=ITDX + 1
               TIPTIME(ITDX)=TIPTIME(ITDX-1)
               TIPTIME(ITDX-1)=TIPTIME(ITDX)-DELTAT/2
               HALFTIP(ITDX-1)=1
               HALFTIP(ITDX)=1
               HTIP=HTIP + 1
            ELSEIF(DELTAT.GT.REVENT)THEN
               NINDX(INDX) = ITDX
               INDX=INDX + 1
            ENDIF
         ELSE
            NINDX(INDX)=ITDX
            INDX=INDX + 1
         ENDIF

      ELSE
         TIME = IDYSEC+IHRSEC+IMINSEC+ISEC !CURRENT TIME IN SECONDS
         CALL GET_TIME(TIME,JDAY1,IHR1,IMIN1)               
         CALL QCMULTI(TIME,ITDX,INDX,NTIPS)
      ENDIF

      GOTO 5
 99   CONTINUE
      NEVENT = INDX 
      WRITE(6,*) 'Total Number of Rain Events:',NEVENT
      NINDX(INDX) = ITDX

      RETURN
      END



C******************************************************************************
C*   THIS SUBROUTINE READS THE DAR DATA.  THIS DATA PROVIDES THE NUMBER OF TIPS
C*   PER MINUTE.  THE TIPS PER MINUTE ARE THEN DIVIDED INTO EQUAL TIME INTERVAL
C*   WITHIN THE MINUTE AND PLACE IN A TIME ORDERED TIME SERIES. SUBROUTINE HAS
C*   BEEN MODIFIED TO READ GEMS FORMAT (MARCH 1, 1996)
C******************************************************************************

      SUBROUTINE READ_TIMEOFTIP(TIPFILE)
      INCLUDE 'rgmin.par'

      CHARACTER*80 TIPFILE
      INTEGER*4 DELTAT,TIME
      INTEGER*2 FLAG_YR

C
C * !EVENT1->EVENT2 ETC. ITDX=INDEX COUNTER FOR TIME RECORDS
C * !INDEX COUNTER FOR ARRAY THAT FLAGS BEG AND END OF RAIN EVENT
C
      ITDX = 0  
      INDX = 0    
      INDX=1 
      LAST_MON=0
      FLAG_YR = 0
      IY=1

      
      DO I=1,10000
         TIPTIME(I)=0
         HALFTIP(I)=0
         NINDX(I)=0
      ENDDO
      N = 0

      WRITE(6,20) TIPFILE
 20   FORMAT('OPENING TIP FILE:',A60)
      OPEN(UNIT=2,FILE=TIPFILE,STATUS='OLD')
c      READ(2,10) HEADER
c 10   FORMAT(A80)

 5    READ(2,*,END=99)IYR,JDAY,IHR,IMIN,ISEC,TIME,DELTAT
      IF(FLAG_YR.EQ.0)THEN
         BYEAR   = IYR
         FLAG_YR = 1
      ENDIF
      CALL COM_LEAP(IYR,LEAP)

      N = N+1

      CALL GET_TIME(TIME,JDAY,IHR,IMIN)
      
      ITDX                 = ITDX+1 
      TIPTIME(ITDX)        = TIME
      IF(ITDX.GT.1)THEN
         IF(DELTAT.GE.THRESH1.AND.DELTAT.LE.THRESH2)THEN
             ITDX            = ITDX + 1
             TIPTIME(ITDX)   = TIPTIME(ITDX-1)
             TIPTIME(ITDX-1) = TIPTIME(ITDX)-DELTAT/2
             HALFTIP(ITDX-1) = 1
             HALFTIP(ITDX)   = 1
             HTIP            = HTIP + 1
          ELSEIF(DELTAT.GT.REVENT)THEN
              NINDX(INDX)    = ITDX
              CALL GET_TIME(float(TIME),JDAY,IHR,IMIN)
c              print*,nindx(indx),itdx,jday,ihr,imin,deltat
              INDX           = INDX + 1
          ENDIF
       ELSE
          NINDX(INDX)        = 1
          INDX               = INDX + 1
       ENDIF

      GOTO 5

      
      
 99   CONTINUE
      NEVENT      = INDX
      NINDX(INDX) = ITDX
      WRITE(6,*) 'Total Number of Rain Events:',NEVENT
      WRITE(6,*) 'FINISHED READING DATA' 

      RETURN
      END

C******************************************************************************
C*   THIS SUBROUTINE READS THE SFL DATA.  THIS DATA PROVIDES THE NUMBER OF TIPS
C*   PER 5 MINUTE.  THE TIPS PER MINUTE ARE THEN DIVIDED INTO EQUAL TIME INTERVAL
C*   WITHIN THE MINUTE AND PLACE IN A TIME ORDERED TIME SERIES. SUBROUTINE HAS
C*   BEEN MODIFIED TO READ GEMS FORMAT (MARCH 1, 1996)
C******************************************************************************

      SUBROUTINE READ_TIPS_PER_5MIN(TIPFILE)

      INCLUDE 'rgmin.par'
      CHARACTER*80 TIPFILE
      INTEGER*2    FLAG_YR
      integer*4 nmin,nmin1,nmin2



      ITDX = 0   !EVENT1->EVENT2 ETC. ITDX=INDEX COUNTER FOR TIME RECORDS
      INDX = 0   !INDEX COUNTER FOR ARRAY THAT FLAGS BEG AND END OF RAIN EVENT
      INDX=1 
      FLAG_YR = 0
      NR = 0
      DO I=1,TDIM
         TIPTIME(I)=0
         HALFTIP(I)=0
         NINDX(I)=0
      ENDDO
      N = 1

      WRITE(6,20) TIPFILE
 20   FORMAT('OPENING TIP FILE: ',A60)
      OPEN(UNIT=2,FILE=TIPFILE,STATUS='OLD')
 10   FORMAT(A80)
 5    READ(2,*,END=99)IYR,IMON,IDAY,JDAY,IHR,IMIN,RR
      NR = NR + 1
      NMIN2 = JDAY*24*60 + IHR*60 + IMIN
      CALL COM_LEAP(IYEAR,LEAP)
      IF(NR.GT.1)THEN
         NMIN = NMIN2 - NMIN1
         IF(NMIN.GE.5)THEN
           NMIN = 5
         ELSE
           NMIN = NMIN
        ENDIF
      ELSE
         NMIN = 5
      ENDIF
      IF(RR.EQ.0)GOTO 5
      IF(FLAG_YR.EQ.0)THEN
         IF(IYR.LT.2000)THEN
            BYEAR = IYR - 1900
         ELSE
            BYEAR = IYR - 2000
         ENDIF
         FLAG_YR = 1
      ENDIF
      

      IF(RR.GT.5.0)THEN
         WRITE(6,*) 'RAINFALL FOR MINUTE EXCEEDS 1500 mm/hr'
         WRITE(6,*) 'SKIPPING RECORD # ',ITDX, ' PLEASE CHECK'
         WRITE(6,*)  IYR,IMON,IDAY,IHR,IMIN,RR
         GOTO 5
      ENDIF

      N = N+1

             

      IDYSEC=JDAY*24*3600
      IHRSEC=IHR*3600
      IMINSEC=IMIN*60 - NMIN*60
      NTIPS=RR*100.
      DTIP=NMIN*60./NTIPS
      NMIN1 = NMIN2

      DO I=1,NTIPS
         ITDX=ITDX+1            !TOTAL TIP COUNTER
         IF(ITDX.GT.TDIM)STOP 'NUMBER OF TIPS EXCEEDS ARRAY DIMENSIONS'
         ISEC = DTIP*(I-.5)
         TIPTIME(ITDX)=IDYSEC+IHRSEC+IMINSEC+ISEC
         IF(ITDX.GT.1)THEN
            IF(TIPTIME(ITDX-1).GT.TIPTIME(ITDX))THEN
               ITT=TIPTIME(ITDX-1)
               JDAY=ITT/(24.*3600.)
               DELTAT=(365+LEAP)*3600*24+24*3600-TIPTIME(ITDX-1)
     +              +TIPTIME(ITDX)-3600*24

            ELSE
               DELTAT=TIPTIME(ITDX)-TIPTIME(ITDX-1)
            ENDIF
c            write(6,*)jday,tiptime(itdx),deltat,isec,i,ntips,nmin,dtip
            IF(DELTAT.GE.THRESH1.AND.DELTAT.LE.THRESH2)THEN
               ITDX=ITDX + 1
               TIPTIME(ITDX)=TIPTIME(ITDX-1)
               TIPTIME(ITDX-1)=TIPTIME(ITDX)-DELTAT/2
               HALFTIP(ITDX-1)=1
               HALFTIP(ITDX)=1
               HTIP=HTIP + 1
            ELSEIF(DELTAT.GT.REVENT)THEN
               NINDX(INDX)=ITDX
               INDX=INDX + 1
            ENDIF
         ELSE
            NINDX(INDX) = ITDX
            INDX = INDX + 1
         ENDIF

      ENDDO
      GOTO 5

 99   CONTINUE
      NEVENT = INDX 
      WRITE(6,*) 'Total Number of Rain Events:',NEVENT
      WRITE(6,*) 'Total Number of Tips', itdx
      NINDX(INDX) = ITDX

      WRITE(6,*) 'FINISHED READING DATA' 

      RETURN
      END

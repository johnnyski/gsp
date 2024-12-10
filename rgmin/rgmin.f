      PROGRAM RGMIN
C *************************************************************
C * THIS PROGRAM PROCESSES THE GEMS TIP DATA AND COMPUTES     *
C * THE ONE MINUTE RAINFALL AVERAGES.  THE TIP DATA IS        *
C * CONVERTED TO A FUNCTIONAL FORM BY PASSING THE DATA TO A   *
C * SPLINE ROUTINE WHERE THE X ARRAY REPRESENTS THE TIME TIP  *
C * AND Y ARRAY THE RAIN RATE.                                *
C *************************************************************
C * PROGRAM WRITTEN BY BRAD FISHER                            *
C *************************************************************
C * PROGRAM WRITTEN ON MARCH 19 1994                          *
C *************************************************************


      INCLUDE   'rgmin.par'

      CHARACTER*80 TIPFILE,GMINFILE,IN_DIR,OUT_DIR,IFILE
      CHARACTER*80 LOC_FILE, RADAR_FILE
      CHARACTER*3  NETWORK_ID
      CHARACTER*4  GV_SITE,GTYPE
      CHARACTER*4  PRIMARY_RADAR
      CHARACTER*8  PRODUCT
      CHARACTER*12 G_NAME

      INTEGER*4    FLEN,DLEN,TIMERES
      INTEGER*4    G_NUMBER
      INTEGER*4    BAD_DAT(0:366*24*60),BAD
          
      REAL*4       RMODE
      REAL*4       GRES, GELEV,GRANGE,GAZ,GLON,GLAT
      REAL*4       RAIND(366),RAIND2(366),RAINH(0:23),RAINH2(0:23)
      REAL*8       XVAL,YVAL1,YVAL2
      REAL*8       X(3000),Y(3000),Y2(3000),YP1,YPN

      COMMON /BADQC/ BAD_DAT
C      COMMON /RTIP/ TIP
	  COMMON /HEADER/ PRODUCT,GV_SITE,PRIMARY_RADAR,G_NUMBER,
     +   G_NAME,GTYPE,GRES,GLAT,GLON,GRANGE,GAZ,GELEV

C
C *** OPEN THE INPUT DIRECTIVE FILE
C
      OPEN(UNIT=1,FILE='rgmin.i',STATUS='OLD')

      READ(1,*)      IVERBOSE
      READ(1,'(A3)') NETWORK_ID
C
C *** INPUT (GAG) AND OUTPUT (GMIN) DIRECTORIES
C
      READ(1,10) IN_DIR
      READ(1,10) OUT_DIR
      READ(1,10) LOC_FILE
      READ(1,10) RADAR_FILE
 10   FORMAT(A80)

      WRITE(6,11) IN_DIR
 11   FORMAT('INPUT DIRECTORY:  ',A60)
      WRITE(6,12) OUT_DIR
 12   FORMAT('OUTPUT DIRECTORY: ',A60)
      WRITE(6,13) LOC_FILE
 13   FORMAT('SITELIST FILE:    ',A60)
      WRITE(6,14) RADAR_FILE
 14   FORMAT('RADAR FILE:       ',A60)

C
C *** BEGIN READING THE INPUT (GAG) FILENAMES
C
      IFILE   = ''
 99   TIPFILE = ''

      READ(1,24,END=999) IFILE
 24   FORMAT(A80)

      CALL GET_SLEN(IFILE,IN_DIR,FLEN,DLEN)
      TIPFILE = IN_DIR
      TIPFILE(DLEN:DLEN+FLEN) = IFILE
      CALL NAMEFILE(IFILE,GMINFILE,OUT_DIR)
      DO I = 0,366*24*60
         BAD_DAT(I) = 1
      ENDDO
      DO I = 1, 366
         RAIND(I)  = 0
         RAIND2(I) = 0
         DO J = 0,23
            RAINH(J)  = 0
            RAINH2(J) = 0
         ENDDO
      ENDDO
      LAST_DAY=0


C ********************************************************************
C * Read the data.                                                   *
C * 
C * RD_MIN: Tips per minute                                          *
C *    e.g., KSC, OMKI, PHUK, OKWJ (Old Kwajalein                    *
C * RD_TIMEOFTIP: Time of tip                                        *
C *    e.g., Darwin, New Kwaj (RMI, KWAJ)                            *
C * RD_TIMEOFTIP_MULT: Time of tip with additional QC for mult. tips *
C *    e.g., STJ, TAMU                                               *
C * RD_OTHER: If Net_ID is not supported, returns with error message *
C ********************************************************************
C * The following Network_ID's are supported:
C *
C * Florida
C     * KSC - Kennedy Space Center
C     * STJ - St. John's WMD
C * Kwajalein
C     * KWA - New Kwajalein
C     * RMI - Marshall Islands
C * Texas
C     * TAM - Texas A&M Mesonet
C * Darwin
C     * CSC - C-scale network
C     * DSC - D-scale network
C     * MSC - M-scale network
C ********************************************************************
C
C *** PROCESSING KSC TIPPING BUCKET DATA
C
      IF(NETWORK_ID .EQ. 'KSC') THEN 

         RMODE = 0.254
         TIP1  = -3.0
         TIP2  = 0.508
         N1MIN = 5
         TIMERES = 60
         PRIMARY_RADAR = 'KMLB'
         RADAR_LAT=   28.10
         RADAR_LON = -80.65
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         CALL READ_TIME_OF_TIP_MULT(TIPFILE) 

      ELSEIF(NETWORK_ID .EQ. 'KSO') THEN

         RMODE = 0.254
         TIP1  = -3.0
         TIP2  = 0.508
         N1MIN = 5
         TIMERES = 60
         PRIMARY_RADAR = 'KMLB'
         RADAR_LAT=   28.10
         RADAR_LON = -80.65
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         CALL READ_TIPS_PER_MIN(TIPFILE)

C
C *****PROCESSING ST JOHN'S TIPPING BUCKET DATA
C
      ELSE IF(NETWORK_ID .EQ. 'STJ') THEN 

         RMODE = 0.254
         TIP1  = -3.0
         TIP2  = 0.508
         N1MIN = 5
         TIMERES = 60
         PRIMARY_RADAR = 'KMLB'
         RADAR_LAT =  28.10
         RADAR_LON = -80.65
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         CALL READ_TIME_OF_TIP_MULT(TIPFILE)
C
C*****PROCESSING DARWIN TIPPING BUCKET DATA
C
      ELSE IF( (NETWORK_ID.EQ.'CSC') .OR.
     +         (NETWORK_ID.EQ.'DSC') .OR.
     +         (NETWORK_ID.EQ.'MSC') ) THEN
         
         RMODE = 0.2
         TIP1 = -2.4
         TIP2  = 0.4
         N1MIN = 5
         TIMERES = 60
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         PRIMARY_RADAR = 'DARW'
         RADAR_LAT = -12.45722
         RADAR_LON = 130.92528
         CALL READ_TIMEOFTIP(TIPFILE)

      ELSE IF(NETWORK_ID .EQ. 'OMK') THEN

         RMODE = 0.254
         TIP1  = -3.0
         TIP2  = 0.508
         N1MIN = 5
         TIMERES = 60
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         PRIMARY_RADAR = 'OMKI'
         RAD_LAT = 17 + 47/60. + 51.1/3600.
         RAD_LON = 98 + 25/60. + 56.0/3600.
         CALL READ_TIPS_PER_MIN(TIPFILE)

      ELSE IF(NETWORK_ID .EQ. 'KWA') THEN

         RMODE = 0.254
         TIP1  = -3.0
         TIP2  = 0.508
         N1MIN = 5
         TIMERES = 60
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         PRIMARY_RADAR = 'KWAJ'
         RAD_LAT = 17 + 47/60. + 51.1/3600.
         RAD_LON = 98 + 25/60. + 56.0/3600.
         CALL READ_TIMEOFTIP(TIPFILE)

      ELSE IF(NETWORK_ID .EQ. 'RMI') THEN

         RMODE = 0.254
         TIP1  = -3.0
         TIP2  = 0.508
         N1MIN = 5
         TIMERES = 60
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         PRIMARY_RADAR = 'KWAJ'
         RAD_LAT = 17 + 47/60. + 51.1/3600.
         RAD_LON = 98 + 25/60. + 56.0/3600.
         CALL READ_TIMEOFTIP(TIPFILE)

      ELSE IF(NETWORK_ID .EQ. 'TFB'.OR.NETWORK_ID .EQ. 'TFT' ) THEN

         RMODE = 0.254
         TIP1  = -3.0
         TIP2  = 0.508
         N1MIN = 5
         TIMERES = 60
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         PRIMARY_RADAR = 'KMLB'
         RADAR_LAT =  28.10
         RADAR_LON = -80.65
         CALL READ_TIMEOFTIP(TIPFILE)

      ELSE IF(NETWORK_ID .EQ. 'TAM') THEN

         RMODE = 0.254
         TIP1  = -3.0
         N1MIN = 5
         TIMERES = 60
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         PRIMARY_RADAR = 'KHGX'
         RADAR_LAT =  29.472778
         RADAR_LON = -95.082222
         CALL READ_TIME_OF_TIP_MULT(TIPFILE)

      ELSE IF(NETWORK_ID .EQ. 'HAR') THEN

         RMODE = 1.0
         TIP1  = -6.0
         TIP2  = 2.032
         N1MIN = 10
         TIMERES = 60
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         PRIMARY_RADAR = 'KHGX'
         RADAR_LAT =  29.472778
         RADAR_LON = -95.082222
         CALL READ_HARRIS(TIPFILE)
      ELSE IF(NETWORK_ID.EQ.'SFL')THEN
         RMODE = 0.254
         TIP1  = -3.0
         TIP2  = 0.508
         N1MIN = 5
         TIMERES = 60
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         PRIMARY_RADAR = 'KHGX'
         RADAR_LAT =  28.10
         RADAR_LON = -80.65
         CALL READ_TIPS_PER_5MIN(TIPFILE)
      ELSE IF(NETWORK_ID .EQ. 'LBA') THEN

         RMODE = 0.254
         TIP1  = -3.0
         TIP2  = 0.508
         N1MIN = 5
         TIMERES = 60
         WRITE(6,*)'PROCESSING: ',NETWORK_ID,' RES= ',RMODE
         PRIMARY_RADAR = 'KMLB'
         RADAR_LAT = -10.77389
         RADAR_LON = -62.33694
         CALL READ_TIMEOFTIP(TIPFILE)

      ELSE
         WRITE(6,*) 'RGMIN ERROR:FORMAT NOT SUPPORTED', NETWORK_ID
         STOP 'FINISHED!'
      ENDIF
 26   CONTINUE
      WRITE(6,*)
      
      PRODUCT = 'GMIN'
      G_NUMBER = GET_GAUGE_NUMBER(IFILE)
      IF(NETWORK_ID.EQ.'KSO')THEN
         CALL GET_SITE_INFO('KSC',LOC_FILE, RADAR_FILE,IFLAG)
      ELSEIF(NETWORK_ID.EQ.'KWO')THEN
         CALL GET_SITE_INFO('KWA',LOC_FILE, RADAR_FILE,IFLAG)
      ELSE
        CALL GET_SITE_INFO(NETWORK_ID,LOC_FILE, RADAR_FILE,IFLAG)
      ENDIF
C
C ****OPEN OUTPUT GMINFILE 
C
      IF(IFLAG.EQ.0)GOTO 99
      OPEN(UNIT=4,FILE=GMINFILE,STATUS='UNKNOWN')
c      WRITE(6,23) PRODUCT,GV_SITE,NETWORK_ID,G_NUMBER,G_NAME,
c     +     GTYPE,GRES,GLAT,GLON,PRIMARY_RADAR,GRANGE,GAZ,GELEV
      WRITE(4,23) PRODUCT,GV_SITE,NETWORK_ID,G_NUMBER,G_NAME,
     +     GTYPE,GRES,GLAT,GLON,PRIMARY_RADAR,GRANGE,GAZ,GELEV
 23   FORMAT(A5,1X,A4,1X,A3,1X,I4.4,1X,A10,1X,A3,1X,F3.1,1X,F9.5,
     +        1X,F10.5,1X,A4,1X,F4.0,1X,F4.0,1X,F6.1)
      
C**** INITIALIZE SOME COUNTER VARIABLES

      NN   = 1          !COUNTER FOR LOCATING BEG & END INDEX FOR RAIN EVENTS
      N    = 0          !COUNTER RELATING TO NUMBER OF RECORDS IN A RAIN EVENT
      NT   = 0          !COUNTER FOR MAINTAINING INDEX IN ORDERED TIME ARRAY 
      NCNT =1
      RAIN_TOT=0
      RAINI_TOT=0      
      RAINB_TOT=0
      DO I=1,2000
         X(I)  = 0
         Y(I)  = 0
         Y2(I) = 0
      ENDDO

      N = NINDX(2) - NINDX(1) - 1 
      IYEAR = BYEAR
      print*,nindx(1),nindx(2)
      DO 50 WHILE(N.LT.2)
         INDX =  NINDX(NN)
         CALL SPECIAL_TIPS(4,TIPTIME,INDX,IYEAR,TIP1,TIP2,N,
     +        RMODE, RAIND2,RAINH2,RAIND,RAINH,I1TIP,I2TIP)
         NN = NN + 1
         N = NINDX(NN+1)-NINDX(NN)-1
 50   CONTINUE
 
C***  LOADING UP X,Y VECTORS FOR SPLINE.  
C***  THESE VECTORS REPRESENT ONE RAIN EVENT.
C***  NOTE THAT N-1 REPRESENTS THE TOTAL NUMBER OF RECORDS IN AN EVENT.

      IFIRST=0
      IY=1
      DO 100 WHILE(NN.LE.NEVENT)
         LEAP = 0
         IF(MOD(IYEAR,4).EQ.0)LEAP = 1
         LFLAG = 0
         CALL LOADXY(N,NN,NT,NTT,RMODE,RAIN,X,Y,RAIND,RAINH,LEAP,LFLAG)
         IF(LFLAG.EQ.1) GOTO 35
         RAIN_TOT = RAIN_TOT + RAIN
         IF(NTT.GT.1)THEN
            DO I=N+2,N+NPAR
               X(I) = 2*X(I-1)-X(I-2)
               Y(I) = Y(I-1) 
            ENDDO
            NX = N + NPAR -1 
         ELSE
            NX = N
         ENDIF
C     
C***  CALL SUBROUTINE TO COMPUTE FIRST DERIVATIVES AT ENDPOINTS
C 
    
         CALL SLOPE(X,Y,N+1,YP1,YPN,IX0,IXN)

C     
C***  CALL SUBROUTINE TO COMPUTE SECOND DERVIVATES AT EACH POINT IN INTERVAL. 
C***  THIS ROUTINE IS NEEDED TO COMPUTE CUBIC SPLINE (NUMERICAL RECIPIES)
C

         CALL SPLINE(X,Y,NX,YP1,YPN,Y2)
         TIME1     = TIPTIME(NTT)
         TIMEN     = TIPTIME(NTT+N)
         CALL GET_TIME(TIME1,JDAY,IHR,IMIN)
         CALL GET_TIME(TIMEN,JDAYN,IHRN,IMINN)
         IF(IVERBOSE.EQ.1)THEN
            WRITE(6,27) JDAY,IHR,IMIN,IYEAR
            WRITE(6,28) JDAYN,IHRN,IMINN,IYEAR
 27         FORMAT(' BDATE: ',I3,3(1X,I2))
 28         FORMAT(' EDATE: ',I3,3(1X,I2))
         ENDIF

         IF(LAST_DAY.GT.JDAY)THEN
            IYEAR=IYEAR +1
            IY=IY+1
            LAST_DAY=JDAY
         ENDIF

C**** THE SUBROUTINE CAL_EVENT COMPUTES CUMULATIVE INTERPOLATED RAINFALL
C***  AND COMPUTES A BIAS FACTOR BASED ON THE CUMULATIVE NUMBER OF TIPS

         IX1    = X(1)
         IF(NTT.GT.1)THEN
            IXN    = X(N+1)
         ELSE
            IXN    = X(N)
         ENDIF
         ITDELTAT  = IXN-IX1
         ITDELT60  = ITDELTAT/60.
         ITDELT60  = ITDELT60*60
         IXBVAL    = X(1)         
         CALL CAL_EVENT(X,Y,Y2,NX,IXBVAL,ITDELT60,RAIN,BIAS,IVERBOSE)
         DO XVAL=IXBVAL,IXBVAL+ITDELT60,TIMERES
            CALL SPLINT(X,Y,Y2,NX,XVAL,YVAL1)
            CALL SPLINT(X,Y,Y2,NX,XVAL+60,YVAL2)

            IF(YVAL1.LT.0)YVAL1=0
            IF(YVAL2.LT.0)YVAL2=0

            RRMIN = BIAS*(YVAL2-YVAL1)*60

            IF(RRMIN.GT.RTHRESHL.AND.RRMIN.LT.RTHRESHU)THEN
               RAIND2(JDAY) = RAIND2(JDAY) + RRMIN/60.
               RAINH2(IHR)  = RAINH2(IHR)  +   RRMIN/60.
               IF(RRMIN.GT.RRMAX)THEN
                  RRMAXB = RRMIN
                  RRMAX = RRMIN/BIAS
               ENDIF
               MINOFYR = MINOFYR_FROM_DATE(JDAY,IHR,IMIN)
               BAD = BAD_DAT(MINOFYR)
               CALL WRITE_DATA(4,IYEAR,JDAY,IHR,IMIN,ISEC,
     +                          RRMIN*BAD)
 40            FORMAT(I2,1X,I3,3(1X,I2.2),F8.2)
 41            FORMAT(I2,1X,F11.7,F8.2,1X,I3)
            ENDIF

C*** INCREMENT MINUTE BY ONE AND CHECK TO SEE WHETHER IHR,JDAY,IYEAR NEED TO
C*** BE INCREMENTED ALSO (SUBROUTINE CH_TIME)

            CALL CH_TIME(IMIN,IHR,JDAY,IYEAR, LEAP)
         ENDDO
         DO I=1,N + NPAR +1
            X(I)=0
            Y(I)=0
         ENDDO
         LAST_DAY=JDAY
 35      NN = NN + 1
         N = NINDX(NN+1) - NINDX(NN) - 1

         IF(N.LE.1)THEN
            INDX =  NINDX(NN)
            CALL SPECIAL_TIPS(4,TIPTIME,INDX,IYEAR,TIP1,TIP2,N,
     +      RMODE, RAIND2,RAINH2,RAIND,RAINH,I1TIP,I2TIP,LAST_DAY)

C            LAST_DAY = JDAY
C            IF(NINDX(NN).EQ.0.OR.NN.GT.NEVENT)GOTO 100
            IF(NN.GT.NEVENT)GOTO 100
            GOTO 35
         ENDIF  
 100  CONTINUE

C*****/This section of code writes out the daily and hourly accumulations for
C      both the raw and gmin data.  It was used for debugging purposes but may 
C      be modified to produce daily and hourly accumulation files.  For the
C     time being the code has been commented out and is considered inactive.
C******/
      IF(IVERBOSE.EQ.1)THEN
         WRITE(6,*) ' DAILY TOTALS'
         WRITE(6,*) '-------------'
         WRITE(6,*) 'Jday GAG GMIN'
         TIME = TIPTIME(1)
         CALL GET_TIME(TIME,JBDAY,IHR,IMIN)
         TIME = TIPTIME(NINDX(NN-1))
         CALL GET_TIME(TIME,JEDAY,IHR,IMIN)
         IF(JBDAY.GT.JEDAY)THEN   
            DO I=JBDAY,366
               TOTAL_GAG = TOTAL_GAG  + RAIND(I)
               TOTAL_GMIN= TOTAL_GMIN + RAIND2(I)
               WRITE(6,201) I,RAIND(I),RAIND2(I)
            ENDDO
            
            DO I=1,JEDAY
               TOTAL_GAG = TOTAL_GAG + RAIND(I)
               TOTAL_GMIN= TOTAL_GMIN+ RAIND2(I)
               WRITE(6,201) I,RAIND(I),RAIND2(I)
            ENDDO
            
         ELSE
            
            DO I=JBDAY,JEDAY
               TOTAL_GAG = TOTAL_GAG + RAIND(I)
               TOTAL_GMIN= TOTAL_GMIN+ RAIND2(I)
               WRITE(6,201) I,RAIND(I),RAIND2(I)
 201           FORMAT(1x,I3,1X,F6.1,1X,F6.1)
            ENDDO
         ENDIF
         WRITE(6,*) "--------------------"
         WRITE(6,'(" TOT ",2(1x,F6.1))') TOTAL_GAG,TOTAL_GMIN
         WRITE(6,*)
         WRITE(6,*) ' HOURLY TOTALS'
         WRITE(6,*) "--------------------"
         WRITE(6,*) 'Hour GAG GMIN'
         DO I=0,23
            TOTAL_HR_GAG =  TOTAL_HR_GAG   + RAINH(I)
            TOTAL_HR_GMIN = TOTAL_HR_GMIN +  RAINH2(I)
            WRITE(6,201) I+1,RAINH(I),RAINH2(I)
 202        FORMAT(I2,1X,F6.1,1X,F6.1)
         ENDDO
         WRITE(6,*) "--------------------"
         WRITE(6,'(" TOT",2(1x,F6.1))')TOTAL_HR_GAG,TOTAL_HR_GMIN
         WRITE(6,*)
         TBIAS =  TOTAL_GAG/TOTAL_GMIN
 198     FORMAT(A4,1X,I5,4(1X,F7.2),1X,2(1X,F8.2)2(1X,F4.2))
         WRITE(6,'("Total Bias: ",F5.3)') TBIAS
         WRITE(6,'("Number of single tips: ",I3," Rainfall Amount: ",
     +        F5.1, " mm")' ) I1TIP,I1TIP*RMODE
         WRITE(6,'("Number of double tips: ",I3," Rainfall Amount: ",
     +        F5.1," mm")') I2TIP,I2TIP*RMODE*2
         WRITE(6,*)
      ENDIF
      TOTAL_GAG = 0
      TOTAL_GMIN = 0
      TOTAL_HR_GAG = 0
      TOTAL_HR_GMIN = 0
      TBIAS = 0
      RAINMISS = 0
      I1TIP = 0
      I2TIP = 0
      GOTO 99

 999  CONTINUE

      STOP 'FINISHED PROCESSING DATA'
      END
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C *         END OF MAIN
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************
C ***********************************************************************

      SUBROUTINE NAMEFILE(IFILE,OFILE,OUT_DIR)
C ***********************************************************************
C * THIS SUBROUTINE NAMES THE OUTPUT FILE DYNAMICALLY                   *
C ***********************************************************************
      CHARACTER*80 IFILE,OFILE,OUT_DIR

      CHARACTER*1 C

      INTEGER*4 DIRLEN,POS,BPOS
c      write(6,'("IFILE: ",a80)')ifile
      OFILE = ''
      C = ''
      IFIRST = 0
      DIRLEN = 0
      DO I=1,60
         C=OUT_DIR(I:I)
         IF(C.EQ.''.AND.IFIRST.EQ.0)THEN
            DIRLEN=I-1
            IFIRST=1
         ENDIF
      ENDDO
      IFIRST = 0
      BPOS = 1
      POS=INDEX(IFILE,'.')
      DO I=POS,1,-1
         C = OUT_DIR(I:I)
         IF(C.EQ.'/'.AND.IFIRST.EQ.0)THEN
            BPOS = I
            IFIRST = 1
         ENDIF
      ENDDO
      
      IF(POS.EQ.0) STOP 'NO INPUT FILENAME GIVEN'
      OFILE(1:DIRLEN)=OUT_DIR(1:DIRLEN)
      OFILE(DIRLEN+1:DIRLEN+1+POS) = IFILE(1:POS)
      OFILE(DIRLEN +1 +POS:DIRLEN+1+POS+4) = 'gmin'


      RETURN
      END
C
C ***********************************************************************
C * This program ???                                                    *
C ***********************************************************************
C
      SUBROUTINE CAL_EVENT(X,Y,Y2,NX,IXBVAL,ITDELT60,RAIN,BIAS,IVERBOSE)
                          
      PARAMETER (RTHRESH=.10/60.)

      REAL*8   X(NX),Y(NX),Y2(NX),XVAL,YVAL1,YVAL2

      RAIN_INT=0
      DO XVAL=IXBVAL,IXBVAL+ITDELT60,60

         CALL SPLINT(X,Y,Y2,NX,XVAL,YVAL1)
         CALL SPLINT(X,Y,Y2,NX,XVAL+60.,YVAL2)

         IF(YVAL1.LT.0)YVAL1=0
         IF(YVAL2.LT.0)YVAL2=0
         RRMIN = (YVAL2-YVAL1)
         IF(RRMIN.GT.RTHRESH) THEN
            RAIN_INT=RAIN_INT+ RRMIN
         ENDIF
      ENDDO
      
      IF(RAIN_INT.GT.0.AND.BIAS.LT.100)THEN
         BIAS=RAIN/RAIN_INT
      ELSE
         BIAS = 0
         WRITE(6,*)
         WRITE(6,*)'****CAL_EVENT ERROR:BIAS UNDEFINED, CHECK DATA****'
      ENDIF
      IF(IVERBOSE.EQ.1)THEN
         WRITE(6,10)RAIN,RAIN_INT
         WRITE(6,11)BIAS
         WRITE(6,*)
      ENDIF
 10   FORMAT(' Rain:',2(1X,F5.1))
 11   FORMAT(' Bias: ',1x,F5.2)

      RETURN
      END
C
C ***********************************************************************
C * This program ???                                                    *
C ***********************************************************************
C

      SUBROUTINE GET_SLEN(IFILE,IN_DIR,FLEN,DLEN)
      CHARACTER*80 IFILE,IN_DIR
      CHARACTER*1  C1, C2
      INTEGER*4    FLEN,DLEN
      
      IFIRST1 = 0
      IFIRST2 = 0
      DO I = 1,80
         C1 = IFILE(I:I)
         C2 = IN_DIR(I:I)
         IF(C1.EQ.''.AND.IFIRST1.EQ.0)THEN
            FLEN    = I
            IFIRST1 = 1
         ENDIF
         IF(C2.EQ.''.AND.IFIRST2.EQ.0)THEN
            DLEN    = I
            IFIRST2 = 1
         ENDIF
      ENDDO

      RETURN
      END
C
C ***********************************************************************
C * This program loads the x and y vectors which are used by the spline
C * routines to perform an interpolation of a given event.
C ***********************************************************************
C

      SUBROUTINE LOADXY(N,NN,NT,NTT,RMODE,RAIN,X,Y,RAIND,RAINH,LEAP,
     +    LFLAG)

      INCLUDE 'rgmin.par'

      REAL*4    RAIND(366),RAINH(0:23)
      REAL*8    X(N+1),Y(N+1)


      RAIN=0 
C
C**** SET INDEX CONTROL PARAMETERS
C
         IF(NT.GT.0.OR.NN.GT.1)THEN
            NTT=NINDX(NN)
            NT =NINDX(NN)
            IF(NTT.EQ.0.OR.NT.EQ.0)THEN
               WRITE(6,*)'LOADXY ERROR:INDEX OUT OF RANGE',NTT,NT
               LFLAG = 1
               RETURN
            ENDIF
         ELSE
            NTT=1
            NT=NTT
         ENDIF

C*****BEGINNING OF LOOP

      DO I=1,N+1 
         
         X(1)=0
         
         
         IT1=TIPTIME(NTT)       !TIME OF FIRST TIP IN AN EVENT
         IT2=TIPTIME(NT)        !TIME OF CURRENT TIP IN AN EVENT

C
C***  COMPUTE THE DIFFERENCE BETWEEN TWO CONSECUTIVE TIMES WITHIN AND 
C***  INDIVIDUAL RAIN EVENT
C
         
         IF(I.GT.1)DT=TIPTIME(NT)-TIPTIME(NT-1)


C ***************************************************************
C * THIS CONDITION CHECKS TO SEE IF YEAR CHANGES. SINCE WE ARE 
C * INTERESTED IN THE RELATIVE TIMES WE NEED BE CAREFUL WHEN 
C * COMPUTING TIME DIFFERENC FOR THIS SPECIAL CASE. NOTE THAT 1 
C * DAY IN SECONDS IS SUBTRACTED TO GET THE ACTUAL DIFFERENCE, 
C * WHEREAS IN GENERAL THIS CORRECTION IS NOT NECESSARY.
C ***************************************************************

         IF(IT2.LT.IT1)THEN
C
C*** COMPUTE TIME TO END OFYEAR
C
            SEC2END=(365+LEAP)*24*3600+24*3600-IT1 
C
C***  ADD SECONDS TO END OF YEAR TO FIRST TIME RECORD IN NEW YEAR
C
            X(I)=SEC2END+IT2-3600*24 
         ELSE                   
            X(I)=IT2-IT1   
         ENDIF
C         
C *** COMPUTE CUMULATIVE RAINFALL FOR A SINGLE RAIN EVENT. 
C *** CHECK FOR HALF TIPS.
C
 
         CALL GET_TIME(FLOAT(IT2),JDAY,IHR,IMIN)
c         write(6,*) x(i),jday,ihr,imin
         ISEC = IT2 -(JDAY*24*3600 + IHR*3600 + IMIN*60)
         IHT=HALFTIP(NT)

         IF(IHT.EQ.1)THEN
            RAIN=RAIN +RMODE/2.
            Y(I)=RAIN
            RAIND(JDAY) = RAIND(JDAY)  + RMODE/2.
            RAINH(IHR)  = RAINH(IHR)   + RMODE/2.
            IH=IH+1
         ELSE
            RAIN=RAIN+RMODE
            Y(I)=RAIN
            RAIND(JDAY) = RAIND(JDAY)  + RMODE
            RAINH(IHR)  = RAINH(IHR)   + RMODE
         ENDIF


         IF(X(I).LE.0.AND.I.GT.1)WRITE(6,*) 
     +       'LOADXY ERROR: PROBLEM WITH TIME:', IT1,IT2,JDAY,
     +        IHR,IMIN,ISEC
         NT=NT+1
      ENDDO
      RETURN
      END


C***************************************************************************
C* THIS SUBROUTINE COMPUTES RANGE AND AZIMUTH FROM THE PRINCIPLE RADAR AND
C* PLACES HEADER INFO INTO COMMON BLOCK.
C**************************************************************************
      SUBROUTINE GET_SITE_INFO(NETWORK_ID, LOC_FILE, RADAR_FILE,IFLAG)
      
      PARAMETER (PI = 3.1415927,KM_PER_DEG = 111.2)
*
C*** VARIABLES DESCRIBING HEADER FIELDS
*
	  CHARACTER*3  NETWORK_ID,GNET,GNETWORK
	  CHARACTER*4  PRIMARY_RADAR
      CHARACTER*4  GV_SITE,GTYPE
      CHARACTER*8  PRODUCT
	  CHARACTER*12 G_NAME
      CHARACTER*80 LOC_FILE, RADAR_FILE

      INTEGER*4    GLOND,GLONM,GLATD,GLATM
      REAL*4       GLONS,GLATS
	  REAL*4       GRES, GELEV,GRANGE,GAZ,GLAT,GLON

	  INTEGER*4    G_NUMBER,GN

	  COMMON /HEADER/ PRODUCT,GV_SITE,PRIMARY_RADAR,G_NUMBER,
     +   G_NAME,GTYPE,GRES,GLAT,GLON,GRANGE,GAZ,GELEV

      GELEV = -99.9
      IF(NETWORK_ID.EQ.'SFL')THEN
         GRES = 5.0
      ELSE
         GRES = 1.0
      ENDIF
      GTYPE = 'TIP'
      GNET = ''
      
      IFLAG = 0
      IF(NETWORK_ID.EQ.'KSO')THEN
         GNETWORK= 'KSC'
      ELSE
         GNETWORK = NETWORK_ID
      ENDIF

      write(6,*) "Reading sitelist: ",LOC_FILE
      OPEN(UNIT=8, FILE=LOC_FILE, STATUS='OLD')
 1    READ(8,10,END = 98)GN,G_NAME,GLOND,GLONM,GLONS,GLATD,GLATM,GLATS
      WRITE(6,13)        GN,G_NAME,GLOND,GLONM,GLONS,GLATD,GLATM,GLATS
 10   FORMAT(I4,1X,A10,13X,I4,1X,I2,1X,F4.1,1X,I3,1X,I2,1X,F4.1)
 13   FORMAT(I4,1X,A10,13X,I4,1X,I2,1X,F4.1,1X,I3,1X,I2,1X,F4.1)
      IF(GN.NE.G_NUMBER)THEN
         GOTO 1
      ELSE
         IFLAG = 1
         IF(GLOND.LT.0)THEN
            GLON = GLOND - GLONM/60. - GLONS/3600.
         ELSE
            GLON = GLOND + GLONM/60. + GLONS/3600.
         ENDIF
         IF(GLATD.LT.0)THEN
            GLAT = GLATD - GLATM/60. - GLATS/3600.
         ELSE
            GLAT = GLATD + GLATM/60. + GLATS/3600.
         ENDIF

      ENDIF
 98   CONTINUE
      CLOSE(8)
      IF(IFLAG.EQ.0)print*, 'ERROR GET_SITE_INFO: Did not match input gauge 
     +with sitelist'   
       
      IFLAG = 0
      OPEN(UNIT=9, FILE=RADAR_FILE, STATUS ='OLD')

      
      READ(9,*) 
      READ(9,*)
 5    READ(9,11,END = 99) GV_SITE,GNET,PRIMARY_RADAR,RLAT,RLON

 11   FORMAT(A4,1X,A3,1X,A4,1X,F10.6,1X,F11.6)
      IF(GNET.NE.GNETWORK) THEN
         GOTO 5
      ELSE
         IFLAG     = 1
         DLON      = GLON - RLON
         DLAT      = GLAT - RLAT
         RLAT_RAD  = (PI/180.) * DLAT
         RLON_RAD  = (PI/180.) * DLON
         GLAT_RAD  = GLAT * (PI/180.)
         RLAT_DIST = DLAT * KM_PER_DEG
         AVG_LAT   = .5 * (RLAT_RAD + GLAT_RAD)
         RLON_DIST = COS(AVG_LAT) * DLON * KM_PER_DEG
         AZ        = ATAN2(RLON_DIST,RLAT_DIST)
         IAZ       = NINT(AZ * (180./PI))
         IF(IAZ.LT.0) IAZ = 360 + IAZ
         GAZ = IAZ 


C     Compute Range
         
         RANGE = RLON_DIST**2 + RLAT_DIST**2
         RANGE = SQRT(RANGE)
C         IGRANGE = NINT(RANGE)
         GRANGE = RANGE

 12      FORMAT(A3,1X,I4.4,1X,A10,1X,A3,1X,F3.1,1X,F9.5,1X,F10.5,
     +        1X,A4,1X,F4.0,1X,F4.0,1X,F6.1)
      ENDIF
 99   CONTINUE
      CLOSE(9)
      IF(IFLAG.EQ.0)print*, 'ERROR GET_SITE_INFO: Did not match network 
     +with principle radar'

      RETURN
      END
      
      FUNCTION GET_GAUGE_NUMBER(FILE)
      CHARACTER*80 FILE
      CHARACTER*7 REMAINDER
      CHARACTER*3  NET
      INTEGER*4    GNUM

      READ(FILE,10)NET,GNUM,REMAINDER
 10   FORMAT(A3,I4,A7)
      GET_GAUGE_NUMBER = GNUM
      RETURN
      END
         
      SUBROUTINE WRITE_DATA(IUNIT,IYEAR,JDAY,IHR,IMIN,ISEC,RATE)

      WRITE(IUNIT,40) IYEAR,JDAY,IHR,IMIN,ISEC,RATE
 40   FORMAT(I2,1X,I3,3(1X,I2.2),F8.2)

      RETURN 
      END

      SUBROUTINE SPECIAL_TIPS(IUNIT,TIPTIME,INDX,IYEAR,TIP1,
     + TIP2,N,RMODE,RAIND2,RAINH2,RAIND,RAINH,I1TIP,I2TIP,LAST_DAY)
      PARAMETER (TDIM = 15000)
      REAL*4     RAIND2(366),RAINH2(0:23),RAIND(366),RAINH(0:23)
      INTEGER*4  TIPTIME(TDIM)

      N1MIN = 5
      TIME1 = TIPTIME(INDX)
      call get_time(TIME1,JDAY,IHR,IMIN)
      IF(INDX.GT.1)THEN
         TIME0 = TIPTIME(INDX - 1)
         CALL CHECKYR(TIME0,TIME1,IYEAR,LAST_DAY)
      ENDIF
      CALL GET_TIME(TIME1,JDAY,IHR,IMIN)

      IF(N.EQ.0)THEN
         I1TIP=I1TIP+1
         IMIN = IMIN - N1MIN
         RAIND(JDAY) = RAIND(JDAY) + RMODE
         RAINH(IHR)  = RAINH(IHR)  + RMODE
         
         
C***  CH_TIME2 PERFORMS SIMILAR FUNCTION AS CH_TIME.  DIFFERENCE IS THAT
C***  CH_TIME CHECKS TO SEE WHETHER INCREMENTATION OF MINUTE EXCEEDS 59
C***  WHEREAS CH_TIME2 CHECK TO SEE WHETHER IMIN < 0.
         
         CALL CH_TIME2(IMIN,IHR,JDAY,IYEAR)
         DO I = -1*N1MIN+1,0
            CALL WRITE_DATA(IUNIT,IYEAR,JDAY,IHR,IMIN,ISEC,TIP1)
C            WRITE(4,40) IYEAR,JDAY,IHR,IMIN,ISEC,TIP1
            RAIND2(JDAY) = RAIND2(JDAY) + -1*TIP1/60.
            RAINH2(IHR)  = RAINH2(IHR)  + -1*TIP1/60.
            CALL CH_TIME(IMIN,IHR,JDAY,IYEAR)
         ENDDO
         
      ELSE IF(N.EQ.1)THEN
         I2TIP=I2TIP+1
         TIME2=TIPTIME(INDX+1)
         RAIND(JDAY) = RAIND(JDAY) + RMODE*2
         RAINH(IHR)  = RAINH(IHR)  + RMODE*2
         DT = (TIME2-TIME1)
C*** 30 seconds = reduction factor to account for est. time for first bucket 
c*** to fill up.

         RATE = TIP2*3600./(DT+30.) 
         CALL GET_TIME(TIME1,JDAY,IHR,IMIN)
         NMIN = DT/60.
         DO I = 1,NMIN+1
            IF(RATE.GT.0.AND.RATE.LT.900)THEN
               CALL WRITE_DATA(IUNIT,IYEAR,JDAY,IHR,IMIN,ISEC,-1*RATE)
C               WRITE(4,40)IYEAR,JDAY,IHR,IMIN,ISEC,-1*RATE
               RAIND2(JDAY) = RAIND2(JDAY) + RATE/60.
               RAINH2(IHR) = RAINH2(IHR) + RATE/60.
            ENDIF                     
            CALL CH_TIME(IMIN,IHR,JDAY,IYEAR)
         ENDDO
      ENDIF 
      RETURN
      END

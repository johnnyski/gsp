      PROGRAM PREPROC_GAUGE
C ******************************************************************
C * THIS PROGRAM READS RAW RAIN GAUGE DATA FROM GV FIELD SITES AND 
C * FORMATS DATA TO BE COMPATIBLE WITH RGMIN.F.  FILENAMES ARE NAMED U
C * SING CONVENTION ESTABLISHED FOR RGMIN INPUT DATA.  MAIN PROGRAM 
C * CONSISTS OF CALLS TO SITE MODULES WHICH DO THE ACTUAL PROCESSING 
C * AND WRITING OF DATA. THE PROGRAM REQUIRES FOUR COMMAND LINE ARGUMENTS.
C *        1) Top level Directory path 
C *        2) NETWORK ID - 3 CHARACTER CODE ASSOCIATING GAUGE W/NETWORK
C *        3) BYEAR - beginning year of data
C *        4  VERBOSE - Verbose Flag
C*****************************************************************************

      CHARACTER*60 FILELIST
      CHARACTER*3  NETWORK_ID
      CHARACTER*2  BYEAR
      CHARACTER*1  VERBOSE
C
C *** Check to see that the number of command line arguments is correct.
C
      NARG = IARGC()
      IF(NARG.NE.4) THEN 
         WRITE(6,*)'Usage:'
         WRITE(6,*)'preproc DIRECTORY SITEID YEAR VERBOSE'
         WRITE(6,*)'DIRECTORY: above the data/ directory'
         WRITE(6,*)'SITEID is a 3 character site identifier'
         WRITE(6,*)'YEAR beginning year of data'
         WRITE(6,*)'VERBOSE is the verbose flag (1 = verbose)'
         STOP
      ENDIF
C
C *** Get command line arguments
C
      CALL GETARG(1,FILELIST)
      CALL GETARG(2,NETWORK_ID)
      CALL GETARG(3,BYEAR)
      CALL GETARG(4,VERBOSE)
      READ(VERBOSE,10)IVERBOSE
 10   FORMAT(I1)

         WRITE(6,'("FILELIST: ",A60)') FILELIST
         WRITE(6,'("NETWORK_ID: ",A6)') NETWORK_ID
         WRITE(6,'("BYEAR: ",A2)') BYEAR
         WRITE(6,'("VERBOSE: ",A1)') VERBOSE
C
C *** Call preprocessing module based on NETWORK_ID given at command line
C
      IF(NETWORK_ID.EQ.'STJ') THEN
         WRITE(6,*) 'PROCESSING DATA FROM St. Johns WMD'
         CALL PREPROC_STJ(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'RMI'.OR.NETWORK_ID.EQ.'KWA') THEN
         WRITE(6,*) 'PROCESSING DATA FROM the Marshall Islands'
         CALL PREPROC_KWA(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'CSC'.OR.NETWORK_ID.EQ.'DSC'.OR.
     +        NETWORK_ID.EQ.'MSC') THEN
         WRITE(6,*) 'PROCESSING DATA FROM the Darwin, Australia'
         CALL PREPROC_DAR(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'TAM') THEN
         WRITE(6,*) 'PROCESSING DATA FROM Texas A&M'
         CALL PREPROC_TAM(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'KSC') THEN
         WRITE(6,*) 'PROCESSING DATA FROM KSC'
         CALL PREPROC_KSC(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'HAR') THEN
         WRITE(6,*) 'PROCESSING DATA FROM HAR'
         CALL PREPROC_HAR(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'OMK') THEN
         WRITE(6,*) 'PROCESSING DATA FROM Om Koi, Thailand'
         CALL PREPROC_GEMS(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'KSO') THEN
         NETWORK_ID = 'KSC'
         WRITE(6,*) 'PROCESSING DATA FROM KSC (OLD FORMAT)'
         CALL PREPROC_GEMS(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'KWO')THEN
         WRITE(6,*) 'PROCESSING DATA FROM Kwajalein (OLD FORMAT)'
         CALL PREPROC_GEMS(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'SFL')THEN
         WRITE(6,*) 'PROCESSING DATA FROM South Florida WMD'
         CALL PREPROC_SFL(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'TFB')THEN
         WRITE(6,*) 'PROCESSING DATA FROM TFB'
         CALL PREPROC_KWA(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'LBA')THEN
         WRITE(6,*) 'PROCESSING DATA FROM Ji-Parana, Brazil'
         CALL PREPROC_KWA(NETWORK_ID, FILELIST,BYEAR,IVERBOSE)
      ELSEIF(NETWORK_ID.EQ.'TFT')THEN
         WRITE(6,*) 'PROCESSING DATA FROM TFT TOKAY'
         CALL PREPROC_TFT(NETWORK_ID,FILELIST,BYEAR,IVERBOSE)
      ENDIF
      END

C ********************************************************************
C *  GET_TIP BACKS OUT THE TIME OF EACH TIP AND RAINFALL AMOUNT BY   *
C * SUBTRACTING THE CURRENT RECORD FROM THE PREVIOUS RECORD.  THIS   *
C * PROCEDURE ALSO RESOLVES FOR MULTIPLE TIPS WHICH ARE ASSIGNED A   *
C * SINGLE TIME STAMP. CURRENTLY USED IN: HAR, STJ.                  *
C ********************************************************************
      SUBROUTINE GET_TIP(NR,RAIN,TIP)

      PARAMETER    (ADIM = 5000)
      REAL*4      RAIN(ADIM),TIP(NR)
      
      TIP(1) = 0
      DO I = 2,NR
         TIP(I) = RAIN(I) - RAIN(I-1)
         RAIN(I-1) = 0
      ENDDO
      RETURN
      END

C *******************************************************************
C *  This subroutine combines the NETWORK_ID, G_NUMBER, and CYR     *
C *  fields from which output filename is constructed.              *
C *******************************************************************
      SUBROUTINE NAMEFILE(NETWORK_ID,IFILE,G_NUMBER,CYR,OFILE)
      CHARACTER*60 IFILE,OFILE
      CHARACTER*4  G_NUMBER
      CHARACTER*3 NETWORK_ID
      CHARACTER*2 CYR
     
      OFILE = ''
C
C ***  Search directory path for position of "raw".  Path convention allows
C ***  output filename and directory path of output to be dynamically named.
C
      POS = INDEX(IFILE,'raw')


      OFILE = IFILE(1:POS-1)
      OFILE(POS:POS+3) = 'gag/'
      OFILE(POS+4:POS+6) = NETWORK_ID
      OFILE(POS+7:POS+7) = '/'
      OFILE(POS+8:POS+10) = NETWORK_ID
      OFILE(POS+11:POS+14) = G_NUMBER
      OFILE(POS+15:POS+15) = '_'
      OFILE(POS+16:POS+17) = CYR
      OFILE(POS+18:POS+21) = '.gag'
      
      RETURN
      END



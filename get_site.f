      SUBROUTINE GET_SITE(NETWORK_ID,IFILE,G_NUMBER,IFIND)
C *********************************************************************
C * This subroutine extracts the sitenumber from the filename and     *
C * compares this with sitenumbers from sitelist.  Once a match is    *
C * found it returns a 4 character sitenumber back to read module     *
C * of specified netwoGNrk so that output file can be named.  Currently *
C * this subroutine is called for RMI, TAM, and KSC.                  *
C *********************************************************************

      CHARACTER*60 IFILE,SITEDIR
      CHARACTER*2 GSITE_DAR
      CHARACTER*3 NETWORK_ID
      CHARACTER*4 G_NUMBER,GNUM, GSITE_SFL, GNAME_SFL
      CHARACTER*4 GSITE_TAM, GSITE_HAR, GSITE_RMI, GSITE_KSC
      CHARACTER*4 GNAME_TAM, GNAME_HAR, GNAME_RMI, GNAME_KSC
      INTEGER*4   NGSITE,IGNUM,POS
C
C *** Construct directory path and sitelist filename based
C *** based on network ID and established directory organization.
C

	  write(6,*) "In get_site..."
	
      POS = INDEX(IFILE,"raw")
      SITEDIR = IFILE(1:POS)
      SITEDIR(POS:POS + 8) = 'sitelist/'
      SITEDIR(POS+9:POS+11) = NETWORK_ID
      SITEDIR(POS+12:POS+19) = '_loc.dat'
      IFIND = 0      ! Flag indicating whether sitenumber was located.
C
C **** Locate correct gauge number from sitelist by keying on network_id
C **** Naming conventions differ from site to site requiring specialized
C **** treatment at this level of preprocessing
C
      IF(NETWORK_ID.EQ.'RMI') THEN
C
C *** Republic of the Marshall Islands
C
         IF(IFILE(POS+9:POS+9).NE.'/')THEN
            GSITE_RMI = IFILE(POS+9:POS+12)
         ELSE
            GSITE_RMI = IFILE(POS+10:POS+13)
         ENDIF
         OPEN(UNIT = 11,FILE = SITEDIR, STATUS = 'OLD')
 5       READ(11,11,END = 99) IGNUM, GNAME_RMI
 11      FORMAT(A4,1X,A4)
         IF(GNAME_RMI.EQ.GSITE_RMI)THEN
            READ(G_NUMBER,13) IGNUM
            IFIND = 1
            CLOSE(11)
            RETURN
         ELSE
            GOTO 5
         ENDIF

      ELSEIF(NETWORK_ID.EQ.'TAM') THEN
C
C *** Texas A&M Mesonet
C
         IF(IFILE(POS+9:POS+9).NE.'/')THEN
            GSITE_TAM = IFILE(POS+12:POS+15)
         ELSE
            GSITE_TAM = IFILE(POS+13:POS+16)
         ENDIF
        write(6,1111) IFILE, gsite_tam
 1111   format(" ",a60,": ",a4)
		write(6,*) "Reading NGSITE into GSITE..."
         READ(GSITE_TAM,13) NGSITE
         OPEN(UNIT = 11,FILE = SITEDIR, STATUS = 'OLD')
 6       READ(11,12,END=99) IGNUM, GNAME_TAM
 12      FORMAT(I4,1X,A4)
 13      FORMAT(I4)
         IF(IGNUM.EQ.NGSITE)THEN
            WRITE(G_NUMBER,16) IGNUM
            IFIND = 1
            CLOSE(11)
            RETURN
         ELSE
            GOTO 6
         ENDIF

       ELSEIF(NETWORK_ID.EQ.'KSC')THEN
C
C *** NASA/KSC, Florida
C
          GSITE_KSC = IFILE(POS+12:pos+15)
          READ(GSITE_KSC,14) NGSITE
 14       FORMAT(I4)
          OPEN(UNIT = 11,FILE = SITEDIR, STATUS = 'OLD')
 7        READ(11,15,END=99) IGNUM
 15       FORMAT(I4)
          IF(IGNUM.EQ.NGSITE) THEN
             WRITE(G_NUMBER,16) IGNUM
             IFIND = 1
             CLOSE(11)
             RETURN
 16          FORMAT(I4.4)
          ELSE
             GOTO 7
          ENDIF

       ELSEIF((NETWORK_ID.EQ.'CSC').OR.(NETWORK_ID.EQ.'DSC').OR.
     +         (NETWORK_ID.EQ.'MSC'))THEN
C
C *** Darwin, Australia
C
          GSITE_DAR = IFILE(POS+11:pos+12)
          READ(GSITE_DAR,14) NGSITE
          OPEN(UNIT = 11,FILE = SITEDIR, STATUS = 'OLD')
 8        READ(11,15,END = 99) IGNUM
          IF(IGNUM.EQ.NGSITE) THEN
             WRITE(G_NUMBER,16) IGNUM
             IFIND = 1
             CLOSE(11)
             RETURN
          ELSE
             GOTO 8
          ENDIF

       ELSEIF(NETWORK_ID.EQ.'HAR')THEN
C
C *** Harris County, Texas
C
          IF(IFILE(POS+8:POS+8).NE.'/') THEN
             GSITE_HAR = IFILE(POS+8:POS+11)
          ELSE
             GSITE_HAR = IFILE(POS+9:POS+12)
          ENDIF
          READ(GSITE_HAR,15) NGSITE
 
          OPEN(UNIT=11,FILE=SITEDIR,STATUS='OLD')
 9        READ(11,15,END=99) IGNUM
          IF(IGNUM.EQ.NGSITE) THEN
             WRITE(G_NUMBER,16) IGNUM
             IFIND = 1
             CLOSE(11)
             RETURN
          ELSE
             GOTO 9
          ENDIF
       ELSEIF(NETWORK_ID.EQ.'SFL')THEN
          IF(IFILE(POS+8:POS+8).NE.'/') THEN
             GSITE_SFL = IFILE(POS+11:POS+14)
          ELSE
             GSITE_SFL = IFILE(POS+12:POS+15)
          ENDIF

c          write(6,'(a4)') ifile(pos+11:14)
          OPEN(UNIT=11,FILE=SITEDIR,STATUS='OLD')
 17       READ(11,11,END=99) IGNUM,GNAME_SFL
c          write(6,'("gname_sfle:",a4,"gsite:",a4)') gname_sfl,gsite_sfl
          IF(GNAME_SFL.EQ.GSITE_SFL)THEN
 	        READ(G_NUMBER,13) IGNUM
             IFIND = 1
             CLOSE(11)
             RETURN
           ELSE
             GOTO 17
          ENDIF
       ENDIF

 99   CONTINUE
C
C *** Report to user if a gauge number match is not found.
C
      IF(IFIND.EQ.0)THEN
         WRITE(6,*)'COULD NOT IDENTIFY GAUGE NUMBER: '
         WRITE(6,'("PLEASE CHECK GAUGENAME AND SITELIST: ",A4)')
     +    GSITE_SFL
         CLOSE(11)
      ENDIF
      RETURN
      END

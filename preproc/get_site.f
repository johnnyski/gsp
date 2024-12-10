      SUBROUTINE GET_SITE(NETWORK_ID,IFILE,G_NUMBER,IFIND)
C *********************************************************************
C * This subroutine extracts the sitenumber from the filename and     *
C * compares this with sitenumbers from sitelist.  Once a match is    *
C * found it returns a 4 character sitenumber back to read module     *
C * of specified netwoGNrk so that output file can be named.  Currently *
C * this subroutine is called for RMI, TAM, and KSC.                  *
C *********************************************************************

      CHARACTER*60 IFILE,SITEDIR
      CHARACTER*2 GSITE_DAR,GSITE_KSC
      CHARACTER*3 NETWORK_ID
      CHARACTER*4 G_NUMBER,GSITE_SFL,GNUMB
      CHARACTER*4 GSITE_TAM, GSITE_HAR, GSITE_RMI

      CHARACTER*4 GNAME_TAM, GNAME_RMI
      CHARACTER*5 GSITE_LBA,GNAME_LBA
      CHARACTER*4 SITE,GNAME_TFT,GSITE_TFT
      CHARACTER*5 GNAME_TFB,GSITE_TFB
      INTEGER*4   NGSITE,IGNUM,POS
C
*** Construct directory path and sitelist filename based
C *** based on network ID and established directory organization.
C

	  write(6,*) "In get_site..."
	
      POS = INDEX(IFILE,"raw")
      SITEDIR = IFILE(1:POS)
      SITEDIR(POS:POS + 8) = 'sitelist/'
      SITEDIR(POS+9:POS+11) = NETWORK_ID
      SITEDIR(POS+12:POS+19) = '_loc.dat'
       
c      write(6,'("POS=",i2)') POS 
c       write(6,'("SiteDir=",a60)') SITEDIR

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
          SITE = GSITE_RMI(1:4)
         OPEN(UNIT = 11,FILE = SITEDIR, STATUS = 'OLD')
 5       READ(11,11,END = 99) GNUMB, GNAME_RMI
 11      FORMAT(A4,1X,A4)
         IF(GNAME_RMI.EQ.GSITE_RMI)THEN
            G_NUMBER = GNUMB
            IFIND = 1
            CLOSE(11)
            RETURN
         ELSE
            GOTO 5
         ENDIF

      ELSEIF(NETWORK_ID.EQ.'TAM'.OR.NETWORK_ID.EQ.'KWA') THEN
C
C *** Texas A&M Mesonet
C
         IF(IFILE(POS+9:POS+9).NE.'/')THEN
            GSITE_TAM = IFILE(POS+12:POS+15)
         ELSE
            GSITE_TAM = IFILE(POS+13:POS+16)
         ENDIF
         SITE = GSITE_TAM(1:4)
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
          GSITE_KSC = IFILE(POS+12:pos+13)
          SITE = GSITE_SFL(1:2)
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
C
       ELSEIF((NETWORK_ID.EQ.'CSC').OR.(NETWORK_ID.EQ.'DSC').OR.
     +         (NETWORK_ID.EQ.'MSC'))THEN
C
C *** Darwin, Australia
C
          GSITE_DAR = IFILE(POS+11:pos+12)
          READ(GSITE_DAR,14) NGSITE
          SITE = GSITE_DAR(1:2)
	  write(6, '("GSITE_DAR=",A4) ')  GSITE_DAR
	  write(6, '("  NGSITE=",i6) ')   NGSITE
          OPEN(UNIT = 11,FILE = SITEDIR, STATUS = 'OLD')
 8        READ(11,15,END = 99) IGNUM
          IF(IGNUM.EQ.NGSITE) THEN
             WRITE(G_NUMBER,16) IGNUM
	  write(6, *)' G_NUMBERE= ', G_NUMBER 
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
          SITE = GSITE_HAR(1:4)
 
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
          SITE = GSITE_SFL(1:4)
	  G_NUMBER=GSITE_SFL 
             IFIND = 1
c          write(6,'(a4)') ifile(pos+11:pos+14)
c          OPEN(UNIT=11,FILE=SITEDIR,STATUS='OLD')
c 17       READ(11,11,END=99) GNUMB,GNAME_SFL
C          write(6,'("gname_sfle:",a4,"gsite:",a4)') gname_sfl,gsite_sfl
c          IF(GNAME_SFL.EQ.GSITE_SFL)THEN
c 	         G_NUMBER = GNUMB(1:4)
c             IFIND = 1
c             CLOSE(11)
c             RETURN
c           ELSE
c             GOTO 17
c         ENDIF
        ELSEIF(NETWORK_ID.EQ.'TFB')THEN
          GSITE_TFB(1:1) = '0'
          IF(IFILE(POS+8:POS+8).NE.'/') THEN
             GSITE_TFB(2:4) = IFILE(POS+10:POS+12)
          ELSE
             GSITE_TFB(2:4) = IFILE(POS+11:POS+13)
          ENDIF
          IF(GSITE_TFB(1:4).EQ.'0108'.OR.GSITE_TFB(1:4).EQ.'0101')THEN
             GSITE_TFB(5:5) = IFILE(POS+14:POS+14)
          ELSE
             GSITE_TFB(5:5) = ' '
          ENDIF
          SITE = GSITE_TFB(1:4)
          OPEN(UNIT=11,FILE=SITEDIR,STATUS='OLD')
 27       READ(11,28,END=99) GNUMB,GNAME_TFB
 28       FORMAT(A4,5X,A5)
C          write(6,'(a5,1x,a5,1x,a1)') gname_tfb,gsite_tfb,
C     + ifile(pos+14:pos+14)
          IF(GNAME_TFB.EQ.GSITE_TFB)THEN
 	         G_NUMBER = GNUMB(1:4)
             IFIND = 1
             CLOSE(11)
             RETURN
           ELSE
             GOTO 27
          ENDIF
        ELSEIF(NETWORK_ID.EQ.'LBA')THEN
         IF(IFILE(POS+9:POS+9).NE.'/')THEN
            GSITE_LBA = IFILE(POS+9:POS+14)
         ELSE
            GSITE_LBA = IFILE(POS+10:POS+15)
         ENDIF
          SITE = GSITE_LBA(3:6)
c          write(6,'(a4,a6)') site, gsite_lba
         OPEN(UNIT = 11,FILE = SITEDIR, STATUS = 'OLD')
 55      READ(11,56,END = 99) GNUMB, GNAME_LBA
 56      FORMAT(A4,A6)
         IF(GNAME_LBA.EQ.GSITE_LBA)THEN
            G_NUMBER = GNUMB
            IFIND = 1
            CLOSE(11)
            RETURN
         ELSE
            GOTO 55
         ENDIF
       ELSEIF(NETWORK_ID.EQ.'TFT')THEN
             IF(IFILE(POS+8:POS+8).NE.'/') THEN
             GSITE_TFT(1:4) = IFILE(POS+11:POS+14)
          ELSE
             GSITE_TFT(1:4) = IFILE(POS+12:POS+15)
          ENDIF
          SITE = GSITE_TFT(1:4)
          OPEN(UNIT=11,FILE=SITEDIR,STATUS='OLD')
 37       READ(11,29,END=99) GNUMB,GNAME_TFT
 29       FORMAT(A4,5X,A4)
c           write(6,'(a4,1x,a4,1x,a1)') gname_tft,gsite_tft,
c     + ifile(pos+14:pos+14)
          IF(GNAME_TFT.EQ.GSITE_TFT)THEN
 	         G_NUMBER = GNUMB(1:4)
             IFIND = 1
             CLOSE(11)
             RETURN
           ELSE
             GOTO 37
          ENDIF
       ENDIF
 99   CONTINUE
C
C *** Report to user if a gauge number match is not found.
C
      IF(IFIND.EQ.0)THEN
         WRITE(6,31)SITE
 31      FORMAT('COULD NOT IDENTIFY GAUGE NUMBER: ',a4)
         WRITE(6,'("PLEASE CHECK GAUGENAME AND SITELIST: ")')

         CLOSE(11)
       ENDIF
      RETURN
      END


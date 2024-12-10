      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)

      PARAMETER (NMAX=3000)

      REAL*8 YP1,YPN,X(N),Y(N),Y2(N)
      REAL*8 P,QN,SIG,UN,U(N)

      INTEGER N
      INTEGER I,K

      IF (YP1.GT..99E30) THEN
         Y2(1)=0.
         U(1)=0.
      ELSE
         Y2(1)=-0.5
         U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF

      DO 11 I=2,N-1
         SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
         P=SIG*Y2(I-1)+2.
         Y2(I)=(SIG-1.)/P
         U(I)=(6.*((Y(I+1)-Y(I))/(X(I+
     *        1)-X(I))-(Y(I)-Y(I-1))/(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*
     *        U(I-1))/P
 11   CONTINUE
      IF (YPN.GT..99E30) THEN
         QN=0.
         UN=0.
      ELSE
         QN=0.5
         UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
         Y2(K)=Y2(K)*Y2(K+1)+U(K)
 12   CONTINUE
      RETURN
      END

      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)

      REAL*8 X,Y,XA(N),Y2A(N),YA(N)
      REAL*8 A,B,H

      INTEGER N
      INTEGER K,KHI,KLO

      KLO=1
      KHI=N

 1    IF (KHI-KLO.GT.1) THEN
         K=(KHI+KLO)/2
         IF(XA(K).GT.X)THEN
            KHI=K
         ELSE
            KLO=K
         ENDIF
         GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) PAUSE 'BAD XA INPUT IN SPLINT'
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**
     +     2)/6.

      RETURN
      END

      SUBROUTINE SLOPE(X,Y,N,YP1,YPN,IX0,IXN)
C*****************************************************************************
C     *   THIS ROUTINE COMPUTES THE FIRST DERIVATIVE OF CUMULATIVE RAINFALL FOR
C     *   A SINGLE RAIN EVENT VS. TIME BEGINNING AT TIME X(1) AND X(N). DURING THE
C     *   FIRST THREE MINUTES THE TIPS ARE ACCUMULATED.  THE TIME DIFFERENCE FOR
C     *   CONSECUTIVE TIPS IS COMPUTED. 
C******************************************************************************
      PARAMETER (ALPHA=-1/60.)

      REAL*8 X(N),Y(N),YP1,YPN

      INTEGER*4 DT

C***  INITIALIZE SOME VARIABLES
      
      IDT1=0
      RR1=0
      RATE1=0
      
      IDTN=0
      RRN=0
      RATEN=0
      NTIP1 = 0
      NTIPN = 0
      J=1
      
      ITIP1=X(1)                !TIME OF FIRST TIP IN RAIN EVENT
      ITIP2=X(N)                !TIME OF LAST TIP IN RAIN EVENT

C***  COMPUTE RAIN AMOUNTS FOR TIPS OCCURRING AT BEGINNING OF RAIN EVENT
C***  DURING A TIME INTERVAL LES THEN OR EQUAL TO 180 SECONDS (3 MINUTES).

      DO I=1,N-1
         DT=X(I+1)-X(I)
         IF(IDT1+DT.LE.180)THEN
            IDT1=IDT1+DT
            RR1=RR1+Y(J)+.254/2
            NTIP1=NTIP1+1   
         ELSE                   
            GOTO 30
         ENDIF
         J=J+1
      ENDDO
 30   CONTINUE 

      DT=0

      DO I=N,2,-1

         DT=X(I)-X(I-1)
         IF(IDTN+DT.LE.180)THEN
            IDTN=IDTN+DT
            RRN=RRN+.254
            NTIPN=NTIPN+1
         ELSE
            GOTO 50
         ENDIF

      ENDDO
 50   CONTINUE

C**   IF MORE THAN ONE TIP RECORDED DURING BEGINNING OR END OF EVENT THEN
C**   ADD A HALF TIP SINCE TWO TIPS REQUIRED TO PASS CONDITION
      IF(IDT1.GT.0) THEN
         RATE1=(RR1+.254/2)/IDT1
      ELSE
         RATE1=.20/150
      ENDIF
      IF(IDTN.GT.0)THEN
         RATEN=(RRN+.254/2)/IDTN
      ELSE
         RATEN=.20/150
      ENDIF

      IF(N.GT.3)THEN
         YP1=RATE1
         YPN=RATEN
      ELSE
         YP1 = RATE1*2
         YPN = RATEN*2
      ENDIF
c      write(6,*)'derivatives at endpoints', yp1,ypn
      

      R1AVG=Y(1)
      RNAVG=.254


      Y1INTER=R1AVG
      YNINTER=RNAVG
      
      X1INTER=(Y1INTER)/RATE1
      X0=X1INTER-X(1)
      IX0=X0/60
      IF(IX0.LE.0)IX0=1
      RNAVG=RRN/NTIPN
      XNINTER=-YNINTER/RATEN
      XN=0-XNINTER
      IXN=XN/60
      IF(IXN.LE.0)IXN=1

      R0=RATE1
      RN=RATEN

      
      DO I=IX0,1,-1
         R=R0*EXP(ALPHA*I*60.)*3600
      ENDDO

      RETURN
      END
      

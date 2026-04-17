      SUBROUTINE GraDatBDFlu0(UPreSuf)

      INTEGER UPreSuf
      
      WRITE(UPreSuf, '(A)') '"lineFieldNames"'
      WRITE(UPreSuf, '(4A)')                                            &
     &     '"Flujo [MW]","[%] FlujoMax","Flujo [GWh]",',                &
     &     '"Perdidas [MW]","Perdidas [GWh]",',                         &
     &     '"Perdidas(2) [MW]","Perdidas(2) [GWh]",',                   &
     &     '"IT [US$/h]","IT [kUS$]"'
      WRITE(UPreSuf, '(A)') '"lineFormat"'
      WRITE(UPreSuf, '(2A)') '"%10.2f","%6.2f","%10.4f",',              &
     &     '"%10.2f","%10.4f","%10.2f","%10.4f","%10.2f","%10.4f"'
      WRITE(UPreSuf, '(A)') '"linePrefixNames"'      
      WRITE(UPreSuf, '(A)')                                             &
     &  '"Linea","Simulacion","Bloque","Numero Linea"'
      END

!************************************************
!     Subrutina Graba Archivo Flujos Total Lineas
!************************************************
      SUBROUTINE GraDatBDFlu2(NBloques, BloDur,                         &
     &     FPerdTram, FPerdLin, NLinea, LinNBar,                        &
     &     LinNFlu, LinPTra, LinPer, LinPer2, LinTMax, CMg,             &
     &     Dim, ULog)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

!     variables globales
!***********************
      CHARACTER*1 FPerdLin
      INTEGER IREC
      INTEGER LRECL
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBloques
      INTEGER NLinea
      LOGICAL FPerdTram
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION CMg(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION LinPer(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPer2(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPTra(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo)
      CHARACTER*4 InvRD
!     variables locales
!**********************
      INTEGER IBlo
      INTEGER IFlu
      INTEGER ILin
      INTEGER UWrite
      DOUBLE PRECISION  PerdEms
      DOUBLE PRECISION  PerdRec
      DOUBLE PRECISION  SumFlu
      DOUBLE PRECISION  SumFluMax
      DOUBLE PRECISION  SumIT
      DATA IREC /0/
      DOUBLE PRECISION D0
      DOUBLE PRECISION D1
      DOUBLE PRECISION D2
      DOUBLE PRECISION D3
      DOUBLE PRECISION D4
      DOUBLE PRECISION D5
      DOUBLE PRECISION D6
      DOUBLE PRECISION D7
      DOUBLE PRECISION D8

      INTEGER AbrirDirecto
      EXTERNAL AbrirDirecto
!

      PerdEms = 0.5d0
      PerdRec = 0.5d0
      IF (FPerdLin .EQ. 'E') THEN
         PerdEms = 1.0d0
         PerdRec = 0.0d0
      END IF
      IF (FPerdLin .EQ. 'R') THEN
         PerdEms = 0.0d0
         PerdRec = 1.0d0
      END IF
      IF (.NOT. FPerdTram) THEN
         PerdEms = 0.0d0
         PerdRec = 0.0d0
      END IF
      LRECL = 36
      UWrite = AbrirDirecto('plplin.res', 'UNKNOWN', LRECL, 'NATIVE', ULog)
      DO IBlo = 1, NBloques
         DO ILin = 1, NLinea
            IREC = IREC + 1
            SumFlu = 0.d0
            SumFluMax = 0.d0
            SumIT = 0.d0
            DO IFlu = 1, LinNFlu(ILin)
               SumFluMax = SumFluMax + MAX(                             &
     &              LinTMax(1, IFlu, ILin, IBlo),                       &
     &              LinTMax(2, IFlu, ILin, IBlo))
               IF (LinPTra(1, IFlu, ILin, IBlo) .GT. 0d0) THEN
                  SumFlu = SumFlu + LinPTra(1, IFlu, ILin, IBlo)
                  SumIT = SumIT +                                       &
     &                 LinPTra(1, IFlu, ILin, IBlo)*                    &
     &                 (CMg(LinNBar(2, ILin), IBlo) -                   &
     &                 CMg(LinNBar(1, ILin), IBlo))
               ELSE
                  SumFlu = SumFlu - LinPTra(2, IFlu, ILin, IBlo)
                  SumIT = SumIT +                                       &
     &                 LinPTra(2, IFlu, ILin, IBlo)*                    &
     &                 (CMg(LinNBar(1, ILin), IBlo) -                   &
     &                 CMg(LinNBar(2, ILin), IBlo))
               ENDIF
            ENDDO
            IF (LinPTra(1, 1, ILin, IBlo) .GT. 0d0) THEN
               SumIT = SumIT - LinPer(ILin , IBlo)*                     &
     &              (PerdRec*CMg(LinNBar(2, ILin), IBlo) +              &
     &              PerdEms*CMg(LinNBar(1, ILin), IBlo))
            ELSE
               SumIT = SumIT - LinPer(ILin, IBlo)*                      &
     &              (PerdRec*CMg(LinNBar(1, ILin), IBlo) +              &
     &              PerdEms*CMg(LinNBar(2, ILin), IBlo))
            ENDIF
            D0 = SumFlu
            IF (SumFluMax .GT. 1.D0) THEN
               D1 = ABS(SumFlu)/SumFluMax*100.D0
            ELSE
               D1 = 0.D0
            ENDIF
            D2 = SumFlu*BloDur(IBlo)/1000d0
            D3 = LinPer(ILin, IBlo)
            D4 = LinPer(ILin, IBlo)*BloDur(IBlo)/1000d0
            D5 = LinPer2(ILin, IBlo)
            D6 = LinPer2(ILin, IBlo)*BloDur(IBlo)/1000d0
            D7 = SumIT
            D8 = SumIT*BloDur(IBlo)/1000d0
            WRITE(UWrite, REC = IREC)                                   &
     &           InvRD(D0),                                             &
     &           InvRD(D1),                                             &
     &           InvRD(D2),                                             &
     &           InvRD(D3),                                             &
     &           InvRD(D4),                                             &
     &           InvRD(D5),                                             &
     &           InvRD(D6),                                             &
     &           InvRD(D7),                                             &
     &           InvRD(D8)
         ENDDO
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END

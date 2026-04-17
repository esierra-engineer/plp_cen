!************************************************
!     Subrutina Graba Archivo Flujos Total Lineas
!************************************************
      SUBROUTINE GraDatBDFlu(NArcNom, ISimul, NBloques, BloEta, BloDur, &
     &     TipoEtapa, FPerdTram, FPerdLin, NLinea, LinNom, LinNBar,     &
     &     LinNFlu, LinPTra, LinPer, LinPer2, LinTMax, CMg, Dim, ULog)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog
!
!     variables globales
!***********************
      CHARACTER*1 FPerdLin
      CHARACTER*48 LinNom(Dim%Lin)
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      CHARACTER*8 NomSimul
      INTEGER ISimul
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBloques
      INTEGER BloEta(Dim%Blo)
      INTEGER NLinea
      LOGICAL FPerdTram
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION CMg(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION LinPer(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPer2(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPTra(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo)
!     variables locales
!**********************
      INTEGER Abrir
      INTEGER IBlo
      INTEGER IFlu
      INTEGER ILin
      INTEGER UWrite
      DOUBLE PRECISION  PerdEms
      DOUBLE PRECISION  PerdRec
      DOUBLE PRECISION  SumFlu(NBloques)
      DOUBLE PRECISION  SumIT(NBloques)
      DOUBLE PRECISION  SumFluMax(NBloques)
      DOUBLE PRECISION  LinUse(NBloques)

 100  FORMAT((A6,",",I4,",",A,",",I5,",",A,2(",",I4),10(",",F8.2)))

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
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(6A)') 'Hidro,Bloque,TipoEtapa,',               &
     &        'LinNum,LinNom,BarA,BarB,LinFluP,LinFluE,',               &
     &        'LinFluMax,','LinUso,',                                   &
     &        'LinPerP,LinPerE,LinPer2P,LinPer2E,',                     &
     &        'LinITP,LinITE'
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      DO ILin = 1, NLinea
         DO IBlo = 1, NBloques
            SumFlu(IBlo) = 0.d0
            SumFluMax(IBlo) = 0.d0
            SumIT(IBlo) = 0.d0
            DO IFlu = 1, LinNFlu(ILin)
               SumFluMax(IBlo) = SumFluMax(IBlo) + MAX(                 &
     &              LinTMax(1, IFlu, ILin, IBlo),                       &
     &              LinTMax(2, IFlu, ILin, IBlo))               
               IF (LinPTra(1, IFlu, ILin, IBlo) .GT. 0d0) THEN
                  SumFlu(IBlo) = SumFlu(IBlo) +                         &
     &                 LinPTra(1, IFlu, ILin, IBlo)
                  SumIT(IBlo) = SumIT(IBlo) +                           &
     &                 LinPTra(1, IFlu, ILin, IBlo)*                    &
     &                 (CMg(LinNBar(2, ILin), IBlo) -                   &
     &                 CMg(LinNBar(1, ILin), IBlo))
               ELSE
                  SumFlu(IBlo) = SumFlu(IBlo) -                         &
     &                 LinPTra(2, IFlu, ILin, IBlo)
                  SumIT(IBlo) = SumIT(IBlo) +                           &
     &                 LinPTra(2, IFlu, ILin, IBlo)*                    &
     &                 (CMg(LinNBar(1, ILin), IBlo) -                   &
     &                 CMg(LinNBar(2, ILin), IBlo))
               ENDIF
            ENDDO
            IF (LinPTra(1, 1, ILin, IBlo) .GT. 0d0) THEN
               SumIT(IBlo) = SumIT(IBlo) - LinPer(ILin , IBlo)*         &
     &              (PerdRec*CMg(LinNBar(2, ILin), IBlo) +              &
     &              PerdEms*CMg(LinNBar(1, ILin), IBlo))
            ELSE
               SumIT(IBlo) = SumIT(IBlo) - LinPer(ILin, IBlo)*          &
     &              (PerdRec*CMg(LinNBar(1, ILin), IBlo) +              &
     &              PerdEms*CMg(LinNBar(2, ILin), IBlo))
            ENDIF

            LinUse(IBlo) = 0.d0
            IF (SumFluMax(IBlo) .GT. 1.D0) THEN
               LinUse(IBlo) =  ABS(SumFlu(IBlo))/SumFluMax(IBlo)*100.D0
            ELSE
               LinUse(IBlo) = 0.d0
            ENDIF
         ENDDO

         IF (ISimul .EQ. 0) THEN
            NomSimul = 'MEDIA'
         ELSE
            WRITE(NomSimul,'("Sim", I3)') ISimul 
         ENDIF         
         
         WRITE(UWrite, 100) &
     &        (NomSimul, &
     &        IBlo, &
     &        TipoEtapa(BloEta(IBlo)), &
     &        ILin, &
     &        LinNom(ILin), &
     &        LinNBar(1, ILin), &
     &        LinNBar(2, ILin), &
     &        SumFlu(IBlo), &
     &        SumFlu(IBlo)*BloDur(IBlo)*1d-3, &
     &        SumFluMax(IBlo), &
     &        LinUse(IBlo), &
     &        LinPer(ILin, IBlo), &
     &        LinPer(ILin, IBlo)*BloDur(IBlo)*1d-3, &
     &        LinPer2(ILin, IBlo), &
     &        LinPer2(ILin, IBlo)*BloDur(IBlo)*1d-3, &
     &        SumIT(IBlo), &
     &        SumIT(IBlo)*BloDur(IBlo)*1d-3, &
     &        IBlo = 1, NBloques) 
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END

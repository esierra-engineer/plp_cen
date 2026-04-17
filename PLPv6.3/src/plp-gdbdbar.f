!**********************
!     Graba Archivo CMg
!**********************
      SUBROUTINE GraDatBDCMg(NArcNom, ISimul,                           &
     &     NBloques, BloEta, BloDur, TipoEtapa, FPhi,                   &
     &     NBarra, BarNom, CMg, BloPot, BarPer, Dim, ULog)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog
      
!     
!     variables globales
!***********************
      CHARACTER*48 BarNom(Dim%Bar)
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      CHARACTER*48 BarNom2
      CHARACTER*8 NomSimul
      INTEGER ISimul
      INTEGER NBarra
      INTEGER NBloques
      INTEGER BloEta(Dim%Blo)
      DOUBLE PRECISION BarPer(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION BloPot(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION CMg(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION FPhi(Dim%Eta)
!     variables locales
!**********************
      INTEGER Abrir
      INTEGER IBlo
      INTEGER IBar
      INTEGER UWrite


 100  FORMAT((A6,",",I4,",",A,",",I4,",",A,2(",",F10.2),5(",",F15.3)))
      
!     
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite,'(5A)') 'Hidro,Bloque,TipoEtapa,',                &
     &        'BarNum,BarNom,CMgBar,',                                  &
     &        'DemBarP,DemBarE,',                                       &
     &        'PerBarP,PerBarE,',                                       &
     &        'BarRetP,BarRetE'    
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF

      IF (ISimul .EQ. 0) THEN
         NomSimul = 'MEDIA '
      ELSE
         WRITE(NomSimul,'("Sim", I3)') ISimul
      ENDIF

      DO IBar = 1, NBarra
         IF (NBarra .GT. 1) THEN
            BarNom2 = BarNom(IBar)
         ELSE
            BarNom2 = 'Uninodal    '
         ENDIF

         WRITE(UWrite, 100)   &
     &        (NomSimul, &
     &        IBlo, &
     &        TipoEtapa(BloEta(IBlo)), &
     &        IBar, &
     &        BarNom2, &
     &        CMg(IBar, IBlo)*FPhi(BloEta(IBlo)), &
     &        BloPot(IBar, IBlo), &
     &        BloPot(IBar, IBlo)*BloDur(IBlo)*1d-3, &
     &        BarPer(IBar, IBlo),  &
     &        BarPer(IBar, IBlo)*BloDur(IBlo)*1d-3, &
     &        BloPot(IBar, IBlo)*CMg(IBar, IBlo), &
     &        BloPot(IBar, IBlo)*CMg(IBar, IBlo)*BloDur(IBlo)*1d-3, &
     &        IBlo = 1, NBloques)
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END

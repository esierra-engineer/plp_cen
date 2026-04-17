!**********************
!     Graba Archivo CMg
!**********************
      SUBROUTINE GraDatBDCMg0(UPreSuf)
      USE PLP, ONLY : PAR_DIMS

      INTEGER UPreSuf

      WRITE(UPreSuf, '(A)') '"busFieldNames"'
      WRITE(UPreSuf, '(4A)')                                            &
     &     '"Costo Marginal [US$/MWh]",',                               &
     &     '"Demanda [MW]","Demanda [GWh]",',                           &
     &     '"Perdidas [MW]","Perdidas [GWh]",',                         &
     &     '"Retiro [US$/h]","Retiro [kUS$]"'
      WRITE(UPreSuf, '(A)') '"busFormat"'
      WRITE(UPreSuf, '(4A)')                                            &
     &     '"%10.2f",',                                                 &
     &     '"%10.2f","%10.4f",',                                        &
     &     '"%10.4f","%10.4f",',                                        &
     &     '"%10.4f","%10.4f"'
      WRITE(UPreSuf, '(A)') '"busPrefixNames"'
      WRITE(UPreSuf, '(A)')                                             &
     & '"Barra","Simulacion","Bloque","Numero Barra"'

      END

         
      SUBROUTINE GraDatBDCMg2(                                          &
     &     NBloques, BloEta, BloDur, FPhi,                              &
     &     NBarra, CMg, BloPot, BarPer, Dim, ULog)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

!     variables globales
!***********************
      INTEGER IREC
      INTEGER LRECL
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
      INTEGER IEta
      INTEGER IBlo
      INTEGER IBar
      INTEGER UWrite
      DOUBLE PRECISION D0
      DOUBLE PRECISION D1
      DOUBLE PRECISION D2
      DOUBLE PRECISION D3
      DOUBLE PRECISION D4
      DOUBLE PRECISION D5
      DOUBLE PRECISION D6
      CHARACTER*4 InvRD
      DATA IREC /0/

      INTEGER AbrirDirecto
      EXTERNAL AbrirDirecto
!
      LRECL = 28
      UWrite = AbrirDirecto('plpbar.res', 'UNKNOWN', LRECL, 'NATIVE', ULog)

      DO IBlo = 1, NBloques
         IEta = BloEta(IBlo)
         DO IBar = 1, NBarra
            IREC = IREC + 1
            D0 = CMg(IBar, IBlo)*FPhi(IEta)
            D1 = BloPot(IBar, IBlo)
            D2 = BloPot(IBar, IBlo)*BloDur(IBlo)/1000d0
            D3 = BarPer(IBar, IBlo)
            D4 = BarPer(IBar, IBlo)*BloDur(IBlo)/1000d0
            D5 = BloPot(IBar, IBlo)*CMg(IBar, IBlo)
            D6 = BloPot(IBar, IBlo)*BloDur(IBlo)/1000d0*CMg(IBar, IBlo)
            WRITE(UWrite, REC = IREC)                                   &
     &           InvRD(D0),                                             &
     &           InvRD(D1),                                             &
     &           InvRD(D2),                                             &
     &           InvRD(D3),                                             &
     &           InvRD(D4),                                             &
     &           InvRD(D5),                                             &
     &           InvRD(D6)
         ENDDO
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END

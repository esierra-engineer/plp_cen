      SUBROUTINE GraDatPlaEmb0(PDLDAcNCol, PDLDAcNom, UPreSuf, Dim)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim


      INTEGER UPreSuf
      INTEGER PDLDAcNCol

      CHARACTER*48 PDLDAcNom(Dim%PDLDAcCol)
      CHARACTER*48 Nombre
      INTEGER LCadena
      INTEGER I

         WRITE(UPreSuf, '(A)') '"planesFieldNames"'
         WRITE(UPreSuf, '(A, '','', $)') '"LD"'
         DO I = 1, PDLDAcNCol - 1
            Nombre = PDLDAcNom(I)
            WRITE(UPreSuf, '(A, A, A, '','', $)') '"',                     &
     &           Nombre(1:LCadena(Nombre)), '"'
         ENDDO
         Nombre = PDLDAcNom(PDLDAcNCol)
         WRITE(UPreSuf, '(A, A, A, $)') '"',                               &
     &        Nombre(1:LCadena(Nombre)), '"'
         WRITE(UPreSuf, *)
         WRITE(UPreSuf, '(A)') '"planesFormat"'
         WRITE(UPreSuf, '(A, '','', $)') '"%10.6e"'
         DO I = 1, PDLDAcNCol - 1
            WRITE(UPreSuf, '(A, '','', $)') '"%10.6e"'
         ENDDO
         WRITE(UPreSuf, '(A, $)') '"%10.6e"'
         WRITE(UPreSuf, *)
         WRITE(UPreSuf, '(A)') '"planesPrefixNames"'
         WRITE(UPreSuf, '(A, '','', $)') '"Plano"'
         WRITE(UPreSuf, '(A, '','', $)') '"Simulacion"'
         WRITE(UPreSuf, '(A, '','', $)') '"Etapa"'
         WRITE(UPreSuf, '(A, $)') '"Numero Iteracion"'
         WRITE(UPreSuf, *)
      

      END

      SUBROUTINE GraDatPlaEmb2(IREC, UWritePhi2, ScaleVol,     &
     &     GradxPhi, LDPhiPrv,                                 &
     &     NSimul, PDLDAcNCol, NEtapa, Dim,  &
     &     ULog)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

      EXTERNAL DxAEQy
      DOUBLE PRECISION ScaleVol(Dim%PDLDAcCol)
      DOUBLE PRECISION LDPhiPrv(Dim%Simul, Dim%Eta)
      DOUBLE PRECISION GradxPhi(Dim%PDLDAcCol, Dim%Simul, Dim%Eta)
      INTEGER IEtapa
      INTEGER ISimul
      INTEGER NEtapa
      INTEGER NSimul
      INTEGER PDLDAcNCol
      INTEGER UWritePhi2
      INTEGER IREC
      LOGICAL DxAEQy
      INTEGER IColAcop

!     variables locales
!**********************
      INTEGER LRECL
      INTEGER I
      DOUBLE PRECISION D(0:Dim%PDLDAcCol)
      CHARACTER*4 InvRD

      INTEGER AbrirDirecto
      EXTERNAL AbrirDirecto
!
!      LRECL = 28
!      UWritePhi2 = AbrirDirecto('plpplaem.res', 'UNKNOWN', LRECL,       &
!     &     'NATIVE', ULog)

      IF (UWritePhi2 .EQ. 0) THEN 


         LRECL = 4*(1 + PDLDAcNCol)
         UWritePhi2 = AbrirDirecto('plpplaem.res', 'UNKNOWN', LRECL,    &
     &        'NATIVE', ULog)

         IREC = 0
      ENDIF

      DO ISimul = 1, NSimul
        DO IEtapa = 2, NEtapa
            IREC = IREC + 1
            D(0) = LDPhiPrv(ISimul, IEtapa)
            DO IColAcop = 1, PDLDAcNCol
               D(IColAcop) = GradxPhi(IColAcop, ISimul, IEtapa)/ScaleVol(IColAcop)
            ENDDO
            WRITE(UWritePhi2, REC = IREC)                                &
     &           (InvRD(D(I)), I = 0, PDLDAcNCol)
         ENDDO
      ENDDO
      RETURN
      END



      SUBROUTINE GraDatPlaEmb3(NPlanos, ULog)
      USE PLP, ONLY : PAR_DIMS

      INTEGER NPlanos
      INTEGER ULog
!     variables locales
!**********************
      INTEGER Abrir
      INTEGER I
      INTEGER ICen
      INTEGER IDec
      INTEGER IUni
      INTEGER UWrite
      CHARACTER*6 NombrePlano
      NombrePlano = 'Ite000'

      UWrite = Abrir('planos.csv', 'UNKNOWN', 'SEQUENTIAL', ULog)
      WRITE(UWrite, '(A)') '#Numero, Nombre, Tipo'
      DO I = 1, NPlanos
         WRITE(UWrite, '(I3, '','', $)') I
            ICen = I/100
            IDec = I/10 - 10*ICen
            IUni = I - 10*IDec  - 100*ICen
            NombrePlano(4:4) = CHAR(48 + INT(ICen))
            NombrePlano(5:5) = CHAR(48 + INT(IDec))
            NombrePlano(6:6) = CHAR(48 + INT(IUni))
         WRITE(UWrite, '( A, '','', $)') NombrePlano
         WRITE(UWrite, *)
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END

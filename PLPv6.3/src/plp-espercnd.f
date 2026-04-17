!*****************************************************************
!     Subrutina en cierto modo analoga a la rutina _bfuturo_ del caso
!     deterministico. Se utiliza en el calculo de las aproximaciones de
!     la funcion de costo futuro esperada
!*****************************************************************
      SUBROUTINE EsperCnd(PromedioZ, PromedioPi, IEtapa,                &
     &     GradxPhi, LDPhiPrv,                                          &
     &     ISimul, NApert,                                              &
     &     PDLDAcNFila,                                                 &
     &     PDLDAcNCol,                                                  &
     &     ULog, Dim)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN)::  Dim
      INCLUDE 'machcons.fpp'
      INTEGER, INTENT(IN):: IEtapa
      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: NApert
      INTEGER, INTENT(IN):: PDLDAcNCol
      INTEGER, INTENT(IN):: PDLDAcNFila
      INTEGER, INTENT(IN):: ULog


!     outs
      DOUBLE PRECISION, INTENT(INOUT):: PromedioZ
      DOUBLE PRECISION, INTENT(OUT):: GradxPhi(Dim%PDLDAcCol, Dim%Simul, Dim%Eta)
      DOUBLE PRECISION, INTENT(OUT):: LDPhiPrv(Dim%Simul, Dim%Eta)
      DOUBLE PRECISION, INTENT(INOUT):: PromedioPi(Dim%EstocFila)
!     locals
      INTEGER IFilaAcop
      INTEGER IColAcop


!     codigo:

      IF (IEtapa .LE. 1) THEN
         WRITE(6, '(A, I4, A)')                                         &
     &        'espercnd: Error, x IEtapa = ', IEtapa, '.'
         WRITE(ULog, '(A, I4, A)')                                      &
     &        'espercnd: Error, IEtapa = ', IEtapa, '.'
         STOP 1
      ENDIF
      PromedioZ = PromedioZ/DBLE(NApert)
!     PromedioPi := <<PromedioPi(IApert2)>>_{IApert2 = 1..NApert(ISimul,
      DO IFilaAcop = 1, PDLDAcNFila
         PromedioPi(IFilaAcop) = PromedioPi(IFilaAcop)/                 &
     &        DBLE(NApert)
      ENDDO

      DO IColAcop = 1, PDLDAcNCol
         GradxPhi(IColAcop, ISimul, IEtapa) = PromedioPi(IColAcop)
      ENDDO

      LDPhiPrv(ISimul, IEtapa) = PromedioZ
      RETURN
      END

!*****************************************************************
!     Subrutina en cierto modo analoga a la rutina _bfuturo_ del caso
!     deterministico. Se utiliza en el calculo de las aproximaciones de
!     la funcion de costo futuro esperada
!*****************************************************************
      SUBROUTINE SaveGradxPhi(PDNAprox,                                 &
     &     UGradxPhi, GradxPhi, IEtapa,                                 &
     &     ISimul, NSimul,                                              &
     &     PDLDAcNCol,                                                  &
     &     NEtapa, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INCLUDE 'machcons.fpp'
      INTEGER, INTENT(IN):: IEtapa
      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: NEtapa
      INTEGER, INTENT(IN):: NSimul
      INTEGER, INTENT(IN):: PDLDAcNCol
      INTEGER, INTENT(IN):: ULog
      INTEGER, INTENT(IN):: PDNAprox(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: GradxPhi(Dim%PDLDAcCol, Dim%Simul)
!     outs
      INTEGER, INTENT(INOUT):: UGradxPhi
!     locals
      INTEGER IColAcop
      INTEGER Ptrx

      INTEGER NAprEtPrev
      INTEGER NAprox


!     codigo:
      IF (IEtapa .LE. 1) THEN
         WRITE(6, '(A, I4, A)')                                         &
     &        'espercnd: Error, x IEtapa = ', IEtapa, '.'
         WRITE(ULog, '(A, I4, A)')                                      &
     &        'espercnd: Error, IEtapa = ', IEtapa, '.'
         STOP 1
      ENDIF

      NAprox = PDNAprox(IEtapa)
      NAprEtPrev = PDNAprox(IEtapa - 1) + 1

      PtrX = (NAprEtPrev - 1)*NEtapa*NSimul*PDLDAcNCol +                &
     &     (NEtapa - IEtapa)*NSimul*PDLDAcNCol +                        &
     &     (ISimul - 1)*PDLDAcNCol
      DO IColAcop = 1, PDLDAcNCol
         WRITE(UGradxPhi, rec = PtrX + IColAcop)                        &
     &        GradxPhi(IColAcop, ISimul)
      ENDDO

      RETURN
      END


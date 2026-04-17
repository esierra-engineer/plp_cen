!***************************************
!     Calcula perdidas por tramos lineas
!***************************************
      SUBROUTINE CalPTrai(                                              &
     &     NLinea, FPerdLin,                                            &
     &     LinFPer, LinNBar, LinRes, LinVNom,                           &
     &     LinTMax, LinNFlu, LinPTra,                                   &
     &     BarPer, LinPer,                                              &
     &     LinPer2, Dim)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     Variables Globales
!***********************
      CHARACTER*1 FPerdLin
      INTEGER IFlu
      INTEGER ILin
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NFlu
      INTEGER NLinea
      LOGICAL LinFPer(Dim%Lin)
      DOUBLE PRECISION  PerdEms
      DOUBLE PRECISION  PerdRec
      DOUBLE PRECISION BarPer(Dim%Bar)
      DOUBLE PRECISION LinPer(Dim%Lin)
      DOUBLE PRECISION LinPer2(Dim%Lin)
      DOUBLE PRECISION LinPTra(2, Dim%Flu, Dim%Lin)
      DOUBLE PRECISION LinRes(Dim%Lin)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin)
      DOUBLE PRECISION LinVNom(Dim%Lin)
      DOUBLE PRECISION LinPerE
      DOUBLE PRECISION LinPerR
      DOUBLE PRECISION LinPotE
      DOUBLE PRECISION LinPotR
      DOUBLE PRECISION LinRImp

      PerdEms = 0.5d0
      PerdRec = 0.5d0
      IF (FPerdLin .EQ. 'E') THEN
         PerdEms = 1.0d0
         PerdRec = 0d0
      END IF
      IF (FPerdLin .EQ. 'R') THEN
         PerdEms = 0d0
         PerdRec = 1.0d0
      END IF
      DO ILin = 1, NLinea
         LinPotE = 0.0d0
         LinPotR = 0.0d0
         IF (LinFPer(ILin)) THEN
            NFlu = LinNFlu(ILin)
            DO IFlu = 1, NFlu
               LinPotR = LinPotR + LinPTra(1, IFlu, ILin)
               LinPotE = LinPotE + LinPTra(2, IFlu, ILin)
               LinRImp = LinRes(ILin)*dble(2*IFlu - 1)
               LinPerR = LinRImp/                                       &
     &              (LinVNom(ILin)*LinVNom(ILin))*                      &
     &              LinPTra(1, IFlu, ILin)*                             &
     &              LinTMax(1, IFlu, ILin)
               LinPerE = LinRImp/                                       &
     &              (LinVNom(ILin)*LinVNom(ILin))*                      &
     &              LinPTra(2, IFlu, ILin)*                             &
     &              LinTMax(2, IFlu, ILin)
               LinPer(ILin) = LinPerR + LinPerE +                       &
     &              LinPer(ILin)
               BarPer(LinNBar(1, ILin)) = PerdRec*LinPerR +             &
     &              PerdEms*LinPerE +                                   &
     &              BarPer(LinNBar(1, ILin))
               BarPer(LinNBar(2, ILin)) = PerdRec*LinPerE +             &
     &              PerdEms*LinPerR +                                   &
     &              BarPer(LinNBar(2, ILin))
            ENDDO

            LinRImp = LinRes(ILin)
            LinPerR = LinRImp/                                          &
     &           (LinVNom(ILin)*LinVNom(ILin))*                         &
     &           LinPTra(1, NFlu, ILin)*                                &
     &           LinTMax(1, NFlu, ILin)
            LinPerE = LinRImp/                                          &
     &           (LinVNom(ILin)*LinVNom(ILin))*                         &
     &           LinPTra(2, NFlu, ILin)*                                &
     &           LinTMax(2, NFlu, ILin)
            LinPer2(ILin) = + LinRImp/                                  &
     &           (LinVNom(ILin)*LinVNom(ILin))*                         &
     &           (LinPotR*LinPotR + LinPotE*LinPotE)
         ENDIF
      ENDDO
      RETURN
      END


      SUBROUTINE CalPTra(NBloques,                                      &
     &     NLinea, NBarra, FPerdLin,                                    &
     &     LinFPer, LinNBar, LinRes, LinVNom,                           &
     &     LinTMax, LinNFlu, LinPTra,                                   &
     &     BarPer, LinPer,                                              &
     &     LinPer2, Dim)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     Variables Globales
!***********************
      CHARACTER*1 FPerdLin
      INTEGER IBlo
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBarra
      INTEGER NBloques
      INTEGER NLinea
      LOGICAL LinFPer(Dim%Lin)
      DOUBLE PRECISION BarPer(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION LinPer(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPer2(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPTra(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinRes(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinVNom(Dim%Lin, Dim%Blo)

      BarPer(1:NBarra, 1:NBloques) = 0.d0
      LinPer(1:NLinea, 1:NBloques) = 0.d0
      LinPer2(1:NLinea, 1:NBloques) = 0.d0

      DO IBlo = 1, NBloques
         CALL CalPTrai(                                                 &
     &     NLinea, FPerdLin,                                            &
     &     LinFPer, LinNBar, LinRes(1, IBlo), LinVNom(1, IBlo),         &
     &     LinTMax(1, 1, 1, IBlo), LinNFlu, LinPTra(1, 1, 1, IBlo),     &
     &     BarPer(1, IBlo), LinPer(1, IBlo),                            &
     &     LinPer2(1, IBlo), Dim)
      ENDDO

      RETURN
      END

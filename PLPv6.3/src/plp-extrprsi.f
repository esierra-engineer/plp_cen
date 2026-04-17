!**********************************
!     calcula la simulacion promedio
!**********************************
      SUBROUTINE ExtrPromi (                                            &
     &     NSimul, NBloques,                                            &
     &     NCenPas, NBarra, NLinea,                                     &
     &     NCols, NFilas,                                               &
     &     EtaDual, EtaPrimal, PasQAfl, BarPer, LinPer, LinPer2,        &
     &     EtaDualS, EtaPrimalS, PasQAflS, BarPerS, LinPerS, LinPer2S,  & 
     &     Dim)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!
      INTEGER NCols, NFilas

      DOUBLE PRECISION EtaDual(NFilas)
      DOUBLE PRECISION EtaPrimal(NCols)
      DOUBLE PRECISION PasQAfl(Dim%Pas, Dim%Blo)
      DOUBLE PRECISION BarPer(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION LinPer(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPer2(Dim%Lin, Dim%Blo)

      DOUBLE PRECISION EtaDualS(NFilas)
      DOUBLE PRECISION EtaPrimalS(NCols)
      DOUBLE PRECISION PasQAflS(Dim%Pas, Dim%Blo)
      DOUBLE PRECISION BarPerS(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION LinPerS(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPer2S(Dim%Lin, Dim%Blo)

      INTEGER NBloques
      INTEGER NSimul
      INTEGER NCenPas, NBarra, NLinea
      
      DOUBLE PRECISION fact

      fact =  1.0d0 / NSimul

      EtaPrimalS(1:NCols) =                                          &
     &     EtaPrimalS(1:NCols) + EtaPrimal(1:NCols) * fact
      
      EtaDualS(1:NFilas) =                                           &
     &     EtaDualS(1:NFilas) + EtaDual(1:NFilas) * fact

      PasQAflS(1:NCenPas, 1:NBloques) =                              &
     &     PasQAflS(1:NCenPas, 1:NBloques)                           &
     &     + PasQAfl(1:NCenPas, 1:NBloques) * fact

      BarPerS(1:NBarra, 1:NBloques) =                                &
     &     BarPerS(1:NBarra, 1:NBloques)                             &
     &     + BarPer(1:NBarra, 1:NBloques) * fact

      LinPerS(1:NLinea, 1:NBloques) =                                &
     &     LinPerS(1:NLinea, 1:NBloques)                             &
     &     + LinPer(1:NLinea, 1:NBloques) * fact

      LinPer2S(1:NLinea, 1:NBloques) =                               &
     &     LinPer2S(1:NLinea, 1:NBloques)                            &
     &     + LinPer2(1:NLinea, 1:NBloques) * fact

      RETURN
      END


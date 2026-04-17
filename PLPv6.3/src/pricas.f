      SUBROUTINE PriCas(NCentral, NCenEmb, NCenSer, NCenPas,            &
     &     NBarra, NLinea, NBloques, NEtapa, NCenFalla,              &
     &     NCenEmbCFUE, NSimul, FPerdTram, NCenRes, NZonRes, IUnit)

      INTEGER IUnit
      INTEGER NBarra
      INTEGER NBloques
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NCenEmbCFUE
      INTEGER NCenFalla
      INTEGER NCenPas
      INTEGER NCenSer
      INTEGER NCenTer
      INTEGER NCentral
      INTEGER NLinea
      INTEGER NSimul
      LOGICAL FPerdTram
      INTEGER NCenRes, NZonRes


      NCenTer = NCentral - NCenEmb - NCenSer - NCenPas - NCenFalla
      IF (FPerdTram) THEN
         WRITE(IUnit, '(A)') 'pricas: Modela perdidas por tramo.'
      ENDIF
      WRITE(IUnit, '(A, 7(I4, A))') 'pricas:',                          &
     &     NBloques, ' blo, ',                                          &
     &     NCentral, ' cen, ',                                          &
     &     NBarra, ' bar, ',                                            &
     &     NLinea, ' lin, ',                                            &
     &     NCenEmb, ' emb, ',                                           &
     &     NCenSer, ' ser, ',                                           &
     &     NCenRes, ' cre.'
      WRITE(IUnit, '(A, 7(I4, A))') 'pricas:',                          &
     &     NEtapa, ' eta, ',                                            &
     &     NCenPas, ' pas, ',                                           &
     &     NCenTer, ' ter, ',                                           &
     &     NCenFalla, ' fal, ',                                         &
     &     NCenEmbCFUE, ' ecf, ',                                       &
     &     NSimul, ' sim, ',                                            &
     &     NZonRes, ' zre.'
      RETURN
      END

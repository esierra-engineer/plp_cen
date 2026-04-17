      SUBROUTINE LeeCnfLinDim(ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcCnfLin

      TYPE(PAR_DIMS) Dim
      INTEGER ULog
      CHARACTER*12 AuxVar
      INTEGER Abrir
      INTEGER URead

      INTEGER NLinea

      CHARACTER*48 LinNom
      DOUBLE PRECISION LinAB
      DOUBLE PRECISION LinBA
      INTEGER LinNBar, LinNFlu
      DOUBLE PRECISION LinVNom, LinRes, LinXImp
      LOGICAL LinFPer

      INTEGER ILin

!
      URead = Abrir(NArcCnfLin, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecnflin: Error, no existe archivo ',       &
     &        NArcCnfLin, '.'
         WRITE(ULog, '(3A)') 'leecnflin: Error, no existe el archivo ', &
     &        NArcCnfLin, '.'
         STOP 1
      ENDIF

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NLinea

      Dim%Lin = MAX(NLinea, 1)

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar

      Dim%Flu = 0
      DO ILin = 1, NLinea
         READ(URead, *)                                                 &
     &        LinNom,                                                   &
     &        LinAB, LinBA,                                             &
     &        LinNBar, LinNBar,                                         &
     &        LinVNom, LinRes,                                          &
     &        LinXImp, LinFPer,                                         &
     &        LinNFlu

         Dim%Flu = MAX(Dim%Flu, LinNFlu)
      ENDDO
      Dim%Flu = MAX(Dim%Flu, 1)

      CALL Cerrar(URead)

      RETURN
      END

!**********************************
!     Subrutina Lee Configuracion Lineas
!**********************************
      SUBROUTINE LeeCnfLin(                                             &
     &     NBloques, NBarra, NLinea, FPerdTram, FPerdLin,               &
     &     LinNom, LinNBar, LinNFlu, LinFPer, ThetaRef,                 &
     &     LinFOpe, LinRes, LinTMax, LinVNom, LinXImp, LinHVDC,         &
     &     NFlujo, ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcCnfLin

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     comun a todas las rutinas:
      INCLUDE 'machcons.fpp'
      CHARACTER*1 FPerdLin
      CHARACTER*12 AuxVar
      CHARACTER*48 LinNom(Dim%Lin)
      INTEGER Abrir
      INTEGER IBlo
      INTEGER IFlu
      INTEGER ILin
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBarra
      INTEGER NBloques
      INTEGER NFlujo
      INTEGER NLinea, io
      INTEGER ULog
      INTEGER URead
      LOGICAL FOpe(Dim%Lin)
      LOGICAL FPerdTram
      LOGICAL FStop
      LOGICAL FWarning
      LOGICAL LinFOpe(Dim%Lin, Dim%Blo)
      LOGICAL LinHVDC(Dim%Lin)
      LOGICAL LinFPer(Dim%Lin)
      LOGICAL HVDCMode
      DOUBLE PRECISION DFluAB
      DOUBLE PRECISION DFluBA
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinVNom(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinXImp(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinAB(Dim%Lin)
      DOUBLE PRECISION LinBA(Dim%Lin)
      DOUBLE PRECISION LinRes(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION ThetaRef
!***************************
!     Lee Datos de NArcCnfLin.Dat
!***************************
      HVDCMode = .TRUE.    
99    URead = Abrir(NArcCnfLin, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecnflin: Error, no existe archivo ',       &
     &        NArcCnfLin, '.'
         WRITE(ULog, '(3A)') 'leecnflin: Error, no existe el archivo ', &
     &        NArcCnfLin, '.'
         STOP 1
      ENDIF
!****************
!     Numero de Lineas
!****************
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NLinea, FPerdTram, FPerdLin, ThetaRef
!**********************************
!     chequeo de la validez de los datos
!**********************************
      FStop = .FALSE.
      FWarning = .FALSE.
      IF (NLinea .GT. Dim%Lin) THEN
         WRITE(6, '(A, I4, A)') 'leecnflin: Numero de lineas > ',       &
     &        Dim%Lin, ', fin.'
         WRITE(ULog, '(A, I4, A)') 'leecnflin: Numero de lineas > ',    &
     &        Dim%Lin, ', fin.'
         STOP 1
      ENDIF
      IF ((NLinea .EQ. 0) .AND. FPerdTram) THEN
         WRITE(6, '(2A)') 'leecnflin: Warning, NLinea = 0 y ',          &
     &        'FPerdTram = T.'
         WRITE(6, '(A)') 'leecnflin: Se hace FPerdTram = F.'
         WRITE(ULog, '(2A)') 'leecnflin: Warning, NLinea = 0 y ',       &
     &        'FPerdTram = T.'
         WRITE(ULog, '(A)') 'leecnflin: Se hace FPerdTram = F.'
         FPerdTram = .FALSE.
      ENDIF
      IF ((FPerdLin .NE. 'E') .AND. (FPerdLin .NE. 'R') .AND.           &
     &     (FPerdLin .NE. 'M')) THEN
         FPerdLin = 'M'
         WRITE(6, '(A)') 'leecnflin: Se hace FPerdLin = M.'
         WRITE(ULog, '(A)') 'leecnflin: Se hace FPerdLin = M.'
      ENDIF
!***************************
!     Lee Datos Lineas Trasmision
!***************************
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      DO ILin = 1, NLinea
         IF (HVDCMode) THEN
            READ(URead, *, IOSTAT=io)                                   &
     &           LinNom(ILin),                                          &
     &           LinAB(ILin), LinBA(ILin),                              &
     &           LinNBar(1, ILin), LinNBar(2, ILin),                    &
     &           LinVNom(ILin, 1), LinRes(ILin, 1),                     &
     &           LinXImp(ILin, 1), LinFPer(ILin), LinNFlu(ILin),        &
     &           FOpe(ILin), LinHVDC(ILin)
            IF (io > 0) THEN
                 HVDCMode = .FALSE.
                 CALL Cerrar(URead)
                 GO TO 99
            ENDIF
         ELSE
            LinHVDC(ILin) = .FALSE.
            READ(URead, *, IOSTAT=io)                                   &
     &           LinNom(ILin),                                          &
     &           LinAB(ILin), LinBA(ILin),                              &
     &           LinNBar(1, ILin), LinNBar(2, ILin),                    &
     &           LinVNom(ILin, 1), LinRes(ILin, 1),                     &
     &           LinXImp(ILin, 1), LinFPer(ILin), LinNFlu(ILin),        &
     &           FOpe(ILin)
         ENDIF
         IF (io < 0) THEN 
              EXIT
         ENDIF
!     $  '(A12, 3x, 2(f8.1, 3x), 2(I6, 3x), f7.1, 1x, 2(f7.3, 1x), 
!     $    7x, L1, 3x, I5, 10x, L1)')
         IF (                                                           &
     &        (LinNBar(1, ILin) .LT. 1) .OR.                            &
     &        (LinNBar(1, ILin) .GT. NBarra) .OR.                       &
     &        (LinNBar(2, ILin) .LT. 1) .OR.                            &
     &        (LinNBar(2, ILin) .GT. NBarra)                            &
     &        ) THEN
            WRITE(6, '(3A)') 'leecnflin: Linea ', LinNom(ILin),         &
     &           ' esta conectada a una barra inexistente.'
            WRITE(ULog, *) 'leecnflin: Linea ', LinNom(ILin),           &
     &           ' esta conectada a una barra inexistente.'
            FStop = .TRUE.
         ENDIF
         IF (                                                           &
     &        (LinXImp(ILin, 1) .LT. SEPSILON) .OR.                     &
     &        (linVNom(ILin, 1) .LT. SEPSILON)                          &
     &        ) THEN
            WRITE(6, '(3A)') 'leecnflin: Linea ', LinNom(ILin),         &
     &           ' tiene el voltaje o la reactancia en cero.'
            WRITE(ULog, *) 'leecnflin: Linea ', LinNom(ILin),           &
     &           ' tiene el voltaje o la reactancia en cero.'
            FStop = .TRUE.
         ENDIF
      ENDDO
      IF (FStop) THEN
         STOP 1
      ENDIF
      
      CALL Cerrar(URead)
!     Traspasa datos a todas las etapas y Tramos de flujo
!***************************************************
      DO ILin = 1, NLinea
         IF (LinNFlu(ILin) .GT. Dim%Flu) THEN
            WRITE(6, '(A, I3, A)') 'leecnflin: Num. tramos perdida > ', &
     &           Dim%Flu, ', fin.'
            WRITE(ULog, '(A, I3, A)')                                   &
     &           'leecnflin: Num. tramos perdida > ',                   &
     &           Dim%Flu, ', fin.'
            STOP 1
         ENDIF
         IF (.NOT. (FPerdTram .AND. LinFPer(ILin))) THEN
            LinNFlu(ILin) = 1
         ENDIF
         DO IBlo = 1, NBloques
            LinFOpe(ILin, IBlo) = FOpe(ILin)
         ENDDO
         DFluAB = LinAB(ILin)/dble(LinNFlu(ILin))
         DFluBA = LinBA(ILin)/dble(LinNFlu(ILin))
         DO IFlu = 1, LinNFlu(ILin)
            DO IBlo = 1, NBloques
               LinTMax(1, IFlu, ILin, IBlo) = DFluAB
               LinTMax(2, IFlu, ILin, IBlo) = DFluBA
            ENDDO
            DO IBlo = 1, NBloques
               LinRes(ILin, IBlo) = LinRes(ILin, 1)
               LinXImp(ILin, IBlo) = LinXImp(ILin, 1)
               LinVNom(ILin, IBlo) = LinVNom(ILin, 1)
            ENDDO
         ENDDO
      ENDDO
      NFlujo = 0
      DO ILin = 1, NLinea
         NFlujo = NFlujo + LinNFlu(ILin)
      ENDDO
      RETURN
      END

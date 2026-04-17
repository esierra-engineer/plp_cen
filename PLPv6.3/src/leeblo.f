      SUBROUTINE LeeBloDim(Dim, ULog)

      USE PLP, ONLY : PAR_DIMS, NArcBlo
 
      TYPE(PAR_DIMS) Dim
      INTEGER ULog

      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER URead
      INTEGER NBloque
      CHARACTER*12 AuxVar
      INTEGER IEta, CEta, IBlo
      INTEGER NumBlo, NumEta

      URead = Abrir(NArcBlo, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         Dim%Blo = Dim%Eta
         Dim%IBlo = 1 
      ELSE
!     Numero de Bloques y unidad de tiempo
         READ(URead, '(A12)') AuxVar
         READ(URead, '(A12)') AuxVar
         READ(URead, *) NBloque

         Dim%Blo = NBloque

         READ(URead, '(A12)') AuxVar
         IEta = 0
         CEta = 0
         Dim%IBlo = 1
         DO IBlo = 1, NBloque
            READ(URead, *) NumBlo, NumEta
            if (IEta .NE. NumEta) THEN               
               CEta = 0
               IEta = NumEta
            ENDIF
            CEta = CEta + 1
            
            Dim%IBlo = MAX(Dim%IBlo, CEta)
         ENDDO
      ENDIF
      
      CALL Cerrar(URead)

      RETURN      
      END

!************************************
!>     Subrutina LeeBlo
!>     Lee Duracion de las Bloques y Relacion con Etapas
!************************************
      SUBROUTINE LeeBlo(NEtapa, EtaDur,                & 
     &     NBloque, BloEta, BloDur,                    &
     &     NBloEta, BloInd,                            &
     &     ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcBlo

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     comun a todas las rutinas:

      INTEGER NEtapa
      DOUBLE PRECISION EtaDur(Dim%Eta)
      INTEGER ULog

      INTEGER NBloque
      INTEGER BloEta(Dim%Blo)
      DOUBLE PRECISION BloDur(Dim%Blo)

      INTEGER NBloEta(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)

      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      DOUBLE PRECISION DurBlo
      INTEGER IBlo, IEta
      DOUBLE PRECISION NDuraBlo
      DOUBLE PRECISION NDuraEta
      INTEGER NumBlo
      INTEGER NumEta
      INTEGER URead

      LOGICAL DxAEQy

!     codigo:
!***********************
!     Lee datos pcpblo.dat
!***********************
      URead = Abrir(NArcBlo, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         NBloque = NEtapa
         DO IBlo = 1, NBloque
            BloEta(IBlo) = IBlo
            BloDur(IBLo) = EtaDur(IBlo)
         ENDDO
      ELSE
!     Numero de Bloques y unidad de tiempo
         READ(URead, '(A12)') AuxVar
         READ(URead, '(A12)') AuxVar
         READ(URead, *) NBloque
!     Verifica Dimensiones
!********************
         IF (NBloque .GT. Dim%Blo) THEN
            WRITE(6, '(A, I4, A)') 'leeblo: Error, Numero de bloques >', &
     &           Dim%Blo, ', fin.'
            WRITE(ULog, '(A, I4, A)') & 
     &           'leeblo: Error, Numero de bloques >',  &
     &           Dim%Blo, ', fin.'
            STOP 1
         ENDIF
!     Lee Duracion Bloques
!*******************
         READ(URead, '(A12)') AuxVar
         IEta = 0
         DO IBlo = 1, NBloque
            READ(URead, *) NumBlo, NumEta, DurBlo

            IF (NumBlo .NE. IBlo) THEN
               WRITE(6, '(A)') 'leeblo: Error en datos bloques.'
               WRITE(6, '(A, 2(A, I4), A)')                             &
     &              'leeblo: Numero de bloque fuera de orden: ',        &
     &              '1 <', NumBlo, ' <', IBlo, '.'
               WRITE(ULog, '(A)') 'leeblo: Error en datos bloques.'
               WRITE(ULog, '(A, 2(A, I4), A)')                          &
     &              'leeblo: Numero de bloque fuera de orden: ',        &
     &              '1 <', NumBlo, ' <', IBlo, '.'
               STOP 1
            ENDIF

            IF((NumEta .LT. 1) .OR. (NumEta .GT. NEtapa)) THEN
               WRITE(6, '(A)') 'leeblo: Error en datos bloques.'
               WRITE(6, '(A, 2(A, I4), A)')                             &
     &              'leeblo: Numero de bloque fuera de rango: ',        &
     &              '1 <', NumEta, ' <', NEtapa, '.'
               WRITE(ULog, '(A)') 'leeblo: Error en datos bloques.'
               WRITE(ULog, '(A, 2(A, I4), A)')                          &
     &              'leeblo: Numero de bloque fuera de rango: ',        &
     &              '1 <', NumEta, ' <', NEtapa, '.'
               STOP 1
            ENDIF

            BloEta(IBlo) = NumEta
            BloDur(IBlo) = DurBlo
         ENDDO
      ENDIF

      NDuraEta = 0.0d0
      DO IEta = 1, NEtapa
         NDuraEta = NDuraEta + EtaDur(IEta)
      ENDDO

      NDuraBlo = 0.0d0
      DO IBlo = 1, NBloque
         NDuraBlo = NDuraBLo + BloDur(IBLo)
      ENDDO      
      
      IF (.NOT. DxAEQy(NDuraEta, NDuraBlo, 0.0d0)) THEN
         WRITE(6, '(A)') 'leeblo: Error en datos bloques.'
         WRITE(6, *)                           &
     &        'leeblo: Duracion de bloques difiere de etapa: ',&
     &        '1 <', NDuraBlo, ' <', NDuraEta, '.'
         WRITE(ULog, '(A)') 'leeblo: Error en datos bloques.'
         WRITE(ULog, *)                        &
     &        'leeblo: Duracion de bloques difiere de etapa: ',&
     &        '1 <', NDuraBlo, ' <', NDuraEta, '.'
         STOP 1
      ENDIF

      NBloEta = 0
      DO IBlo = 1, NBloque
         IEta = BloEta(IBlo)
         NBloEta(IEta) =  NBloEta(IEta) + 1
         BloInd(NBloEta(IEta), IEta) = IBlo
      ENDDO
      
      CALL Cerrar(URead)
      RETURN
      END

!***********************************************************************
!     traspasa informacion de un Archivo de Acceso Directo a memoria RAM
!***********************************************************************
      SUBROUTINE AAD2RAMd(Unidad, IPtr, Arreglo, NRec, ULog)
      IMPLICIT NONE
      INTEGER IRec
      INTEGER NRec
      INTEGER IPtr
      INTEGER IPtr2
      INTEGER Unidad
      INTEGER ULog
      DOUBLE PRECISION Arreglo (*)
!
      IPtr2 = IPtr - 1
      DO IRec = 1, NRec
         READ(Unidad, rec = IPtr2 + IRec, ERR=20) Arreglo(IRec)
      ENDDO
      RETURN

 20   WRITE (6,*) 'Error de lectura en AAD2Ramd', Unidad, IPtr, IRec
      WRITE (ULog,*) 'Error de lectura en AAD2Ramd', Unidad, IPtr, IRec
      STOP 1
      END
!***********************************************************************
!     traspasa informacion de memoria RAM a un Archivo de Acceso Directo
!***********************************************************************
      SUBROUTINE RAM2AADd(Unidad, IPtr, Arreglo, NRec)
      IMPLICIT NONE
      INTEGER IRec
      INTEGER NRec
      INTEGER IPtr
      INTEGER IPtr2
      INTEGER Unidad
      DOUBLE PRECISION Arreglo (*)
!
      IPtr2 = IPtr - 1
      DO IRec = 1, NRec
         WRITE(Unidad, rec = IPtr2 + IRec) Arreglo(IRec)
      ENDDO
      RETURN
      END
!***********************************************************************
!     traspasa informacion de un Archivo de Acceso Directo a memoria RAM
!***********************************************************************
      SUBROUTINE AAD2RAMi(Unidad, IPtr, Arreglo, NRec)
      IMPLICIT NONE
      INTEGER IRec
      INTEGER NRec
      INTEGER IPtr
      INTEGER IPtr2
      INTEGER Unidad
      INTEGER Arreglo (*)
!
      IPtr2 = IPtr - 1
      DO IRec = 1, NRec
         READ(Unidad, rec = IPtr2 + IRec) Arreglo(IRec)
      ENDDO
      RETURN
      END
!***********************************************************************
!     traspasa informacion de memoria RAM a un Archivo de Acceso Directo
!***********************************************************************
      SUBROUTINE RAM2AADi(Unidad, IPtr, Arreglo, NRec)
      IMPLICIT NONE
      INTEGER IRec
      INTEGER NRec
      INTEGER IPtr
      INTEGER IPtr2
      INTEGER Unidad
      INTEGER Arreglo (*)
!
      IPtr2 = IPtr - 1
      DO IRec = 1, NRec
         WRITE(Unidad, rec = IPtr2 + IRec) Arreglo(IRec)
      ENDDO
      RETURN
      END

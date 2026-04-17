      INTEGER FUNCTION GetUnit()
      INCLUDE 'pxp.fpp'
      INTEGER NUnit(99)
      INTEGER IUnit
      COMMON /UNITNUM/NUnit
!     PELIGRO, WILL ROBINSON, PELIGRO!(10)
!     Ojo con el archivo pxp.fpp y el parametro UnitIni.
      DATA NUnit/                                                       &
     &     1, 2, 3, 4, 5, 6, 7, 8, 9, 10,                               &
     &     11, 12, 13, 14, 15, -16, -17, -18, -19, -20,                 &
     &     -21, -22, -23, -24, -25, -26, -27, -28, -29, -30,            &
     &     -31, -32, -33, -34, -35, -36, -37, -38, -39, -40,            &
     &     -41, -42, -43, -44, -45, -46, -47, -48, -49, -50,            &
     &     -51, -52, -53, -54, -55, -56, -57, -58, -59, -60,            &
     &     -61, -62, -63, -64, -65, -66, -67, -68, -69, -70,            &
     &     -71, -72, -73, -74, -75, -76, -77, -78, -79, -80,            &
     &     -81, -82, -83, -84, -85, -86, -87, -88, -89, -90,            &
     &     -91, -92, -93, -94, -95, -96, -97, -98, -99/
!$OMP CRITICAL (FUNIT)
      IUnit = UNitIni
      DO WHILE ((NUnit(IUnit) .GT. 0) .AND.                             &
     &     (IUnit .LT. 100))
         IUnit = IUnit + 1
      ENDDO
      IF (IUnit .LT. 100) THEN
!     NUnit(IUnit) < 0.
         NUnit(IUnit) = -NUnit(IUnit)
         GetUnit = NUnit(IUnit)
      ELSE
         WRITE(6, *) 'getunit: Todas las unidades estas ocupadas.'
         GetUnit = 0
         STOP 1
      ENDIF
!$OMP END CRITICAL (FUNIT)
      RETURN
      END
!
      SUBROUTINE FreeUnit(IUnit)
      INCLUDE 'pxp.fpp'
      INTEGER IUnit
      INTEGER NUnit(99)
      COMMON /UNITNUM/NUnit
!$OMP CRITICAL (FUNIT)
!     PELIGRO, WILL ROBINSON, PELIGRO!(10)
!     Ojo con el archivo pxp.fpp.
      IF ((IUnit .GE. UnitIni) .AND.                                    &
     &     (UnitIni .LT. 100)) THEN
!     Borrame.
         IF (NUnit(IUnit) .LT. 0) THEN
            PRINT*, 'freeunit: Error.'
            STOP 1
         ENDIF
!     Borrame.
         Nunit(IUnit) = -NUnit(IUnit)
      ENDIF
!$OMP END CRITICAL (FUNIT)
      RETURN
      END
!
      INTEGER FUNCTION Abrir(FileName, CStatus, CAccess, ULog)
      IMPLICIT NONE
      EXTERNAL GetUnit
      INTEGER GetUnit
      CHARACTER*(*) CStatus
      CHARACTER*(*) CAccess
      CHARACTER*(*) FileName
      CHARACTER*128 CadMay
      CHARACTER*68 PathDat
      CHARACTER*80 fconcat
      CHARACTER*80 PathName
      INTEGER IUnit
      INTEGER ULog
      COMMON/CPathName/PathDat
!     Lee Path Dato
!*************
      IF (PathDat .EQ. '.') THEN
         PathName = FileName
      ELSE
         PathName = fconcat(PathDat, FileName)
      ENDIF
      IUnit = GetUnit()
      OPEN(UNIT = IUnit, FILE = PathName, STATUS = CStatus,             &
     &     ACCESS = CAccess, ERR = 10)
      CALL UpperCase(CAccess, CadMay)
      IF (CadMay .EQ. 'SEQUENTIAL') THEN
         REWIND(IUnit)
      ENDIF
      Abrir = IUnit
      RETURN
   10 Abrir = 0
      IF (ULog .gt. 0) THEN
         WRITE(ULog, '(A, A)') &
     &        'abrir: Hubo problemas al abrir el archivo ',  &
     &        Filename
         ENDIF
      RETURN
      END
!
      INTEGER FUNCTION AbrirScratch(LRec, ULog)
      INCLUDE 'pxp.fpp'
      EXTERNAL GetUnit
      INTEGER GetUnit
      INTEGER IUnit
      INTEGER LRec
      INTEGER ULog
!     Lee Path Dato
!*************
      IUnit = GetUnit()

      OPEN(UNIT = IUnit, STATUS = 'SCRATCH',              &
     &     ACCESS = 'DIRECT', RECL = LRec, ERR = 10)
      AbrirScratch = IUnit      
      RETURN
   10 AbrirScratch = 0

      WRITE(ULog, '(A, A)') &
     &     'abrirdirect: Hubo problemas unidad scratch'


      END
!
      INTEGER FUNCTION AbrirDirecto(FileName, CStatus, LRec, Convert, ULog)
      INCLUDE 'pxp.fpp'
      EXTERNAL GetUnit
      INTEGER GetUnit
      CHARACTER*(*) Convert
      CHARACTER*(*) CStatus
      CHARACTER*(*) FileName
      CHARACTER*68 PathDat
      CHARACTER*80 fconcat
      CHARACTER*80 PathName
      INTEGER IUnit
      INTEGER LRec
      INTEGER ULog
      COMMON/CPathName/PathDat
!     Lee Path Dato
!*************
      IF (PathDat .EQ. '.') THEN
         PathName = FileName
      ELSE
         PathName = fconcat(PathDat, FileName)
      ENDIF
      IUnit = GetUnit()

      OPEN(UNIT = IUnit, STATUS = CStatus, FILE = PathName,             &
     &     ACCESS = 'DIRECT', RECL = LRec, CONVERT = Convert,           &
     &     ERR = 10)
      AbrirDirecto = IUnit      
      RETURN
   10 AbrirDirecto = 0

      WRITE(ULog, '(A, A)') &
     &     'abrirdirect: Hubo problemas al abrir el archivo', &
     &     Filename

      END
!
      SUBROUTINE Cerrar(IUnit)
      INCLUDE 'pxp.fpp'
      INTEGER IUnit
      CLOSE(IUnit)
      CALL FreeUnit(IUnit)
      RETURN
      END
!*********************
!     Subrutina LeePathName
!*********************
      SUBROUTINE LeePath
      INCLUDE 'pxp.fpp'
      CHARACTER*68 PathDat
      INTEGER URead
      CHARACTER*12 AuxVar
      COMMON /CPathName/PathDat
!************************
!     Lee Datos de Path.Dat
!************************
      URead = 7
      OPEN(URead, FILE = NArcUStop, STATUS = 'UNKNOWN')
      REWIND(URead)
      WRITE(URead, '(A)') 'N'
      CLOSE (URead)
      OPEN(URead, FILE = NArcPath, STATUS = 'OLD', ERR = 20)
      REWIND(URead)
      READ(URead, '(A12)') AuxVar
      READ(URead, '(A12)') AuxVar
      READ(URead, '(A68)') PathDat
      CLOSE (URead)
      RETURN
   20 PathDat = '.'
      RETURN
      END

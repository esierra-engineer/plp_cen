      SUBROUTINE GraDatBDCen0(UPreSuf)

      INTEGER UPreSuf

      WRITE(UPreSuf, '(A)') '"powerPlantFieldNames"'
      WRITE(UPreSuf, '(4A)')                                            &
     &     '"CenQgen [m3/s]","Potencia [MW]","Energia [GWh]",',         &
     &     '"Inyeccion [US$/h]","Inyeccion [kUS$]",',                   &
     &     '"CostoOp [kUS$]"'
      WRITE(UPreSuf, '(A)') '"powerPlantFormat"'
      WRITE(UPreSuf, '(4A)')                                            &
     &     '"%10.2f","%10.2f","%10.2f",',                               &
     &     '"%10.2f","%10.2f",',                                        &
     &     '"%10.2f"'
      WRITE(UPreSuf, '(A)') '"powerPlantPrefixNames"'
      WRITE(UPreSuf, '(A)')                                             &
     & '"Central","Simulacion","Bloque","Numero Central"'
      
      END

!***************************************
!     Graba Archivo Generacion Centrales
!***************************************
      SUBROUTINE GraDatBDCen2(NBloques, BloDur,         &
     &     NCentral, CenTipo,                        &
     &     CenGBar, CenPGen, CMg,               &
     &     RenCen, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

!
      CHARACTER*12 STipoCen
      CHARACTER*1 CenTipo(Dim%Cen)
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CMg(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION RenCen(Dim%Cen, Dim%Blo)
      INTEGER CenGBar(Dim%Cen)
      INTEGER fPosChar
      INTEGER IBlo
      INTEGER ICen
      INTEGER IREC
      INTEGER LRECL
      INTEGER NBloques
      INTEGER NCentral
      INTEGER UWrite
      CHARACTER*4 InvRD
      DATA IREC /0/
      DOUBLE PRECISION D0
      DOUBLE PRECISION D1
      DOUBLE PRECISION D2
      DOUBLE PRECISION D3
      DOUBLE PRECISION D4
      INTEGER AbrirDirecto
      EXTERNAL AbrirDirecto

      STipoCen = PCenTipEmb//PCenTipTer//PCenTipPas//PCenTipSer//       &
     &     PCenTipFal//PCenTipMod//PCenTipBat
      STipoCen(8:8) = Char(0)

      LRECL = 20
      UWrite = AbrirDirecto('plpcen.res', 'UNKNOWN', LRECL, 'NATIVE', ULog)
      
      DO IBlo = 1, NBloques
         DO ICen = 1, NCentral
            IF(fPosChar(CenTipo(ICen), STipoCen) .GT. 0) THEN
               IREC = IREC + 1
               D0 = CenPGen(ICen, IBlo)
               D1 = CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)
               D2 = CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)*             &
     &              BloDur(IBlo)/1000d0
               D3 = CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)*             &
     &              CMg(CenGBar(ICen), IBlo)
               D4 = CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)*             &
     &              BloDur(IBlo)/1000d0*CMg(CenGBar(ICen), IBlo)
               WRITE(UWrite, REC = IREC)                                &
     &              InvRD(D0),                                          &
     &              InvRD(D1),                                          &
     &              InvRD(D2),                                          &
     &              InvRD(D3),                                          &
     &              InvRD(D4)
            ENDIF
         ENDDO
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END

      INTEGER FUNCTION InvII(IX)
      IMPLICIT NONE
      INTEGER IX
      INTEGER IY
      CHARACTER CY(4)
      CHARACTER CT
      EQUIVALENCE (IY, CY)
      IY = IX
      CT = CY(4)
      CY(4) = CY(1)
      CY(1) = CT
      CT = CY(3)
      CY(3) = CY(2)
      CY(2) = CT
      InvII = IY
      RETURN
      END
      FUNCTION InvRD(DX)
      IMPLICIT NONE
      DOUBLE PRECISION DX
      REAL RY
      CHARACTER*4 CY
      CHARACTER*4 InvRD
      CHARACTER CT
      EQUIVALENCE (RY, CY)
      RY = REAL(DX)
      CT = CY(4:4)
      CY(4:4) = CY(1:1)
      CY(1:1) = CT
      CT = CY(3:3)
      CY(3:3) = CY(2:2)
      CY(2:2) = CT
      InvRD = CY
      RETURN
      END

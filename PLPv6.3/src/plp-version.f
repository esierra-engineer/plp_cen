      SUBROUTINE Version(IUnit)
      IMPLICIT NONE
      INTEGER IUnit
      WRITE(IUnit, *)
      WRITE(IUnit, '(A)') '            PLP v6.3 BATERIAS          '
      WRITE(IUnit, '(A)') '    Gerencia de Operacion     '
      WRITE(IUnit, '(A)') 'COORDINADOR ELECTRICO NACIONAL'
      WRITE(IUnit, '(A)') '     $Date: 2023-12-13 $'
      WRITE(IUnit, '(A)') '------------------------------'
      RETURN
      END

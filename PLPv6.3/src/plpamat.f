MODULE A_MATRIX
  USE ISO_C_BINDING

#if 1
  USE amat

  TYPE AMATRIX
     INTEGER(c_size_t) m
     INTEGER nz
     INTEGER ncols
     INTEGER nfilas
  END TYPE AMATRIX

CONTAINS

  ! Initialize
  SUBROUTINE Am_init(A, ncols, nfilas)
    TYPE(AMatrix) :: A
    INTEGER ncols
    INTEGER nfilas

    A%m = amat_new()
    A%nz = 0
    A%ncols = ncols
    A%nfilas = nfilas

  END SUBROUTINE Am_init

  ! Close
  SUBROUTINE Am_close(A)
    TYPE(AMatrix) :: A

    CALL amat_delete(A%m)

  END SUBROUTINE Am_close


  ! set element
  SUBROUTINE Am_set(A, icol, irow, val)
    TYPE(AMatrix) :: A
    INTEGER icol
    INTEGER irow
    DOUBLE PRECISION val

    IF (val .ne. 0.0d0) THEN
       CALL amat_set(A%m, icol, irow, val)
       A%nz = A%nz + 1
    ENDIF

  END SUBROUTINE Am_set

  ! get element
  DOUBLE PRECISION FUNCTION Am_get(A, icol, irow)
    TYPE(AMatrix) :: A
    INTEGER icol
    INTEGER irow

    Am_get = amat_get(A%m, icol, irow)
  END FUNCTION Am_get

  ! flat version
  SUBROUTINE Am_flat(A, eps, matbeg, matind, matval, nnzero)
    TYPE(AMatrix) :: A
    DOUBLE PRECISION eps
    INTEGER matbeg(A%ncols + 1)
    INTEGER matind(A%nz)
    DOUBLE PRECISION matval(A%nz)
    INTEGER nnzero

    CALL amat_flat(A%m, A%ncols, eps, matbeg, matind, matval)
    NNZero = matbeg(A%ncols + 1)

  END SUBROUTINE Am_flat

#else

  TYPE AMatrix
     DOUBLE PRECISION, ALLOCATABLE :: m(:,:)
     INTEGER nz
     INTEGER ncols
     INTEGER nfilas
  END TYPE AMatrix

CONTAINS

  SUBROUTINE Am_init(A, ncols, nfilas)
    TYPE(AMatrix) :: A
    INTEGER ncols
    INTEGER nfilas

    A%nz = 0
    A%ncols = ncols
    A%nfilas = nfilas

    ALLOCATE(A%m(ncols, nfilas))
    A%m(1:ncols, 1:nfilas) = 0.0d0

  END SUBROUTINE Am_init

  SUBROUTINE Am_close(A)
    TYPE(AMatrix) :: A

    DEALLOCATE(A%m)

  END SUBROUTINE Am_close


  SUBROUTINE Am_set(A, icol, irow, val)

    TYPE(AMatrix) :: A
    INTEGER icol
    INTEGER irow
    DOUBLE PRECISION val

    IF (val .ne. 0.0d0) THEN
       A%m(icol, irow) = val
       A%nz = A%nz + 1
    ENDIF

  END SUBROUTINE Am_set

  DOUBLE PRECISION FUNCTION Am_get(A, icol, irow)

    TYPE(AMatrix) :: A
    INTEGER icol
    INTEGER irow

    Am_get = A%m(icol, irow)

  END FUNCTION Am_get


  SUBROUTINE Am_flat(A, eps, matbeg, matind, matval, nnzero)

    TYPE(AMatrix) A
    DOUBLE PRECISION eps
    INTEGER matbeg(A%ncols + 1)
    INTEGER matind(A%nz)
    DOUBLE PRECISION matval(A%nz)
    INTEGER nnzero

    LOGICAL DxAEQy

    INTEGER ICol, IFila

    NNZero = 0
    DO ICol = 1, A%ncols
       matbeg (ICol) = NNZero
       DO IFila = 1, A%nfilas
          IF (A%m (ICol, IFila) .NE. 0.0d0) THEN
             IF (.NOT. DxAEQy (A%m (ICol, IFila), 0.0d0, eps))   &
                  &              THEN
                NNZero = NNZero + 1
                matval (NNZero) = A%m (ICol, IFila)
                matind (NNZero) = IFila - 1
             ENDIF
          ENDIF
       ENDDO
    ENDDO
    matbeg(A%ncols + 1) = NNZero


  END SUBROUTINE Am_flat


#endif

END MODULE A_MATRIX


module amat
  use iso_c_binding

  !
  !
  !


  interface
     subroutine amat_test() bind(c) 
     end subroutine amat_test

     function amat_new() bind(c) result (venv)
       import :: c_size_t
       integer(c_size_t) :: venv
     end function amat_new

     subroutine amat_delete(vap) bind(c) 
       import :: c_size_t
       integer(c_size_t), value :: vap
     end subroutine amat_delete
     
     subroutine amat_size(vap, nnzero, ncols) bind(c) 
       import :: c_size_t, c_int
       integer(c_size_t), value :: vap
       integer(c_int) :: nnzero
       integer(c_int) :: ncols
     end subroutine amat_size

     subroutine amat_set(vap, row, col, val) bind(c) 
       import :: c_size_t, c_int, c_double
       integer(c_size_t), value :: vap
       integer(c_int), value :: row
       integer(c_int), value :: col
       real(c_double), value :: val
     end subroutine amat_set


     function amat_get(vap, row, col) bind(c) result(val)
       import :: c_size_t, c_int, c_double
       real(c_double) :: val
       integer(c_size_t), value :: vap
       integer(c_int), value :: row
       integer(c_int), value :: col
     end function amat_get
     
     subroutine amat_flat(vap, ncol, eps, matbeg, matind, matval) bind(c) 
       import :: c_size_t, c_int, c_double
       integer(c_size_t), value :: vap
       integer(c_int), value :: ncol
       real(c_double), value :: eps
       integer(c_int) :: matbeg(*)
       integer(c_int) :: matind(*)
       real(c_double) :: matval(*)
     end subroutine amat_flat
  end interface

end module amat

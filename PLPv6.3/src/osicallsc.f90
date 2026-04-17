module osi
  use iso_c_binding

  !
  ! parameters
  !

  !/** Whether to do a presolve in initialSolve */
  INTEGER OsiDoPresolveInInitial
  PARAMETER (OsiDoPresolveInInitial = 0)
  !/** Whether to use a dual algorithm in initialSolve.
  !    The reverse is to use a primal algorithm */
  INTEGER OsiDoDualInInitial
  PARAMETER (OsiDoDualInInitial = 1)
  !/** Whether to do a presolve in resolve */
  INTEGER OsiDoPresolveInResolve
  PARAMETER (OsiDoPresolveInResolve = 2)
  !/** Whether to use a dual algorithm in resolve.
  !    The reverse is to use a primal algorithm */
  INTEGER OsiDoDualInResolve
  PARAMETER (OsiDoDualInResolve = 3)
  !/** Whether to scale problem */
  INTEGER OsiDoScale
  PARAMETER (OsiDoScale = 4)


  INTEGER OSI_STAT_OPTIMAL
  INTEGER OSI_STAT_INFEASIBLE
  INTEGER OSI_STAT_UNKNOWN

  PARAMETER (OSI_STAT_OPTIMAL = 1)
  PARAMETER (OSI_STAT_INFEASIBLE = 3)
  PARAMETER (OSI_STAT_UNKNOWN = -1)

  !
  ! new/delete methods
  !

  interface
     subroutine osi_start(tty_stat) bind(c) 
       import :: c_int
       integer(c_int) :: tty_stat
     end subroutine osi_start

     subroutine osi_stop() bind(c) 
     end subroutine osi_stop


     function osi_env_new() bind(c) result (venv)
       import :: c_size_t
       integer(c_size_t) :: venv
     end function osi_env_new

     subroutine osi_env_delete(venv) bind(c) 
       import :: c_size_t
       integer(c_size_t), value :: venv
     end subroutine osi_env_delete

     subroutine osi_env_setepopt(venv, epopt) bind(c) 
       import :: c_size_t, c_double
       integer(c_size_t), value :: venv
       real(c_double), value :: epopt
     end subroutine osi_env_setepopt

     subroutine osi_env_seteprhs(venv, eprhs) bind(c) 
       import :: c_size_t, c_double
       integer(c_size_t), value :: venv
       real(c_double), value :: eprhs
     end subroutine osi_env_seteprhs

     subroutine osi_env_setmemmode(venv, memmode) bind(c) 
       import :: c_size_t, c_int
       integer(c_size_t), value :: venv
       integer(c_int), value :: memmode
     end subroutine osi_env_setmemmode

  end interface

  !
  ! LP
  !

  interface
     function osi_new_env(epopt, eprhs) bind(c) result (venv)
       import :: c_size_t, c_double
       integer(c_size_t) :: venv
       real(c_double), value :: epopt
       real(c_double), value :: eprhs
     end function osi_new_env

     function osi_lp_new(venv) bind(c) result (vlp)
       import :: c_size_t
       integer(c_size_t) :: vlp
       integer(c_size_t), value :: venv
     end function osi_lp_new

     function osi_lp_loadproblem(venv, probname, numcols, numrows, objsen, start,& 
          & index, values, collb, colub, obj, rowsen, rowrhs, colname) bind(c) result (vlp)
       import :: c_size_t, c_double, c_int, c_char
       integer(c_size_t) :: vlp
       integer(c_size_t), value :: venv
       character(c_char) :: probname(*)
       integer(c_int), value :: numcols 
       integer(c_int), value :: numrows 
       integer(c_int), value :: objsen
       integer(c_int) :: start(*)
       integer(c_int) :: index(*)
       real(c_double) :: values(*)
       real(c_double) :: collb(*)
       real(c_double) :: colub(*)
       real(c_double) :: obj(*)
       character(c_char) :: rowsen(*)
       real(c_double) :: rowrhs(*)
       integer(c_size_t) :: colname(*)
     end function osi_lp_loadproblem

     subroutine osi_lp_delete(vlp) bind(c) 
       import :: c_size_t
       integer(c_size_t), value :: vlp
     end subroutine osi_lp_delete
  end interface

  !
  ! solve methods
  !

  interface
     subroutine osi_lp_initialsolve(vlp) bind(c)
       import :: c_size_t
       integer(c_size_t), value :: vlp
     end subroutine osi_lp_initialsolve

     subroutine osi_lp_dualopt(vlp, presolve) bind(c)
       import :: c_size_t, c_bool
       logical(c_bool), value :: presolve
       integer(c_size_t), value :: vlp
     end subroutine osi_lp_dualopt

     subroutine osi_lp_primopt(vlp, presolve) bind(c)
       import :: c_size_t, c_bool
       logical(c_bool), value :: presolve
       integer(c_size_t), value :: vlp
     end subroutine osi_lp_primopt

     subroutine osi_lp_dualiniopt(vlp, presolve) bind(c)
       import :: c_size_t, c_bool
       logical(c_bool), value :: presolve
       integer(c_size_t), value :: vlp
     end subroutine osi_lp_dualiniopt

     subroutine osi_lp_priminiopt(vlp, presolve) bind(c)
       import :: c_size_t, c_bool
       logical(c_bool), value :: presolve
       integer(c_size_t), value :: vlp
     end subroutine osi_lp_priminiopt

     function osi_lp_isprovenoptimal(vlp) bind(c) result (val)
       import :: c_size_t, c_bool
       logical(c_bool) :: val
       integer(c_size_t), value :: vlp
     end function osi_lp_isprovenoptimal

     function osi_lp_isprovendualinfeasible(vlp) bind(c) result (val)
       import :: c_size_t, c_bool
       logical(c_bool) :: val
       integer(c_size_t), value :: vlp
     end function osi_lp_isprovendualinfeasible

     function osi_lp_isprovenprimalinfeasible(vlp) bind(c) result (val)
       import :: c_size_t, c_bool
       logical(c_bool) :: val
       integer(c_size_t), value :: vlp
     end function osi_lp_isprovenprimalinfeasible

     function osi_lp_getconditionnumber (vlp) bind(c) result (val)
       import :: c_size_t, c_double
       real(c_double) :: val
       integer(c_size_t), value :: vlp
     end function osi_lp_getconditionnumber

     subroutine osi_lp_writelp(vlp, filename, eps) bind(c)
       import :: c_size_t, c_char, c_double
       integer(c_size_t), value :: vlp
       character(c_char) :: filename(*)
       real(c_double), value :: eps
     end subroutine osi_lp_writelp
  end interface

  !
  ! add methods
  !
  interface
     subroutine osi_lp_addcol (vlp, lb, ub, obj, colname) bind(c)
       import :: c_size_t, c_double, c_char
       integer(c_size_t), value :: vlp
       real(c_double), value :: lb
       real(c_double), value :: ub
       real(c_double), value :: obj
       character(c_char) :: colname(*)
     end subroutine osi_lp_addcol

     subroutine osi_lp_addrow(vlp, numberElements, columns,  element, &
          & rowlb, rowub) bind(c) 
       import :: c_size_t, c_double, c_int
       integer(c_size_t), value :: vlp
       integer(c_int), value :: numberElements
       integer(c_int) :: columns(*) 
       real(c_double) :: element(*)
       real(c_double), value :: rowlb
       real(c_double), value :: rowub
     end subroutine osi_lp_addrow

     subroutine osi_lp_deleterows(vlp, number, rowIndices) bind(c)
       import :: c_size_t, c_int
       integer(c_size_t), value :: vlp
       integer(c_int), value :: number
       integer(c_int) :: rowIndices(*) 
     end subroutine osi_lp_deleterows
  end interface

  !
  ! get/set methods
  !

  interface
     function osi_lp_getnumcols(vlp) bind(c) result (val)
       import :: c_size_t, c_int
       integer(c_int) :: val
       integer(c_size_t), value :: vlp
     end function osi_lp_getnumcols

     function osi_lp_getnumrows(vlp) bind(c) result (val)
       import :: c_size_t, c_int
       integer(c_int) :: val
       integer(c_size_t), value :: vlp
     end function osi_lp_getnumrows

     function osi_getinfty() bind(c) result (val)
       import :: c_double
       real(c_double) :: val
     end function osi_getinfty

     function osi_lp_getobjvalue(vlp) bind(c) result (val)
       import :: c_size_t, c_double
       real(c_double) :: val
       integer(c_size_t), value :: vlp
     end function osi_lp_getobjvalue


     function osi_lp_getextremeray(vlp, ray, rhs) bind(c) result(res)
       import :: c_size_t, c_double, c_int
       integer(c_size_t), value :: vlp
       real(c_double) :: ray(*)
       real(c_double) :: rhs
       integer(c_int) :: res
     end function osi_lp_getextremeray

     subroutine osi_lp_sethintparam(vlp, key, yesNo) bind(c)
       import :: c_size_t, c_int, c_bool
       integer(c_size_t), value :: vlp
       integer(c_int), value :: key
       logical(c_bool), value :: yesNo
     end subroutine osi_lp_sethintparam

     subroutine osi_lp_setcoefficient(vlp, row, col, val) bind(c)
       import :: c_size_t, c_int, c_double
       integer(c_size_t), value :: vlp
       integer(c_int), value :: row
       integer(c_int), value :: col
       real(c_double), value :: val
     end subroutine osi_lp_setcoefficient


#define DEC_OSI_LP_SET_ELEMENT(Name) \
     Name(vlp, index, val) bind(c); \
       import :: c_size_t, c_double, c_int; \
       integer(c_size_t), value :: vlp; \
       integer(c_int), value :: index; \
       real(c_double), value :: val; \

     subroutine DEC_OSI_LP_SET_ELEMENT(osi_lp_setcollower)
     end subroutine 

     subroutine DEC_OSI_LP_SET_ELEMENT(osi_lp_setcolupper)
     end subroutine 

     subroutine DEC_OSI_LP_SET_ELEMENT(osi_lp_setobjcoeff)
     end subroutine 

     subroutine DEC_OSI_LP_SET_ELEMENT(osi_lp_setrowrhs)
     end subroutine 

#define DEC_OSI_LP_GET_VECTOR(Name) \
     Name(vlp, begin, end, values) bind(c); \
       import :: c_size_t, c_double, c_int; \
       integer(c_size_t), value :: vlp; \
       integer(c_int), value :: begin; \
       integer(c_int), value :: end; \
       real(c_double) :: values(*); \

     subroutine DEC_OSI_LP_GET_VECTOR(osi_lp_getrowrhs)
     end subroutine

     subroutine DEC_OSI_LP_GET_VECTOR(osi_lp_getcollower)
     end subroutine

     subroutine DEC_OSI_LP_GET_VECTOR(osi_lp_getcolupper)
     end subroutine

     subroutine DEC_OSI_LP_GET_VECTOR(osi_lp_getcolsol)
     end subroutine

     subroutine DEC_OSI_LP_GET_VECTOR(osi_lp_getredcost)
     end subroutine

     subroutine DEC_OSI_LP_GET_VECTOR(osi_lp_getrowprice)
     end subroutine

     subroutine DEC_OSI_LP_GET_VECTOR(osi_lp_getobjcoeff)
     end subroutine

     subroutine osi_lp_getcolname(vlp, index, size, name) bind(c)
       import :: c_size_t, c_double, c_int
       integer(c_size_t), value :: vlp
       integer(c_int), value :: index
       integer(c_size_t), value :: size
       integer(c_size_t), value :: name
     end subroutine osi_lp_getcolname

     function osi_new_colidxs(vlp, size) bind(c) result (vmap)
       import :: c_size_t, c_int
       integer(c_size_t) :: vmap
       integer(c_size_t), value :: vlp
       integer(c_size_t), value :: size
     end function osi_new_colidxs

     function osi_get_colidx(vci, cname, size) bind(c) result (idx)
       import :: c_size_t, c_int
       integer(c_int) :: idx
       integer(c_size_t), value :: vci
       integer(c_size_t), value :: cname
       integer(c_size_t), value :: size
     end function osi_get_colidx
     
     subroutine osi_delete_colidxs(vci) bind(c) 
       import :: c_size_t
       integer(c_size_t), value :: vci
     end subroutine osi_delete_colidxs
     
     
     subroutine osi_getsolversion(size, name) bind(c)
       import :: c_size_t
       integer(c_size_t) :: size
       integer(c_size_t), value :: name
     end subroutine osi_getsolversion


     function osi_lp_get_feasible_cut(vlpi, vlpo, nrows, rows, cols, objs, fname, eps, dbl, ray, rhs) bind(c) result (res)
       import :: c_size_t, c_double, c_int
       integer(c_int) :: res
       integer(c_size_t), value :: vlpi
       integer(c_size_t), value :: vlpo
       integer(c_int), value :: nrows
       integer(c_size_t), value :: rows
       integer(c_size_t), value :: cols
       integer(c_size_t), value :: objs
       integer(c_size_t), value :: fname
       real(c_double), value :: eps
       integer(c_int), value :: dbl
       integer(c_size_t), value :: ray
       integer(c_size_t), value :: rhs
     end function osi_lp_get_feasible_cut


  end interface



end module osi

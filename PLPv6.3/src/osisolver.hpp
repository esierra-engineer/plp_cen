/*
 * OsiSolver.hpp
 *
 */

#ifndef OSISOLVER_HPP_
#define OSISOLVER_HPP_


#ifdef COIN_USE_OSL
#include "coin/OsiOslSolverInterface.hpp"
#define OSI_SOLVER OSL
typedef OsiOslSolverInterface OsiGenSolverInterface;
#include "ekk_c_api.h"
#endif

#ifdef COIN_USE_CPX
#include "coin/OsiCpxSolverInterface.hpp"
#define OSI_SOLVER CPLEX
typedef OsiCpxSolverInterface OsiGenSolverInterface;
#endif


#ifdef COIN_USE_GRB
#include "coin/OsiGrbSolverInterface.hpp"
#define OSI_SOLVER GUROBI
typedef OsiGrbSolverInterface OsiGenSolverInterface;
#endif


#ifndef OSI_SOLVER
#ifndef COIN_USE_CLP
#define COIN_USE_CLP
#endif
#endif

#ifdef COIN_USE_CLP
#include "coin/OsiClpSolverInterface.hpp"
#define OSI_SOLVER CLP
typedef OsiClpSolverInterface OsiGenSolverInterface;
#endif



#endif /* OSISOLVER_HPP_ */

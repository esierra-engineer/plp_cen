#include "osisolver.hpp"
#include "osicallsc.hpp"

#include <sstream>
#include <algorithm>
#include <map>
#include <cassert>
#include <cmath>
#include <unistd.h>

#include <fenv.h>

namespace {

double infty_solver = COIN_DBL_MAX;
const double OSI_MIN = 1.0;
const double OSI_MAX = -1.0;

const bool prevent_eqset = true;

struct LPCont {
  OsiGenSolverInterface * lp;
  bool scloning;
  std::vector<double> colsol;
  std::vector<double> redcost;
  std::vector<double> rowprice;
  double objvalue;
  double condnumber;
  int isprovenoptimal;
  int isprovendualinfeasible;
  int isprovenprimalinfeasible;


  LPCont(bool pscloning)
    : lp(new OsiGenSolverInterface())
    , scloning(pscloning)
  {
  }

  ~LPCont()
  {
    delete lp;
  }

  void save_sol(OsiSolverInterface * lp)
  {
    isprovenoptimal = lp->isProvenOptimal();
    isprovendualinfeasible = lp->isProvenDualInfeasible();
    isprovenprimalinfeasible = lp->isProvenPrimalInfeasible();

    if (isprovenoptimal) {
      objvalue = lp->getObjValue();
      condnumber = lp->getConditionNumber();

      if (scloning) {
        colsol.assign(lp->getColSolution(), lp->getColSolution() + lp->getNumCols());
        rowprice.assign(lp->getRowPrice(), lp->getRowPrice() + lp->getNumRows());
        redcost.assign(lp->getReducedCost(), lp->getReducedCost() + lp->getNumCols());
      }
    }

    if (lp != this->lp) {
      delete lp;
    }
  }

  OsiSolverInterface *get_lpi()
  {
    OsiSolverInterface * lpi = scloning ? lp->clone() : lp;

    return lpi;
  }

  void initial_solve()
  {
    OsiSolverInterface * lpi = get_lpi();
    lpi->initialSolve();
    save_sol(lpi);
  }

  void resolve()
  {
    OsiSolverInterface * lpi = get_lpi();
    lpi->resolve();
    save_sol(lpi);
  }

  const double * const get_colsol() const
  {
    return !scloning ? lp->getColSolution() : &(colsol[0]);
  }

  const double * const get_rowprice() const
  {
    return !scloning ? lp->getRowPrice() : &(rowprice[0]);
  }

  const double * const get_redcost() const
  {
    return !scloning ? lp->getReducedCost() : &(redcost[0]);
  }

};


LPCont * osi_lp_create(OsiEnv * env,
                       double objsen = OSI_MIN)
{
  LPCont * lp_cont = new LPCont(env->memmode > 0);
  OsiGenSolverInterface * lp = lp_cont->lp;

  lp->setObjSense(objsen);
  if (env) {
    if (env->epopt > 0) {
      lp->setDblParam(OsiDualTolerance, env->epopt);
    }
    if (env->eprhs > 0) {
      lp->setDblParam(OsiPrimalTolerance, env->eprhs);
    }
  }

  infty_solver = lp->getInfinity();

  return lp_cont;
}

}

extern "C" {

  void osi_start(int * tty_stat)
  {
    *tty_stat = isatty(fileno(stdout));
  }

  void osi_stop()
  {
  }

  void osi_getsolversion (size_t * size, char * version)
  {
    OsiGenSolverInterface * lp = new OsiGenSolverInterface();

    std::string n = lp->solverVersion();

    *size = std::min(n.size(), *size);
    std::strncpy(version, n.c_str(), *size);

    delete lp;
  }


  void * osi_env_new()
  {
    OsiEnv * env = new OsiEnv;

    env->epopt = -1;
    env->eprhs = -1;
    env->memmode = -1;

    return env;
  }

  void osi_env_delete(void * venv)
  {
    OsiEnv * env = static_cast<OsiEnv*>(venv);
    delete env;
  }

  void osi_env_setepopt(void * venv, double epopt)
  {
    OsiEnv * env = static_cast<OsiEnv*>(venv);

    env->epopt = epopt;

    return;
  }

  void osi_env_seteprhs(void * venv, double eprhs)
  {
    OsiEnv * env = static_cast<OsiEnv*>(venv);

    env->eprhs = eprhs;

    return;
  }

  void osi_env_setmemmode(void * venv, int memmode)
  {
    OsiEnv * env = static_cast<OsiEnv*>(venv);
    if (memmode == -2) {
      memmode = 1;
#ifdef COIN_USE_CLP
      memmode = 0;
#endif
    }

    env->memmode = memmode;

    return;
  }

  //
  // LP
  //
  void * osi_lp_new(void * venv)
  {
    OsiEnv * env = static_cast<OsiEnv*>(venv);
    return osi_lp_create(env);
  }

  void * osi_lp_loadproblem(void * venv,
                            const char * probname,
                            const int numcols,
                            const int numrows,
                            const int objsen,
                            const int* start,
                            const int* index,
                            const double* value,
                            const double* collb,
                            const double* colub,
                            const double* obj,
                            const char* rowsen,
                            const double* rowrhs,
                            const char **colname)
  {
    OsiEnv * env = static_cast<OsiEnv*>(venv);

    LPCont * lp_cont = osi_lp_create(env, objsen);
    OsiGenSolverInterface * lp = lp_cont->lp;

    bool gennames = env->memmode > 1;
    lp->setIntParam(OsiNameDiscipline,
                    gennames ? 1 : 2);

    const double* rowrng = 0;
    lp->loadProblem(numcols, numrows,
                    start, index,
                    value,
                    collb, colub,
                    obj,
                    rowsen, rowrhs,
                    rowrng);

    lp->setHintParam(OsiDoReducePrint, true);

    if (! gennames) {
      if (colname)
        {
          for (int i = 0; i < numcols; ++i)
            {
              lp->setColName(i, colname[i]);
            }
        }

      for (int i = 0; i < numrows; ++i)
        {
          std::ostringstream os;
          os << "c" << i + 1;
          lp->setRowName(i, os.str());
        }

      lp->setObjName("OBJROW");

      lp->setStrParam(OsiProbName, probname);
    }

    return lp_cont;
  }

  void osi_lp_delete(void * vlp)
  {
    LPCont * lp = static_cast<LPCont*>(vlp);
    delete lp;
  }

  void osi_lp_sethintparam(void * vlp, int key, bool yesNo)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;

    lp->setHintParam(static_cast<OsiHintParam>(key), yesNo);

    return;
  }


  void osi_lp_initialsolve(void * vlp)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);

    return lp_cont->initial_solve();
  }

  void osi_lp_writelp(void * vlp,
                      const char * filename,
                      double eps)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;

    lp->writeLps(filename, "lp");

    //    int numberAcross = 8;
    //    int decimals = 15;
    //    lp->writeLp(filename, "lp", eps, numberAcross, decimals);
  }

  int osi_lp_getnumcols(void * vlp)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;

    return lp->getNumCols();
  }

  int osi_lp_getnumrows(void * vlp)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;

    return lp->getNumRows();
  }


  double osi_getinfty()
  {
    return infty_solver;
  }

  void osi_lp_addcol(void * vlp,
                     const double collb, const double colub,
                     const double obj, const char* c_name)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;

    std::string name(c_name, std::strlen(c_name));

    lp->addCol(0, 0, 0,
               collb, colub,
               obj, name);
  }

  void osi_lp_addrow(void * vlp,
                     const int numberElements,
                     const int *columns,
                     const double *element,
                     const double rowlb,
                     const double rowub)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;

    int i = lp->getNumRows();

    lp->addRow(numberElements,
               columns, element,
               rowlb, rowub);

    std::ostringstream os;
    os << "c" << i + 1;
    lp->setRowName(i, os.str());
  }

  void osi_lp_deleterows(void * vlp,
                         const int num, const int * rowIndices)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;

    lp->deleteRows(num, rowIndices);
  }

  double osi_lp_getcoefficient(void * vlp,
                               int rowIndex, int colIndex)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;

    return lp->getCoefficient(rowIndex, colIndex);
  }

  void osi_lp_setcoefficient(void * vlp,
                             int rowIndex, int colIndex, double value)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;

    if (prevent_eqset && lp->getCoefficient(rowIndex, colIndex) == value) return;
    lp->setCoefficient(rowIndex, colIndex, value);
  }


  void osi_lp_setrowrhs(void * vlp, int i, double rhs)
  {
    OsiGenSolverInterface * lp = static_cast<LPCont*>(vlp)->lp;
    if (prevent_eqset && lp->getRightHandSide()[i] == rhs) return;

    double range = lp->getRowRange()[i];
    char   sense = lp->getRowSense()[i];
    lp->setRowType( i, sense, rhs, range );
  }

  //
  // Get/Set methods
  //

#define IMP_OSI_LP_SET_ELEMENT(Name, SetMethod, GetMethod)              \
  void Name (void * vlp, int elementIndex, double elementValue )        \
  {                                                                     \
    OsiGenSolverInterface * lp =                                        \
      static_cast<LPCont*>(vlp)->lp;                         \
    if (prevent_eqset && lp->GetMethod()[elementIndex] == elementValue) \
      return;                                                           \
    lp->SetMethod(elementIndex, elementValue);                          \
  }



  IMP_OSI_LP_SET_ELEMENT(osi_lp_setcolupper, setColUpper, getColUpper)
  IMP_OSI_LP_SET_ELEMENT(osi_lp_setcollower, setColLower, getColLower)
  IMP_OSI_LP_SET_ELEMENT(osi_lp_setobjcoeff, setObjCoeff, getObjCoefficients)


#define IMP_OSI_LP_GET_VECTOR(Name, Method)       \
  void Name (void * vlp,                          \
             int begin,                           \
             int end,                             \
             double * values)                     \
  {                                               \
    OsiGenSolverInterface * lp =                  \
      static_cast<LPCont*>(vlp)->lp;		\
    const double * iv = lp->Method();             \
    std::copy(iv + begin, iv + end + 1, values);	\
  }

  IMP_OSI_LP_GET_VECTOR(osi_lp_getrowrhs,   getRightHandSide)
  IMP_OSI_LP_GET_VECTOR(osi_lp_getcollower, getColLower)
  IMP_OSI_LP_GET_VECTOR(osi_lp_getcolupper, getColUpper)
  IMP_OSI_LP_GET_VECTOR(osi_lp_getobjcoeff, getObjCoefficients)

#define IMP_OSI_LP_GET_VECTOR_D(Name, Method)     \
  void Name (void * vlp,                          \
             int begin,                           \
             int end,                             \
             double * values)                     \
  {                                               \
    LPCont * lp_cont =                            \
      static_cast<LPCont*>(vlp);                  \
    const double * const iv = lp_cont->Method();  \
    std::copy(iv + begin, iv + end + 1, values);	\
  }

  IMP_OSI_LP_GET_VECTOR_D(osi_lp_getcolsol,   get_colsol)
  IMP_OSI_LP_GET_VECTOR_D(osi_lp_getredcost,  get_redcost)
  IMP_OSI_LP_GET_VECTOR_D(osi_lp_getrowprice, get_rowprice)



  void osi_lp_getcolname (void * vlp,
                          int index,
                          size_t size,
                          char * name)
  {
    OsiGenSolverInterface * lp =
      static_cast<LPCont*>(vlp)->lp;
    std::string n = lp->getColName(index, size);
    size_t ns = n.size();
    std::strncpy(name, n.c_str(), std::min(ns, size));
    for (size_t i = ns; i < size; ++i)
      {
        name[i] = ' ';
      }
  }

  void * osi_new_colidxs (void * vlp, size_t size)
  {
    OsiGenSolverInterface * lp =
      static_cast<LPCont*>(vlp)->lp;

    typedef std::vector<std::string> OsiNameVec ;
    typedef std::map<std::string, int> OsiNameMap ;
    const OsiNameVec & vnames = lp->getColNames();
    OsiNameMap * mnames = new OsiNameMap;

    for (int j = 0; j < vnames.size(); ++j)
      {
        std::string name = vnames[j];
        if (size > 0)
          {
            name.resize(size, ' ');
          }

        (*mnames)[name] = j;
      }
    return mnames;
  }

  int osi_get_colidx (void * vcidx, char * cname, size_t size)
  {
    typedef std::map<std::string, int> OsiNameMap ;
    OsiNameMap * mnames = static_cast<OsiNameMap*>(vcidx);

    std::string name(cname, size);
    std::map<std::string, int>::iterator it = mnames->find(name);
    if (it == mnames->end())
      {
        return -1;
      }
    else
      {
        return it->second;
      }
  }

  void osi_delete_colidxs (void * vcidx)
  {
    typedef std::map<std::string, int> OsiNameMap ;
    OsiNameMap * mnames = static_cast<OsiNameMap*>(vcidx);

    delete mnames;
  }



  double osi_lp_getobjvalue(void * vlp)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);

    return lp_cont->objvalue;
  }



  void osi_lp_dualopt (void * vlp, bool presolve)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);
    OsiGenSolverInterface * lp = lp_cont->lp;

    lp->setHintParam(OsiDoDualInResolve, true);
#ifdef COIN_USE_CLP
    presolve = false;
#endif
    lp->setHintParam(OsiDoPresolveInResolve, presolve);

    lp_cont->resolve();
  }

  void osi_lp_primopt (void * vlp, bool presolve)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);
    OsiGenSolverInterface * lp = lp_cont->lp;

    lp->setHintParam(OsiDoDualInResolve, false);
#ifdef COIN_USE_CLP
    presolve = false;
#endif
    lp->setHintParam(OsiDoPresolveInResolve, presolve);

    lp_cont->resolve();
  }


  void osi_lp_dualiniopt (void * vlp, bool presolve)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);
    OsiGenSolverInterface * lp = lp_cont->lp;

    lp->setHintParam(OsiDoDualInInitial, true);
#ifdef COIN_USE_CLP
    presolve = false;
#endif
    lp->setHintParam(OsiDoPresolveInInitial, presolve);

    lp_cont->initial_solve();
  }

  void osi_lp_priminiopt (void * vlp, bool presolve)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);
    OsiGenSolverInterface * lp = lp_cont->lp;

    lp->setHintParam(OsiDoDualInInitial, false);
#ifdef COIN_USE_CLP
    presolve = false;
#endif
    lp->setHintParam(OsiDoPresolveInInitial, presolve);

    lp_cont->initial_solve();
  }

  bool osi_lp_isprovenoptimal (void * vlp)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);
    return lp_cont->isprovenoptimal;
  }

  bool osi_lp_isprovendualinfeasible (void * vlp)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);
    return lp_cont->isprovendualinfeasible;
  }

  bool osi_lp_isprovenprimalinfeasible (void * vlp)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);
    return lp_cont->isprovenprimalinfeasible;
  }


  double osi_lp_getconditionnumber (void * vlp)
  {
    LPCont* lp_cont = static_cast<LPCont*>(vlp);

    return lp_cont->condnumber;
  }

  int
  osi_lp_get_feasible_cut(void * vlp,
                          void * vlpo,
                          const int nrows,
                          const int *rows,
                          const int *cols,
                          const double *objs,
                          const char * fname,
                          double eps,
                          const int dbl,
                          double * ray, double * rhs)
  {
    LPCont * lp_cont = static_cast<LPCont *>(vlp);
    OsiGenSolverInterface *lp_base = lp_cont->lp;

    LPCont* lp_cont_dest = static_cast<LPCont*>(vlpo);
    OsiGenSolverInterface * lp_dest = lp_cont_dest->lp;

#ifdef COIN_USE_CLP
    OsiSolverInterface *lp = lp_base->clone();
#else
    OsiSolverInterface *lp =
      lp_cont->scloning ? lp_base->clone() : lp_base;
#endif
    lp->setIntParam(OsiNameDiscipline, 1);

    const int ncols = lp->getNumCols();
    int ind_obj[ncols];
    double old_obj[ncols];
    double new_obj[ncols];
    const double *obj = lp->getObjCoefficients();

    for (int i = 0; i < ncols; ++i) {
      ind_obj[i] = i;
      old_obj[i] = obj[i];
      new_obj[i] = 0.0;
    }

    lp->setObjCoeffSet(ind_obj, ind_obj + ncols, new_obj);

    int new_cols[2 * nrows];

    int index = 0;
    for (int i = 0; i < nrows; ++i) {
      const int numberElements = 1;
      const int row = rows[i];
      const double obj = objs ? objs[i] : 1.0;

      double redcost = 0.01 * lp_dest->getReducedCost()[cols[i]];

      double rhs = lp->getRightHandSide()[rows[i]];
      double collb = std::max(rhs - lp_dest->getColLower()[cols[i]], 0.0);
      double colub = std::max(lp_dest->getColUpper()[cols[i]] - rhs, 0.0);

      int sp_col;
      {
        const double element = -1.0;
        std::ostringstream name;
        name << "sp_" << i + 1;

        double robj = redcost > 0 ? obj + redcost : obj;
        lp->addCol(numberElements, &row, &element, 0, colub, robj, name.str());
        new_cols[index] = index + ncols;
        sp_col = new_cols[index];
        ++index;
      }

      int sn_col;
      {
        const double element = 1.0;
        std::stringstream name;
        name << "sn_" << i + 1;

        double robj = redcost < 0 ? obj - redcost : obj;
        lp->addCol(numberElements, &row, &element, 0, collb, robj, name.str());
        new_cols[index] = index + ncols;
        sn_col = new_cols[index];
        ++index;
      }
    }

    if (fname && dbl > 1) {
      lp->writeLps(fname, "lp");
    }

    bool presolve = true;
    lp->setHintParam(OsiDoDualInResolve, true);
#ifdef COIN_USE_CLP
    presolve = false;
#endif
    lp->setHintParam(OsiDoPresolveInResolve, presolve);
    lp->resolve();

    const double aps = eps * 1e-4;
    const double dps = eps * 1e-2;
    int optimal = false;
    if (lp->isProvenOptimal()) {
      const double *dual = lp->getRowPrice();
      const double *b = lp->getRightHandSide();
      const double *x = lp->getColSolution();

      double sc = 0.0;
      for (int i = 0; i < nrows; ++i) {
        sc += std::abs(rows[i]);
      }

      double sum = 0.0;
      for (int i = 0; i < nrows; ++i) {
        const int row = rows[i];
        //	ray[i] = std::abs(dual[row]) < eps*(sc + aps) ? 0.0 :
        //-dual[row];
        ray[i] = std::abs(dual[row]) < eps ? 0.0 : -dual[row];
        int ip = new_cols[i * 2];
        int in = ip + 1;
        double dx = x[ip] - x[in];
        if ((std::abs(b[row]) + 1e-6) * eps > std::abs(dx)) {
          dx = 0.0;
          ray[i] = 0.0;
        }

        if (ray[i] <0){
          ray[i]=-1;
        }
        if (ray[i]> 0){
          ray[i]=1;
        }
        const double nx = b[row] + dx;

        //	const int col = cols[i];
        //	const double collb = lp_dest->getColLower()[col];
        //	const double colub = lp_dest->getColUpper()[col];
        //
        //	if ((ray[i] > 0 && (nx - collb) < dps*(std::abs(collb) +
        //std::abs(nx) + aps) )
        //	    || (ray[i] < 0 && (colub - nx) < dps*(std::abs(colub) +
        //std::abs(nx) + aps))) { 	  ray[i] = 0.0;
        //	}

        rhs[i] = nx * ray[i];
        sum += rhs[i];
      }
      rhs[nrows] = sum;

      optimal = true;
    }
    if (lp != lp_base) {
      delete lp;
    } else {
      lp->deleteCols(index, new_cols);
      lp->setObjCoeffSet(ind_obj, ind_obj + ncols, old_obj);
    }

    return optimal;
  }
}

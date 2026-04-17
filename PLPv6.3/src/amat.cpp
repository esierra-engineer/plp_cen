#include "amat.hpp"

#include <iostream>
#include <sstream>
#include <algorithm>
#include <map>
#include <cassert>
#include <cmath>

#include <fenv.h>

extern "C" {


  typedef std::map<int, double> row_t;
  typedef std::map<int, row_t> sparse_t;

  void * amat_new() 
  {
    return new sparse_t;
  }

  void amat_delete(void * vap) 
  {
    sparse_t * Ap = static_cast<sparse_t*>(vap);
    delete Ap;
  }


  void amat_set(void * vap, 
		int colIndex, 
		int rowIndex, 
		double value)
  {
    if (value == 0.0) return;

    sparse_t * Ap = static_cast<sparse_t*>(vap);

    sparse_t & A = *Ap;


    A[colIndex][rowIndex] = value;
    
  }

  double amat_get(void * vap, 
		  int colIndex, 
		  int rowIndex)
  {
    sparse_t * Ap = static_cast<sparse_t*>(vap);
    
    sparse_t & A = *Ap;
    
    sparse_t::const_iterator it = A.find(colIndex);

    if (it == A.end()) return 0.0;

    row_t::const_iterator jt = it->second.find(rowIndex);

    if (jt == it->second.end()) return 0.0;

    return jt->second;
  }

  void amat_size(void * vap, int * nnzerop, int * ncolsp)
  {
    *nnzerop = 0;
    *ncolsp = 0;

    sparse_t * Ap = static_cast<sparse_t*>(vap);

    if (!Ap) return;

    int nnzero = 0;
    int ncols = 0;
    for (sparse_t::const_iterator it = Ap->begin(); it != Ap->end(); ++it)  {
      nnzero += it->second.size();
      ncols = it->first;
    }

    *nnzerop = nnzero;
    *ncolsp = ncols;
  }

  void amat_test()
  {
    void * vap = amat_new();

    amat_set(vap, 1, 1, 1.0);
    amat_set(vap, 1, 2, 1.0);
    amat_set(vap, 1, 3, 1.0);
    amat_set(vap, 4, 2, 2.0);
    amat_set(vap, 4, 4, 2.0);
    
    int nnzero;
    int ncols;
    amat_size(vap, &nnzero, &ncols);
    std::cout <<  "size " << nnzero << " " << ncols << std::endl;

    ++ncols;

    int * matbeg = new int[ncols + 1];
    int * matind = new int[nnzero];
    double * matval = new double[nnzero];

    double eps = 1.0e-6;
    amat_flat(vap, ncols, eps, matbeg, matind, matval);

    for (int i = 0; i <= ncols; ++i)
      {
	std::cout << "mb " << i <<  " " << matbeg[i] << std::endl;
      }
    std::cout << std::endl;

    for (int i = 0; i < nnzero; ++i)
      {
	std::cout << "miv " << i <<  " " << matind[i] << " " << matval[i] << std::endl;
      }
    std::cout << std::endl;    
  }


  void amat_flat(void * vap,
		 int ncols,
		 double eps,
		 int * matbeg,
		 int * matind, 
		 double * matval)
  {
    int nnzero = 0;
    int ncolsi = 0;
    amat_size(vap, &nnzero, &ncolsi);
    
    if (!nnzero || !ncolsi) return;
    
    assert(ncolsi <= ncols);
    
    sparse_t * Ap = static_cast<sparse_t*>(vap);
    sparse_t & A = *Ap;
  
    sparse_t::const_iterator it = A.begin();
    int ii = 0;
    int ic = 0;

    for (; ic < ncols && it != A.end(); ++ic, ++it) {
      do {
	matbeg[ic] = ii;
      } while (it->first - 1 != ic && ++ic < ncols);
      
      assert(ic < ncols);

      for (row_t::const_iterator jt = it->second.begin(); jt != it->second.end(); ++jt) {
	double aij = jt->second;
	if (std::abs(aij) < eps) continue;

	matind[ii] = jt->first - 1;
	matval[ii] = aij;
	++ii;
      }
    }

    for (; ic < ncols; ++ic) {
      matbeg[ic] = ii;
    }

    assert(ii <= nnzero);

    matbeg[ncols] = nnzero;
  }

}

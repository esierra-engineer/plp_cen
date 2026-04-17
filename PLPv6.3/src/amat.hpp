#ifndef AMAT_H
#define AMAT_H

extern "C" {

  //
  // A Matrix
  //

  void amat_test();
  
  void * amat_new();

  void amat_delete(void * vap);

  void amat_size(void * vap, 
		 int * nnzerop, 
		 int * ncolsp);

  void amat_set(void * vap, 
		int rowIndex, 
		int colIndex, 
		double value);

  double amat_get(void * vap, 
		  int rowIndex, 
		  int colIndex);

  void amat_flat(void * vap,
		 int ncols,
		 double eps,
		 int * matbeg,
		 int * matind, 
		 double * matval);

}

#endif

#ifndef OSICALLSC_H
#define OSICALLSC_H

extern "C" {

  //
  // Osi
  //


  void osi_getsolversion (size_t * size, char * name);
  
  //
  // Env
  //
  struct OsiEnv {
    double epopt;
    double eprhs;
    int memmode;
  };

  
  void * osi_env_new();  
  void osi_env_delete(void * vosi);

  void osi_env_setepopt(void * venv, double epopt);
  void osi_env_seteprhs(void * venv, double eprhs);
  void osi_env_setmemmode(void * venv, int memmode);

  //
  //  LP
  //

  void * osi_lp_new(void * venv);
  void osi_lp_delete(void * vlp);

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
			    const char **colname);

  void osi_lp_initialsolve(void * vlp);

  void osi_lp_addrow(void * vosi,
		  const int numberElements,
		  const int *columns, 
		  const double *element,
		  const double rowlb, 
		  const double rowub);

  void osi_lp_deleterows(void * vlp, 
			 const int num, 
			 const int * rowIndices);

  void osi_lp_addcol(void * vlp,
		     const double collb, 
		     const double colub,   
		     const double obj, 
		     const char* c_name);
  

  double osi_getinfty();
  int osi_lp_getnumcols(void * vlp);
  int osi_lp_getnumrows(void * vlp);

  void osi_lp_writelp(void * vlp, 
		      const char * filename,
		      double eps);

  void osi_lp_sethintparam(void * vosi, int key, bool yesNo);

#define DEC_OSI_LP_SET_ELEMENT(Name)			\
  void Name (void * vosi, int index, double value) 

  DEC_OSI_LP_SET_ELEMENT(osi_lp_setobjcoeff);
  DEC_OSI_LP_SET_ELEMENT(osi_lp_setcollower);
  DEC_OSI_LP_SET_ELEMENT(osi_lp_setcolupper);
  DEC_OSI_LP_SET_ELEMENT(osi_lp_setrowrhs);

#define DEC_OSI_LP_GET_VECTOR(Name)				\
  void Name (void * vosi, int begin, int end, double * values)

  DEC_OSI_LP_GET_VECTOR(osi_lp_getrowrhs);
  DEC_OSI_LP_GET_VECTOR(osi_lp_getcollower);
  DEC_OSI_LP_GET_VECTOR(osi_lp_getcolupper);
  DEC_OSI_LP_GET_VECTOR(osi_lp_getcolsol);
  DEC_OSI_LP_GET_VECTOR(osi_lp_getredcost);
  DEC_OSI_LP_GET_VECTOR(osi_lp_getrowprice);
  DEC_OSI_LP_GET_VECTOR(osi_lp_getobjcoeff);


  void osi_lp_getcolname (void * vlp,					
			  int index,
			  size_t size,						
			  char * name);

  void * osi_new_colidxs (void * vlp, size_t size);
  int osi_get_colidx (void * vcidx, char * cname, size_t size);
  
  void osi_delete_colidxs (void * vcidx);
  
  double osi_lp_getobjvalue(void * vlp);

  int osi_lp_getextremeray(void * vlp,					
			      double * ray,
			   double * rhs);

  void osi_lp_dualopt (void * vlp, bool presolve);
  void osi_lp_primopt (void * vlp, bool presolve);

  void osi_lp_dualiniopt (void * vlp, bool presolve);
  void osi_lp_priminiopt (void * vlp, bool presolve);

  double osi_lp_getconditionnumber (void * vlp);


  // ***
  int 
  osi_lp_get_feasible_cut(void * vlpi,
			  void * vlpo,
			  const int nrows, 
			  const int *rows, 
			  const int *cols, 
			  const double *objs,
			  const char * fname,
			  double eps,
			  const int dbl,
			  double * ray, double * rhs);
  
}

#endif

#include <stdlib.h>
#include "global.h"

/* non-optimized, non-threaded DSCAL replacement */
void pmrrr_dscal(PMRRR_Int *n, double *alpha, double *restrict x, PMRRR_Int *incx)
{
  PMRRR_Int i;
  PMRRR_Int stride = *incx;
  PMRRR_Int size   = *n;
  double s   = *alpha;

  for (i=0; i<size; i++)
    x[i * stride] *= s; 

  return;
}

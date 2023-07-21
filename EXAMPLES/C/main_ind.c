/*
 * Example file for using PMRRR to compute all eigenpairs.
 * Example run: mpiexec -np 4 -env PMR_NUM_THREADS 2 ./main_all.x
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>      /* ceil */
#include <assert.h>
#include "mpi.h"
#include "pmrrr.h"

static PMRRR_Int  read_tri_mat(char*, double**, double**);
static void print_vector(char*, double*, char*, PMRRR_Int);
static void print_matrix(char*, double*, PMRRR_Int, PMRRR_Int, PMRRR_Int);


int main(int argc, char **argv)
{
  /* Input parameter to 'pmrrr' */
  PMRRR_Int n;              /* Matrix size */
  PMRRR_Int il, iu;         
  PMRRR_Int tryRAC = 1;     /* Try high rel. accuracy */
  double    *D, *E;         /* Diagonal and off-diagonal elements */
  double    vl, vu;         

  double    *W;             /* eigenvalues */
  PMRRR_Int nz;             /* # local eigenvectors */
  PMRRR_Int offset;
  double    *Z;             /* eigenvectors; stored by colums */
  PMRRR_Int ldz;
  PMRRR_Int *Zsupp;         /* eigenvector support */

  /* Others */
  int       pid, nproc, status;
  PMRRR_Int info, i;

  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &status);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  /* Read in data from file, space for D and E will
   * be allocated and needs to be freed at the end */
  n = read_tri_mat("./Wilkinson21.data", &D, &E);

  /* Print input */
  if (pid == 0) {
    printf("\n%% Input matrix:\n\n");
    printf("n = %d;\n", n);
    print_vector("D=[", D, "];", n  );
    print_vector("E=[", E, "];", n-1);
  }

  /* Compute eigenpairs 'il' to 'iu' */
  il = 3;
  iu = 18;

  /* Allocate memory */
  W     = (double *) malloc( n    * sizeof(double) );
  Zsupp = (PMRRR_Int *)    malloc( 2*n  * sizeof(PMRRR_Int)    );

  nz    = (PMRRR_Int) ceil((iu-il+1) / (double) nproc);
  ldz   = n;

  Z     = (double *) malloc((size_t) n * nz * sizeof(double) );

  /* Use MRRR to compute eigenvalues and -vectors */
  info = pmrrr("Vectors", "Index", &n, D, E, &vl, &vu, &il,
	       &iu, &tryRAC, MPI_COMM_WORLD, &nz, &offset, 
	       W, Z, &ldz, Zsupp);
  assert(info == 0);

  /* Possibly communicate eigenvalues */
  /* PMR_comm_eigvals(MPI_COMM_WORLD, &nz, &offset, W); */

  /* Print results */
  for (i=0; i<nproc; i++) {
    MPI_Barrier(MPI_COMM_WORLD);
    if (i == pid) {
      printf("\n\n%% Results of process %d:\n", pid);
      if (i == 0) printf("W = [];");
      print_vector("W = [W;", W, "];", nz);
      print_matrix("Z", Z, n, nz, offset);
      fflush(stdout);
    }
  }

  /* Free allocated memory */
  free(D);
  free(E);
  free(W);
  free(Z);
  free(Zsupp);

  MPI_Finalize();

  return(0);
}




/* 
 * Reads the triadiagonal matrix from a file.
 */
static PMRRR_Int read_tri_mat(char *filename, double **Dp, double **Ep)
{
  PMRRR_Int    i, n;
  FILE   *filedes;

  filedes = fopen(filename, "r");
  assert(filedes != NULL);

  fscanf(filedes, "%d", &n);

  *Dp = (double *) malloc( n * sizeof(double) );
  assert(*Dp != NULL);

  *Ep = (double *) malloc( n * sizeof(double) );
  assert(*Ep != NULL);

  for (i=0; i<n; i++) {
    fscanf(filedes, "%le %le", *Dp+i, *Ep+i);
  }
  (*Ep)[n-1] = 0.0;

  fclose(filedes);

  return(n);
}



static void print_vector(char *pre, double *v, char *post, PMRRR_Int n)
{
  PMRRR_Int i;

  printf("\n%s\n", pre);
  for (i=0; i<n; i++) {
    printf("%.17e", v[i]);
    printf("\n");
  }
  printf("%s\n", post);
}



static void print_matrix(char *name, double *A, PMRRR_Int lda, 
			 PMRRR_Int numcols, PMRRR_Int offset)
{
  PMRRR_Int  j;
  char str1[20], str2[20];

  for (j=1; j<=numcols; j++) {
    str1[0] = '\0';
    strcat(str1,name);
    strcat(str1,"(:,");
    sprintf(str2, "%d",j+offset);
    strcat(str1, str2);
    strcat(str1, ")=[");
    print_vector(str1, &A[(j-1)*lda],"];", lda);
  }
}

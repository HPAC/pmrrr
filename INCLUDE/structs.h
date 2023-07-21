/* Copyright (c) 2010, RWTH Aachen University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or 
 * without modification, are permitted provided that the following
 * conditions are met:
 *   * Redistributions of source code must retain the above 
 *     copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above 
 *     copyright notice, this list of conditions and the following 
 *     disclaimer in the documentation and/or other materials 
 *     provided with the distribution.
 *   * Neither the name of the RWTH Aachen University nor the
 *     names of its contributors may be used to endorse or promote 
 *     products derived from this software without specific prior 
 *     written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL RWTH 
 * AACHEN UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF 
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
 * SUCH DAMAGE.
 *
 * Coded by Matthias Petschow (petschow@aices.rwth-aachen.de),
 * August 2010, Version 0.6
 *
 * This code was the result of a collaboration between 
 * Matthias Petschow and Paolo Bientinesi. When you use this 
 * code, kindly reference a paper related to this work.
 *
 */

#ifndef SSTRUCTS_H
#define SSTRUCTS_H

#include "mpi.h"
#include "global.h"
#include "counter.h"
#include "queue.h"

typedef struct {
  PMRRR_Int              n;
  double *restrict D;
  double *restrict E;
  PMRRR_Int              nsplit;
  PMRRR_Int    *restrict isplit ;
  double           spdiam;
} in_t;

typedef struct {
  PMRRR_Int              n;
  double           *vl;
  double           *vu;
  PMRRR_Int              *il;
  PMRRR_Int              *iu;
  double *restrict W;
  double *restrict Werr;
  double *restrict Wgap;
  PMRRR_Int    *restrict Windex;
  PMRRR_Int    *restrict iblock;
  PMRRR_Int    *restrict iproc;
  double *restrict Wshifted;
  double *restrict gersch;
} val_t;

typedef struct {
  PMRRR_Int              ldz;
  PMRRR_Int              nz;
  double *restrict Z;
  PMRRR_Int    *restrict Zsupp;
  PMRRR_Int    *restrict Zindex;
} vec_t;

typedef struct {
  PMRRR_Int      pid;
  PMRRR_Int      nproc;
  MPI_Comm comm;
  PMRRR_Int      nthreads;
  PMRRR_Int      thread_support;
} proc_t;

typedef struct {
  double split;
  double rtol1;
  double rtol2;
  double pivmin;
} tol_t;

typedef struct {
  PMRRR_Int         num_messages;
  MPI_Request *requests;
  MPI_Status  *stats;
} comm_t;

typedef struct {
  queue_t *r_queue;
  queue_t *s_queue;
  queue_t *c_queue;
} workQ_t;

typedef struct {
  double lambda;
  PMRRR_Int    local_ind;
  PMRRR_Int    block_ind;
  PMRRR_Int    ind;
} sort_struct_t;

typedef struct {
  PMRRR_Int    n;
  double *D;
  double *E;
  double *E2;
  PMRRR_Int    il;
  PMRRR_Int    iu;
  PMRRR_Int    my_il;
  PMRRR_Int    my_iu;
  PMRRR_Int    nsplit;
  PMRRR_Int    *isplit;
  double bsrtol;
  double pivmin;
  double *gersch;
  double *W;
  double *Werr;
  PMRRR_Int    *Windex;
  PMRRR_Int    *iblock;
} auxarg1_t;

typedef struct {
  PMRRR_Int          bl_size;
  double       *D;
  double       *DE2;
  PMRRR_Int          rf_begin;
  PMRRR_Int          rf_end;
  double        *W;
  double        *Werr;
  double        *Wgap;
  PMRRR_Int            *Windex;
  double       rtol1;
  double       rtol2;
  double       pivmin;
  double       bl_spdiam;
} auxarg2_t;

typedef struct {
  PMRRR_Int          tid;
  proc_t       *procinfo;
  val_t        *Wstruct;
  vec_t        *Zstruct;
  tol_t        *tolstruct;
  workQ_t      *workQ;
  counter_t    *num_left;
} auxarg3_t;

#endif

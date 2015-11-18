# Overview #

**pmrrr** is a hybrid distributed-memory implementation the algorithm of Multiple Relatively Robust Representations for the real symmetric tridiagonal eigenproblem. It is used for MRRR-based Hermitian eigensolvers of [Elemental](http://libelemental.org) - an open-source software for distributed-memory dense linear algebra.


# More information #

For more information and related projects, please visit the [HPAC website](http://hpac.rwth-aachen.de).


# Download #

Lastest version of the source code: [pmrrr-0.72.tar.gz](http://hpac.rwth-aachen.de/~petschow/pmrrr-0.72.tar.gz)

A prototyp using extended precision internally to improve accuracy: [pmrrr-extended-0.10.tar.gz](http://hpac.rwth-aachen.de/~petschow/pmrrr-extended-0.10.tar.gz)



# Citing pmrrr #

If you are using the code, please kindly reference [this paper](http://epubs.siam.org/doi/abs/10.1137/110848803):

```
@article{Petschow2013:208,
    author  = "Matthias Petschow and Elmar Peise and Paolo Bientinesi",
    title   = "High-Performance Solvers for Dense Hermitian Eigenproblems",
    journal = "SIAM Journal on Scientific Computing",
    year    = 2013,
    volume  = 35,
    number  = 1,
    pages   = "C1--C22"
}
```




# Citing Elemental's Hermitian Eigensolvers #

If you are using Elemental's Hermitian eigensolvers, please kindly reference [the above paper](http://epubs.siam.org/doi/abs/10.1137/110848803) and additionally [this paper](http://dl.acm.org/citation.cfm?id=2427030):

```
@article{Poulson:2013:ENF:2427023.2427030,
    author = "Poulson, Jack and Marker, Bryan and van de Geijn, Robert A. and Hammond, Jeff R. and Romero, Nichols A.",
    title = "Elemental: A New Framework for Distributed Memory Dense Matrix Computations",
    journal = "ACM Trans. Math. Softw.",
    volume = "39",
    number = "2",
    year = "2013",
    pages = "13:1--13:24",
    articleno = "13",
    url = "http://doi.acm.org/10.1145/2427023.2427030",
    publisher = "ACM",
} 
```

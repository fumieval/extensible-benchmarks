A result on GHC 8.0.2 with Intel(R) Core(TM) i7-6700K CPU @ 4.00GHz:

```
benchmarking rws/extensible
time                 9.844 μs   (9.773 μs .. 9.940 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.886 μs   (9.827 μs .. 10.00 μs)
std dev              265.8 ns   (155.8 ns .. 429.8 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking rws/mtl
time                 810.3 ns   (806.4 ns .. 815.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 816.8 ns   (814.0 ns .. 818.9 ns)
std dev              7.834 ns   (6.160 ns .. 9.731 ns)

benchmarking rws/mtl-RWS
time                 588.2 ns   (586.3 ns .. 590.3 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 587.2 ns   (585.9 ns .. 588.9 ns)
std dev              4.944 ns   (3.939 ns .. 6.946 ns)

benchmarking rws/exteff
time                 128.6 μs   (128.2 μs .. 129.0 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 128.3 μs   (128.0 μs .. 128.6 μs)
std dev              1.086 μs   (937.6 ns .. 1.329 μs)

benchmarking rws/effin
time                 31.02 μs   (30.91 μs .. 31.15 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 31.20 μs   (31.12 μs .. 31.31 μs)
std dev              308.5 ns   (254.0 ns .. 405.5 ns)

benchmarking rws/freer-effects
time                 19.81 μs   (19.74 μs .. 19.88 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 20.26 μs   (19.86 μs .. 21.86 μs)
std dev              2.592 μs   (199.7 ns .. 5.505 μs)
variance introduced by outliers: 91% (severely inflated)
```

# Modified PerformanceAnalytics

Contains two extra functions, SharpeError and AdjustedSharpeError, which calculate the error in the Sharpe and Adjusted Sharpe. All documentation done via roxygen2.

Also contains generic but slow wrapper xtsboot that bootstraps error functions for various metrics in PerfA using sample. The mean and median flags for the sample function are almost exclusively for checking purposes as to whether the function works, if you feel like you do not need them, remove them ( and the flag parameter ) altogether as there are more efficient ways to do the same.

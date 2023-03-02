
# Reproduce the F-value from the original study for anterior reach direction
# Reported F-value = 3.818

ant_pval = 0.025
ant_quantile = 1 - ant_pval

ant_Fval <- qf(ant_quantile, df1=3, df2=19) # GG degrees of freedom assumed - see original study
ant_Fval


# Reproduce the F-value from the original study for postlat reach direction
# Reported F-value = 6.503
pl_pval = 0.0004
pl_quantile = 1 - pl_pval

pl_GG_Fval <- qf(pl_quantile, df1=3, df2=19) # GG degrees of freedom assumed
pl_GG_Fval

pl_Fval <- qf(pl_quantile, df1=3, df2=57) # Sphercity assumed
pl_Fval

# NOTE - these calculations seem quite different from the reported F

# Reproduce the F-value from the original study for postmed reach direction
# Reported F-value = 2.215
pm_pval = 0.059
pm_quantile = 1 - pm_pval

pm_GG_Fval <- qf(pm_quantile, df1=3, df2=19) # GG degrees of freedom assumed
pm_GG_Fval

pm_Fval <- qf(pm_quantile, df1=3, df2=57) # Sphercity assumed
pm_Fval

# NOTE - these calculations seem quite different from the reported F

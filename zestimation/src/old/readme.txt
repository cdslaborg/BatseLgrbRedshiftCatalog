I just figured that in this old version, there was a severe bug, which I just discovered.
In ParaPost_mod.f90, the routine getModelIntOverLogLisoGivenZ() is called, which has an
implicit conditional call to the specific choice of getLogSFR. Therefore, in the old code,
I apparently used the same getLogSFR() for all redshift scenarios.
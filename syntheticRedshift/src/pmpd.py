import paramonte as pm
import os
import numpy as np
from scipy.stats import multivariate_normal
import StarFormationRate
import paramonte as pm


pmpd = pm.ParaDRAM()
pmpd.spec.mpiFinalizeRequested = False
pmpd.spec.sampleSize = 10000
pmpd.spec.domainLowerLimitVec = [0.1]
pmpd.spec.domainUpperLimitVec = [100]

model_names=['H06'                  # model names being use
            ,'L08'
            ,'B10'
            ,'M14'
            ,'M17'
            ,'F18'
            ]
            
fileNamesPrefixList =   [ "LogRate"+model in model_names] # makes list of LogRateModelName to name directory for multiple models

out_dir='../out'

sfr_functions = [ getattr(StarFormationRate,'get'+PrefixNames) for PrefixNames in fileNamesPrefixList] #calls functions within StarFormationRate given model names
               

sfr_functions_len = len(sfr_functions) # length of functions used to determine when mpiFinalizeRequested will close

for counter, model in enumerate(sfr_functions):
    pmpd.spec.outputFileName = out_dir+ "/" + fileNamesPrefixList[counter] + "/" +fileNamesPrefixList[counter]
    if counter == sfr_functions_len-1: pmpd.spec.mpiFinalizeRequested = True
    pmpd.runSampler( ndim = 1              # number of dimensions of the objective function
                    , getLogFunc = model   # the objective function: multivariate normal distribution
                    # NOTE: inputFilePath is optional: all simulation specifications can be set as attributes of pmpd.spec
                    #, inputFilePath = os.path.dirname(os.path.abspath(__file__)) + "/paramonte.in"
                    , mpiEnabled = False
                    )


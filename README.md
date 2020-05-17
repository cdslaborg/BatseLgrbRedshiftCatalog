## BATSE Individual LGRB Redshift Estimation 

This folder contains data, code and output for the estimation of BATSE 1366 LGRBs' redshifts. This is done by feeding the four observed properties of BATSE LGRBs:  

1. The observed 1024ms bolometric peak brightness (Pbol)  
2. The observed bolometric gamma-ray energy released (Sbol)  
3. The observed gamma-ray spectral peak energy (Epk)  
4. The observed gamma-ray duration (T90)  

to an LGRB world model to infer the parameters of the model, which described the joint four dimensional distribution of the corresponding intrinsic properties of LGRBs:  

1. Isotropic 1024ms peak luminosity (Liso)  
2. Isotropic total gamma-ray energy emission (Eiso)  
3. The gamma-ray spectral peak energy (Epk)  
4. The gamma-ray duration (T90)  

In doing so, multiple plausible Star Formation Rate scenarios will be used as the prior distribution for the unknown redshifts of BATSE-catalog LGRBs. The individual LGRB redshifts are then estimated by convolving this prior knowledge with the inferred probablity density functions for the individual LGRBs' redshifts from the LGRB world model, combined with the input observed LGRB properties.  

First modified by Amir Shahmoradi, Friday 8:37 PM, September 14, 2012, IFS, UTEXAS.  

## Results  

- The 90% variation of the entire underlying LGRB population is (4.76-0.46)=4.0. The corresponding 50% variation is (3.31-1.37)=2.94.  
- The average 90% variation for individual BATSE LGRBs is 1.53 \pm 0.52 with a peak at ~ 1.1.  
- The average 50% variation for individual BATSE LGRBs is 0.67 \pm 0.22 with a peak at ~ 0.45.  

This indicates that the LGRB world model overall, can serve as an approximately good indicator of redshift, in the case of the underlying LGRB population tracing the star formation rate of Hopkins & Beacom (2006).  

Amir Shahmoradi, Friday 10:07 PM, September 21, 2012, IFS, UTEXAS.  

## Acknowledgment

As per the project's license agreement terms, if you use any parts of this library for any purposes, we ask you to acknowledge the usage in your work (education/research/industry/development/...) by citing the resulting scientific paper from this work described below in BibTeX format:  

@article{shahmoradi2019catalog,
  title={A Catalog of Redshift Estimates for 1366 BATSE Long-Duration Gamma-Ray Bursts: Evidence for Strong Selection Effects on the Phenomenological Prompt Gamma-Ray Correlations},
  author={Shahmoradi, Amir and Nemiroff, Robert J},
  journal={arXiv preprint arXiv:1903.06989},
  year={2019}
}


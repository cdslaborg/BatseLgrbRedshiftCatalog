def getRateDensityMadau(zplus1,model):
    from numpy import log,exp,log10,zeros
    
    model=[model_names.upper() for model_names in model]
    RateDensity=zeros((len(zplus1),len(model)))
    for i in range(0,len(model)):
        if model[i] == 'F18':
            a=2.99
            b=2.63
            c=6.19
            d=0.013
        elif model[i] == 'M17':
            a=2.6
            b=3.2
            c=6.2
            d=0.01
        elif model[i] == 'M14':
            a=2.7
            b=2.9
            c=5.6
            d=0.015
        else:
            print('please enter F18, M17, or M14')
        RateDensity[:,i] =(zplus1**a/(1.0+(zplus1/b)**c))*d
    return RateDensity


#def getRateDensityPiecewise(z,model):
#    from numpy import log,exp,log10,zeros
#    count = len(z)
#    RateDensity = zeros((count,len(model)))
#    model=[model_names.upper() for model_names in model]
#    zplus1=z+1
#    for j in range(len(model)):
#        if model[j]=='H06':
#            a0=-2.02
#            a1=-0.930
#            a2=4.64
#            b0=3.44
#            b1=-0.26
#            b2=-7.8
#            z0Break=0.97
#            z1Break=4.48
#            Logzplus1=log10(zplus1)
#        elif model[j]=='B10':
#            a0= -4.202275550138904
#            a1= -2.995375844044087
#            a2=3.893018421173863
#            b0=3.14
#            b1=1.36
#            b2= -2.92
#            z0Break=0.97
#            z1Break=4.0
#            Logzplus1=log10(zplus1)
#        elif model[j]=='L08':
#            a0=-1.70
#            a1=-0.727
#            a2=2.35
#            b0=3.30
#            b1=0.0549
#            b2=-4.46
#            z0Break=0.993
#            z1Break=3.80
#            Logzplus1=log10(zplus1)
#
#
#        for i in range(count):
#            if z[i] < 0:
#                RateDensity[i,j] =0
#            elif z[i] < z0Break:
#                RateDensity[i,j] = exp(a0+b0*Logzplus1[i])
#            elif z[i] < z1Break:
#                RateDensity[i,j] =  exp(a1+b1*Logzplus1[i]) 
#            else:
#                RateDensity[i,j] =  exp(a2+b2*Logzplus1[i])
#    return RateDensity

def getLogRateDensityPiecewise(Logzplus1,model):
    from numpy import zeros,log,log10
    count = len(Logzplus1)
    logRateDensity = zeros((count,len(model)))
    model=[model_names.upper() for model_names in model]
    for j in range(len(model)):
        if model[j]=='H06':
            Logz0plus1Constant=log10(1.97)
            Logz1plus1Constant=log10(5.50)
            g0=3.4
            g1=-0.3
            g2=-7.8
            LogNormFac1=Logz0plus1Constant*(g0-g1)
            LogNormFac2=(Logz1plus1Constant*(g1-g2))+LogNormFac1
        elif model[j]=='B10':
            Logz0plus1Constant=log10(1.97)
            Logz1plus1Constant=log10(5.00)
            g0=3.14
            g1=1.36
            g2=-2.92
            LogNormFac1=Logz0plus1Constant*(g0-g1)
            LogNormFac2=(Logz1plus1Constant*(g1-g2))+LogNormFac1
        elif model[j]=='L08':
            Logz0plus1Constant=log10(1.993)
            Logz1plus1Constant=log10(4.800)
            g0=3.3000
            g1=0.0549
            g2=-4.4600
            LogNormFac1=Logz0plus1Constant*(g0-g1)
            LogNormFac2=(Logz1plus1Constant*(g1-g2))+LogNormFac1


        for i in range(count):
            if Logzplus1[i] < 0:
                logRateDensity[i,j] = -Inf
            elif Logzplus1[i] < Logz0plus1Constant:
                logRateDensity[i,j] = Logzplus1[i] * g0
            elif Logzplus1[i] < Logz1plus1Constant:
                logRateDensity[i,j] = LogNormFac1 + Logzplus1[i] * g1
            else:
                logRateDensity[i,j] = LogNormFac2 + Logzplus1[i] * g2
    return logRateDensity
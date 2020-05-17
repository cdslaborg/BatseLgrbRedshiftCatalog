def getLogRateM14(z):
    from numpy import log,pi,Inf,all
    from TwiceLogLumDisWicMPC import TwiceLogLumDisWicMPC
    if all(z>0):
        log_coeff=log(4*pi*(3E5))
        omega_dm=0.3
        omega_de=0.7
        z_plus1=z+1
        log_z_plus1=log(z_plus1)
        a=2.7
        b=2.9
        c=5.6
        d=log(0.015)
        z_plus1_coeff=1.0/(b**c)
        log_sfrd=d+a*log_z_plus1-log(1.0+z_plus1_coeff*z_plus1**c)
        return_value=(log_coeff + TwiceLogLumDisWicMPC(z_plus1) - ( 3.0*log_z_plus1 + 0.5*log(omega_dm*z_plus1**3+omega_de) )+log_sfrd)
    else:
        return_value=-Inf
    return return_value
        
        
        
        
def getLogRateM17(z):
    from numpy import log,pi,Inf,all
    from TwiceLogLumDisWicMPC import TwiceLogLumDisWicMPC
    if all(z>0):
        log_coeff=log(4*pi*(3E5))
        omega_dm=0.3
        omega_de=0.7
        z_plus1=z+1
        log_z_plus1=log(z_plus1)
        a=2.6
        b=3.2
        c=6.2
        d=log(0.01)
        z_plus1_coeff=1.0/(b**c)
        log_sfrd=d+a*log_z_plus1-log(1.0+z_plus1_coeff*z_plus1**c)
        return_value= (log_coeff + TwiceLogLumDisWicMPC(z_plus1) - ( 3.0*log_z_plus1 + 0.5*log(omega_dm*z_plus1**3+omega_de) )+log_sfrd)
    else:
        return_value=-Inf
    return return_value
        
        
        
def getLogRateF18(z):
    from numpy import log,pi,Inf
    from TwiceLogLumDisWicMPC import TwiceLogLumDisWicMPC
    if all(z>0):
        log_coeff=log(4*pi*(3E5))
        omega_dm=0.3
        omega_de=0.7
        z_plus1=z+1
        log_z_plus1=log(z_plus1)
        a=2.99
        b=2.63
        c=6.19
        d=log(0.013)
        z_plus1_coeff=1.0/(b**c)
        log_sfrd=d+a*log_z_plus1-log(1.0+z_plus1_coeff*z_plus1**c)
        return_value=(log_coeff + TwiceLogLumDisWicMPC(z_plus1) - ( 3.0*log_z_plus1 + 0.5*log(omega_dm*z_plus1**3+omega_de) )+log_sfrd)
    else:
        return_value=-Inf
    return return_value

def getLogRateL08(z):
    from numpy import zeros,log,log10,Inf,pi
    from TwiceLogLumDisWicMPC import TwiceLogLumDisWicMPC
    if all(z>0):
        log_coeff=log(4*pi*(3E5))
        omega_dm=0.3
        omega_de=0.7
        z_plus1=z+1
        log_z_plus1=log(z_plus1)
        try:
            count = len(log_z_plus1)
        except TypeError:
            count =z.shape
        logRateDensity = zeros(count)
        Logz0plus1Constant=log(1.993)
        Logz1plus1Constant=log(4.800)
        g0=3.3000
        g1=0.0549
        g2=-4.4600
        LogNormFac1=Logz0plus1Constant*(g0-g1)
        LogNormFac2=(Logz1plus1Constant*(g1-g2))+LogNormFac1
        
        for i in range(count):
            if log_z_plus1[i] < 0:
                logRateDensity[i] = -Inf
            elif log_z_plus1[i] < Logz0plus1Constant:
                logRateDensity[i] = log_z_plus1[i] * g0
            elif log_z_plus1[i] < Logz1plus1Constant:
                logRateDensity[i] = LogNormFac1 + log_z_plus1[i] * g1
            else:
                logRateDensity[i] = LogNormFac2 + log_z_plus1[i] * g2
        return_value=logRateDensity+log_coeff + TwiceLogLumDisWicMPC(z_plus1) - ( 3.0*log_z_plus1 + 0.5*log(omega_dm*z_plus1**3+omega_de) )
    else:
        return_value=-Inf
    return return_value

def getLogRateH06(z):
    from numpy import zeros,log,log10,Inf,pi
    from TwiceLogLumDisWicMPC import TwiceLogLumDisWicMPC
    if all(z>0):
        log_coeff=log(4*pi*(3E5))
        omega_dm=0.3
        omega_de=0.7
        z_plus1=z+1
        log_z_plus1=log(z_plus1)
        count = len(log_z_plus1)
        logRateDensity = zeros(count)
        Logz0plus1Constant=log10(1.97)
        Logz1plus1Constant=log10(5.50)
        g0=3.4
        g1=-0.3
        g2=-7.8
        LogNormFac1=Logz0plus1Constant*(g0-g1)
        LogNormFac2=(Logz1plus1Constant*(g1-g2))+LogNormFac1
        
        for i in range(count):
            if log_z_plus1[i] < 0:
                logRateDensity[i] = -Inf
            elif log_z_plus1[i] < Logz0plus1Constant:
                logRateDensity[i] = log_z_plus1[i] * g0
            elif log_z_plus1[i] < Logz1plus1Constant:
                logRateDensity[i] = LogNormFac1 + log_z_plus1[i] * g1
            else:
                logRateDensity[i] = LogNormFac2 + log_z_plus1[i] * g2
        return_value= logRateDensity+log_coeff + TwiceLogLumDisWicMPC(z_plus1) - ( 3.0*log_z_plus1 + 0.5*log(omega_dm*z_plus1**3+omega_de) )
    else:
        return_value=-Inf
    return return_value

def getLogRateB10(z):
    from numpy import zeros,log,log10,Inf,pi,all
    from TwiceLogLumDisWicMPC import TwiceLogLumDisWicMPC
    if all(z>0):
        log_coeff=log(4*pi*(3E5))
        omega_dm=0.3
        omega_de=0.7
        z_plus1=z+1
        log_z_plus1=log(z_plus1)
        
        count = len(log_z_plus1)

        logRateDensity = zeros(count)
        Logz0plus1Constant=log10(1.97)
        Logz1plus1Constant=log10(5.00)
        g0=3.14
        g1=1.36
        g2=-2.92
        LogNormFac1=Logz0plus1Constant*(g0-g1)
        LogNormFac2=(Logz1plus1Constant*(g1-g2))+LogNormFac1
        
        for i in range(count):
            if log_z_plus1[i] < 0:
                logRateDensity[i] = -Inf
            elif log_z_plus1[i] < Logz0plus1Constant:
                logRateDensity[i] = log_z_plus1[i] * g0
            elif log_z_plus1[i] < Logz1plus1Constant:
                logRateDensity[i] = LogNormFac1 + log_z_plus1[i] * g1
            else:
                logRateDensity[i] = LogNormFac2 + log_z_plus1[i] * g2
        return_value=logRateDensity+log_coeff + TwiceLogLumDisWicMPC(z_plus1) - ( 3.0*log_z_plus1 + 0.5*log(omega_dm*z_plus1**3+omega_de) )
    else:
        return_value=-Inf
    return return_value
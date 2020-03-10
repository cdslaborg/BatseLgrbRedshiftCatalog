    function logLumDisWicMPC = getLogLumDisWicMPC(zplus1)
        % This function calculates the luminosity distance in units of Mpc. Input is redshift+1 (zplus1) and must be zplus1>=1.1.
        % The distance is calculated according to approximate algorithm of Wickramasinghe & Okwatta (2010). 
        % Note that for redshifts less than 0.1, the error in the calculated luminosity distance grows to more than 0.001.
        % This algorithm should therefore not be used for zplus1<0.1.
        % MPC2CM = 3.09e24;   % 1 Mega Parsec = MPC2CM centimeters.
        % LOGMPC2CMSQ4PI = log(4.0*PI) + 2.0*log(MPC2CM);     % log(MegaParsec2centimeters).

        % Cosmological constants
        LIGHT_SPEED = 3.e5;                                      % LIGHT_SPEED is the speed of light (Km/s).
        HUBBLE_CONST = 7.1e1;                                    % HUBBLE_CONST is the Hubble constant in units of km/s/MPc.
        LS2HC = LIGHT_SPEED / HUBBLE_CONST;                      % the speed of light in units of km/s divided by the Hubble constant.
        OMEGA_DE = 0.7                                     ;     % Dark Energy density.
        OMEGA_DM = 0.3                                      ;    % Dark Matter density.

        TWICE_OMEGA_DE_OVER_OMEGA_DM = 2. * OMEGA_DE / OMEGA_DM;
        ALPHA0 = 1. + TWICE_OMEGA_DE_OVER_OMEGA_DM;
        X0 = log( ALPHA0 + sqrt(ALPHA0^2-1.) );
        X0Sq = X0^2;
        PSI_COEF1 = 2.^(2./3.);
        PSI_COEF2 = -PSI_COEF1 / 252.;
        PSI_COEF3 = +PSI_COEF1 / 21060.;
        PSIX0 = X0^0.33333333 * ( PSI_COEF1 + X0Sq * (PSI_COEF2 + X0Sq*PSI_COEF3) );
        LOG_NORM_FAC = log(LS2HC / (OMEGA_DE^0.1666666666666667*OMEGA_DM^0.3333333333333333));
        alpha1          = 1. + TWICE_OMEGA_DE_OVER_OMEGA_DM ./ zplus1.^3;
        x1              = log( alpha1 + sqrt( alpha1.^2 - 1. ) );
        x1Sq            = x1.^2;
        psix1           = x1.^0.33333333 .* ( PSI_COEF1 + x1Sq .* (PSI_COEF2 + x1Sq*PSI_COEF3) );
        logLumDisWicMPC = LOG_NORM_FAC +  log(zplus1.*(PSIX0-psix1));
    end
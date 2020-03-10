function logDensitySFR = getLogDensitySFRM14(zplus1,logzplus1)
    logDensitySFR   = -4.199705077879927 ... log(0.015)
                    + 2.7 .* logzplus1 ...
                    - log( 1.0 + 0.002573778755292 * zplus1.^5.6 );
end

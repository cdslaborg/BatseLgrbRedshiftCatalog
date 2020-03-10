function logDensitySFR = getLogDensitySFRB10(logzplus1)
    count = length(logzplus1);
    logDensitySFR = zeros(count,1);
    for i = 1:count
        if logzplus1(i) < 0.
            logDensitySFR(i) = -Inf;
        elseif logzplus1(i) < 0.678033542749897
            logDensitySFR(i) = logzplus1(i) * 3.14 - 4.202275550138904;
        elseif logzplus1(i) < 1.609437912434100
            logDensitySFR(i) = logzplus1(i) * 1.36 - 2.995375844044087;
        else
            logDensitySFR(i) = 3.893018421173863 - logzplus1(i) * 2.92;
        end
    end
end

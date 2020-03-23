function burninLoc = getBurninLoc(LogFunc)
    refLogFunc = max(LogFunc);
    numLogFunc = length(LogFunc);
    negLogIncidenceProb = log( numLogFunc );
    burninLoc = 0;
    while true
        burninLoc = burninLoc + 1;
        if burninLoc>numLogFunc || refLogFunc-LogFunc(burninLoc)<negLogIncidenceProb
            break;
        end
    end
end
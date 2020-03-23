function skip4NewSampleSize = getSkip4NewSampleSize(oldSampleSize,newSampleSize)
    addition = int32(1);
    quotient = idivide( int32(oldSampleSize) , int32(newSampleSize) );
    if mod(int32(oldSampleSize),int32(newSampleSize))==int32(0)
        addition = int32(0);
    end
    skip4NewSampleSize = quotient + addition;
end
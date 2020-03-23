function Map = mapTriggers(Trig1,Trig2)
    lenTrig1 = length(Trig1);
    lenTrig2 = length(Trig2);
    maxLen = max(lenTrig1,lenTrig2);
    Map.Trig = zeros(maxLen,1);
    Map.Indx1 = zeros(maxLen,1);
    Map.Indx2 = zeros(maxLen,1);
    counter = 0;
    for i = 1:length(Trig1)
        for j = 1:length(Trig2)
            if Trig1(i)==Trig2(j)
                counter = counter + 1;
                Map.Trig(counter) = Trig1(i);
                Map.Indx1(counter) = i;
                Map.Indx2(counter) = j;
                break;
            end
        end
    end
    Map.Trig = Map.Trig(1:counter);
    Map.Indx1 = Map.Indx1(1:counter);
    Map.Indx2 = Map.Indx2(1:counter);
end
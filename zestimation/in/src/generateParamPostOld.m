close all;
clear all;

filePath = '..\..\..\..\..\2010_LGRB_WORLD\28. Pbol-Epk-Sbol-T90\1. SFR Hopkins\1. fitting\1. original\';
File = dir([filePath,'MHMCMC_RAW_SAMPLE*.txt']);
nFile = length(File);
d = cell(nFile,1);
BurninLoc = zeros(nFile,1);
ChainSize = zeros(nFile,1);

for iFile = 1:nFile

    d{iFile} = importdata( [ filePath , File(iFile).name ] );
    d{iFile}.data = d{iFile}.data(2:end,:); % ignore the first row which has incorrect loglikelihood value reported
    BurninLoc(iFile) = getBurninLoc(d{iFile}.data(:,3));

    figure; hold on;
    plot(d{iFile}.data(:,3));
    plot(BurninLoc(iFile),d{iFile}.data(BurninLoc(iFile),3),'.','markersize',20,'color','red');
    set(gca,'xscale','log');
    hold off;

    d{iFile}.data = d{iFile}.data(BurninLoc(iFile):end,:); % remove the burnin part
    ChainSize(iFile) = length(d{iFile}.data);

end

nvar = length(d{iFile}.data(1,:)) - 4;
offset = 6;
ncol = offset + nvar;
lenPostDistCompact = sum(ChainSize);
PostDistCompact = -999*ones(lenPostDistCompact,ncol);
CumSumChainSize = zeros(nFile+1,1);
CumSumChainSize(1) = 0;
CumSumChainSize(2:end) = cumsum(ChainSize);
for iFile = 1:nFile
    PostDistCompact(CumSumChainSize(iFile)+1:CumSumChainSize(iFile+1),2) = d{iFile}.data(:,4); % MeanAcceptanceRate
    SampleWeight = zeros(ChainSize(iFile),1);
    for isample = 1:ChainSize(iFile)-1
        SampleWeight(isample) = d{iFile}.data(isample+1,2) - d{iFile}.data(isample,2);
    end
    SampleWeight(end) = 1; % set the weight of the last sample to one
    PostDistCompact(CumSumChainSize(iFile)+1:CumSumChainSize(iFile+1),5) = SampleWeight; % SampleWeight
    
    PostDistCompact(CumSumChainSize(iFile)+1:CumSumChainSize(iFile+1),6) = d{iFile}.data(:,3); % SampleLogFunc
    PostDistCompact(CumSumChainSize(iFile)+1:CumSumChainSize(iFile+1),offset+1:end) = d{iFile}.data(:,5:end);
end

Acl = autocorr(PostDistCompact(:,7),lenPostDistCompact-1);
CumSumAcl = cumsum(Acl);
figure; plot(CumSumAcl); set(gca,'xscale','log');

dlmwrite('../ParamPostRaw.txt',PostDistCompact,' ');

ColHead = { 'ProcessorID' ...
          , 'MeanAcceptanceRate' ...
          , 'AdaptationMeasure' ...
          , 'BurninLocation' ...
          , 'SampleWeight' ...
          , 'SampleLogFunc' ...
          , 'avgLog10Liso' ...
          , 'avgLog10Epkz' ...
          , 'avgLog10Eiso' ...
          , 'avgLog10T90z' ...
          , 'stdLog10Liso' ...
          , 'stdLog10Epkz' ...
          , 'stdLog10Eiso' ...
          , 'stdLog10T90z' ...
          , 'rhoLisoEpkz' ...
          , 'rhoLisoEiso' ...
          , 'rhoLisoT90z' ...
          , 'rhoEisoEpkz' ...
          , 'rhoEisoT90z' ...
          , 'rhoEpkzT90z' ...
          , 'avgLog10Thresh' ...
          , 'stdLog10Thresh' ...
          };
lenPostDistVerbose = sum(PostDistCompact(:,5));
ibegin = 1;
icolSampleWeight = 5;
%PostDistVerbose = struct();
%for irow = lenPostDistVerbose:-1:1
    for icol = 1:ncol
        PostDistVerbose.(ColHead{icol}) = zeros(lenPostDistVerbose,1);
    end
%end
for irow = 1:lenPostDistCompact
    iend = PostDistCompact(irow,icolSampleWeight) + ibegin - 1;
    for icol = 1:ncol
        PostDistVerbose.(ColHead{icol})(ibegin:iend) = PostDistCompact(irow,icol);
    end
    ibegin = iend + 1;
end
iskip = 500;
for icol = 1:ncol
	figure;
    h = histogram(PostDistVerbose.(ColHead{icol}));
    xlabel(ColHead{icol})
    figure;
    PostDist.(ColHead{icol}) = PostDistVerbose.(ColHead{icol})(1:iskip:end);
    autocorr(PostDist.(ColHead{icol}),length(PostDist.(ColHead{icol}))-1)
    set(gca,'xscale','log');
end

PostDist = struct2table(PostDist);
writetable(PostDist(:,6:end),'../paramPostDistHB06.txt');

d = importdata('../paramPostDistHB06.txt');
for icol = 1:length(d.data(1,:))
    figure;
    plot(d.data(:,15),d.data(:,icol),'.');
	%histogram(d.data(:,icol));
    %set(gca,'xscale','log');
end


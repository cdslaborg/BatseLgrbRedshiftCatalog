close all;
clear all;

requestedSampleSize = 2000;
nFile = 3;
nameOfSFR = {'HB06','Li08','BB10'};
rootPath = '../../../cosmicRate/out/withKfac/';
filePath.HB06 = [rootPath,'HB06/ParaDRAM_run_20190101_001428.573_image_1_chain.txt'];
filePath.Li08 = [rootPath,'Li08/ParaDRAM_run_20181228_030154.938_image_1_chain.txt'];
filePath.BB10 = [rootPath,'BB10/ParaDRAM_run_20181228_033327.012_image_1_chain.txt'];

RefinedSample = cell(nFile,1);
for iFile = 1:nFile

    disp(['Processing data for SFR model ',nameOfSFR{iFile},'...'])

    d = importdata( [ filePath.(nameOfSFR{iFile}) ] );
    burninLoc = getBurninLoc(d.data(:,6));  % colum 6 contains logFunc

    figure; hold on;
    plot(d.data(:,6));
    plot(burninLoc,d.data(burninLoc,6),'.','markersize',20,'color','red');
    set(gca,'xscale','log');
    hold off;

    d.data = d.data(burninLoc:end,:); % remove the burnin part
    compactChainSize = length(d.data);
    offset = 6;
    ncol = length(d.data(1,:));
    nvar = ncol - offset;

    % generate verbose chain
    disp('    generating verbose chain...')
    verboseChainSize = sum(d.data(:,5)); % sum of weights
    dv = zeros(verboseChainSize,nvar+1);
    irow = 0;
    for isample = 1:compactChainSize
        for iweight = 1:d.data(isample,5)
            irow = irow + 1;
            dv(irow,:) = d.data(isample,6:end);
        end
    end

    % write output ParaPost file
    paraPostFile = ['../paraPost',nameOfSFR{iFile},'.csv'];

    % write column headers
    fid = fopen(paraPostFile, 'w');
    fprintf(fid, '%s,', d.colheaders{6:end-1} );  % column headers
    fprintf(fid, '%s\n', d.colheaders{end} );  % last column header
    fclose(fid);

    % write sample
    skip = getSkip4NewSampleSize(verboseChainSize,requestedSampleSize+1);
    RefinedSample{iFile} = dv(1:skip-1:verboseChainSize,:);
    disp(['    Sample Size: ', num2str(length(RefinedSample{iFile}(:,1))) ])
    dlmwrite(paraPostFile,RefinedSample{iFile},'-append','delimiter',',');

end
return
% plot variables
fontSize = 12;
lineWidth = 1.5;
for icol = 1:nvar+1

    % histogram variables
    figure; hold on;
    for iFile = 1:nFile
        data = RefinedSample{iFile}(:,icol);
        histogram( data ...
                 , 'Normalization' , 'probability' ...
                 ..., 'Normalization' , 'countdensity' ...
                 , 'EdgeColor' , 'none' ...
                 )
    end
    xlabel(d.colheaders{icol+5})
    legend(nameOfSFR{:})
    hold off;

	% AutoCorr variables
    figure; hold on;
    for iFile = 1:nFile
        data = RefinedSample{iFile}(:,icol);
        [acl.vals,acl.lags,acl.bounds] = autocorr(data,length(data)-1);
        plot(acl.lags, acl.vals, 'linewidth', lineWidth );
        %line([acl.lags(2),acl.lags(end)], [acl.bounds(1),acl.bounds(1)], 'color', 'red', 'linewidth', lineWidth );
        %line([acl.lags(2),acl.lags(end)], [acl.bounds(2),acl.bounds(2)], 'color', 'red', 'linewidth', lineWidth );
    end
    xlabel('Auto-Correlation Lag', 'fontsize', fontSize);
    ylabel(['AutoCorrelation ( ',d.colheaders{icol+5},' )'], 'fontsize', fontSize);
    set(gca,'xscale','log')
    legend(nameOfSFR{:})
    hold off;

end

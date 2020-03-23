close all
clear all

NGRB = 1366;
nModelSFR = 3;
nameOfSFR = {'HB06','Li08','BB10'};
datPath = cell(nModelSFR,1);
StatZ = cell(nModelSFR,1);
CI50 = zeros(NGRB,nModelSFR);
CI90 = zeros(NGRB,nModelSFR);
ColNumCI50 = [5,7];
ColNumCI90 = [4,8];
for imodel = 1:nModelSFR

    datPath{imodel} = ['../../out/',nameOfSFR{imodel},'/'];
    StatZ{imodel} = importdata([datPath{imodel},'batse_zstat.txt']);

    %upper = getLogzplus1( StatZ{imodel}.data(:,ColNumCI50(2)) );
    %lower = getLogzplus1( StatZ{imodel}.data(:,ColNumCI50(1)) );
    %CI50(:,imodel) = exp( upper - lower ) - 1.0;
    %upper = getLogzplus1( StatZ{imodel}.data(:,ColNumCI90(2)) );
    %lower = getLogzplus1( StatZ{imodel}.data(:,ColNumCI90(1)) );
    %CI90(:,imodel) = exp( upper - lower ) - 1.0;
    upper = ( StatZ{imodel}.data(:,ColNumCI50(2)) );
    lower = ( StatZ{imodel}.data(:,ColNumCI50(1)) );
    CI50(:,imodel) = ( upper - lower );
    upper = ( StatZ{imodel}.data(:,ColNumCI90(2)) );
    lower = ( StatZ{imodel}.data(:,ColNumCI90(1)) );
    CI90(:,imodel) = ( upper - lower );

end


ncol = 8;
ColheadersStatZ =   { 'Trigger' ...
                    , 'Mean(Redshift)' ...
                    , 'Mode(Redshift)' ...
                    , 'Q05(Redshift)' ...
                    , 'Q25(Redshift)' ...
                    , 'Q50(Redshift)' ...
                    , 'Q75(Redshift)' ...
                    , 'Q95(Redshift)' ...
                    };
for icol = 2:ncol
    figure; hold on;
    for imodel = 1:nModelSFR
        data = log10( 1 + StatZ{imodel}.data(:,icol) );
        histogram   ( data ...
                    , 'Normalization' , 'probability' ...
                    ..., 'Normalization' , 'countdensity' ...
                    , 'EdgeColor' , 'none' ...
                    )

%         figure;hold on;
%         histogram(StatZ.data(:,7)-StatZ.data(:,5))
%         histogram(StatZ.data(:,8)-StatZ.data(:,4))
%         hold off;
% 
%         disp(['median 50% confidence level on GRB redshifts: ', num2str(median(StatZ.data(:,7)-StatZ.data(:,5)))] )
%         disp(['median 90% confidence level on GRB redshifts: ', num2str(median(StatZ.data(:,8)-StatZ.data(:,4)))] )
    end
    xlabel(ColheadersStatZ{icol})
    legend(nameOfSFR{:})
    hold off;
end

RangeZ = [0.1,10];
figure; hold on;
icol = 2;
imodelx = 2;
imodely = {1,3};
ErrCol = {4,8}; % 90%
%ErrCol = {5,7}; % 50%
for imodel = 1:1
    %plot(StatZ{imodelx}.data(:,icol),StatZ{imodely{imodel}}.data(:,icol),'.')
    p = errorbar( StatZ{imodelx}.data(:,icol) ...
                , StatZ{imodely{imodel}}.data(:,icol) ...
                ,     StatZ{imodely{imodel}}.data(:,icol) - StatZ{imodely{imodel}}.data(:,ErrCol{1}) ... %yneg
                , abs(StatZ{imodely{imodel}}.data(:,icol) - StatZ{imodely{imodel}}.data(:,ErrCol{2})) ... %ypos
                ,     StatZ{imodelx}.data(:,icol) - StatZ{imodelx}.data(:,ErrCol{1}) ... %xneg
                , abs(StatZ{imodelx}.data(:,icol) - StatZ{imodelx}.data(:,ErrCol{2})) ... %xpos
                , '.', 'markersize', 10, 'color', 'red' ...
                , 'MarkerFaceColor' , 'black' ...
                , 'MarkerEdgeColor' , 'black' ...
                , 'CapSize', 0 ...
                );
	P.LineWidth = 0.1;
end
line(RangeZ, RangeZ, 'color', 'green', 'linewidth', 3);
set(gca,'xscale','log')
set(gca,'yscale','log')
hold off;

figure; hold on;
histogram(StatZ{1}.data(:,2)-StatZ{2}.data(:,2))
histogram(StatZ{1}.data(:,2)-StatZ{3}.data(:,2))
hold off;

return

for imodel = 1:nModelSFR

    FileList = dir([datPath{imodel},'zdist*.txt']);
    nFile = length(FileList);

    % find nz
    filePath = [datPath{imodel},FileList(1).name];
    d = importdata(filePath);
    nz = length(d.data(:,1));
    Grid.Z = d.data(:,1);
    Grid.Likelihood = zeros(nz,nFile);
    SumLikelihood = zeros(nz,1);

    % read data and perform analysis
    for iFile = nFile:-1:1

        filePath = [datPath{imodel},FileList(iFile).name];
        disp(['Processing file number ', num2str(iFile), ': ', filePath]);
        d = importdata(filePath);
        Grid.Likelihood(1:nz,iFile) = d.data(:,2);

        % find the average distribution of all GRB redshifts together
        SumLikelihood = SumLikelihood + d.data(:,2);

        % find the mean of the distribution
        Redshift(iFile).mean = sum(d.data(:,2).*d.data(:,1)) / sum(d.data(:,2));

        % find the mode of the distribution
        [ modeLikelihood , modeIndex ] = max(d.data(:,2));
        Redshift(iFile).mode = d.data(modeIndex,1);

        % find the quantiles of the distribution
        CumSumLikelihood = cumsum(d.data(:,2));
        CumSumLikelihood = CumSumLikelihood / CumSumLikelihood(end);
        for iloc = 1:length(CumSumLikelihood)
            if CumSumLikelihood(iloc)<0.05
                Redshift(iFile).quantile05 = d.data(iloc,1);
            elseif CumSumLikelihood(iloc)<0.25
                Redshift(iFile).quantile25 = d.data(iloc,1);
            elseif CumSumLikelihood(iloc)<0.50
                Redshift(iFile).quantile50 = d.data(iloc,1);
            elseif CumSumLikelihood(iloc)<0.75
                Redshift(iFile).quantile75 = d.data(iloc,1);
            elseif CumSumLikelihood(iloc)<0.95
                Redshift(iFile).quantile95 = d.data(iloc,1);
            end
        end

        Redshift(iFile).width50 = Redshift(iFile).quantile75 - Redshift(iFile).quantile25;
        Redshift(iFile).width90 = Redshift(iFile).quantile95 - Redshift(iFile).quantile05;

    end

    figure; hold on;
    iskip = 50;
    for iFile = nFile:-25:1
        plot(Grid.Z,Grid.Likelihood(:,iFile))
    end
    set(gca,'xscale','log');
    hold off;

    figure;
    SumLikelihood = SumLikelihood / nz;
    plot(d.data(:,1),SumLikelihood,'.')
    set(gca,'xscale','log');
    hold off;

    figure; hold on;
    histogram([Redshift.mode])
    histogram([Redshift.mean])
    histogram([Redshift.quantile50])
    legend('mode','mean','median')
    hold off;

    figure;hold on;
    histogram([Redshift.quantile75]-[Redshift.quantile25])
    histogram([Redshift.quantile95]-[Redshift.quantile05])
    hold off;

    % figure;hold on;
    % histogram( log10([Redshift.quantile75]+1) - log10([Redshift.quantile25]+1) )
    % histogram( log10([Redshift.quantile95]+1) - log10([Redshift.quantile05]+1) )
    % hold off;

end
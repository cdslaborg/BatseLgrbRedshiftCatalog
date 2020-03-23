close all;
%clear all;
format compact; format long;
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath);
addpath(genpath('../../../../../lib/matlab/')) % lib codes

fontSize = 13;
lineWidth = 2.5;
SNErrCol = {4,8}; % 90%
%SNErrCol = {5,7}; % 50%
RangeZ = [0.08,30.0];
binWidth = 0.25;
BinLimits = [ -2 2 ] * log(10);
rootPath = '../../winx64/intel/release/static/serial/bin/';

% read rival data
ZModel.count = 3;
LegendArr =  { 'PI_{50%}: H06' ...
             , 'PI_{90%}: H06' ...
             , 'PI_{50%}: L08' ...
             , 'PI_{90%}: L08' ...
             , 'PI_{50%}: B10' ...
             , 'PI_{90%}: B10' ...
             };
ZModel.ID = {'S19H06','S19L08','S19B10'};
ZModel.(ZModel.ID{1}) = importdata([rootPath,'out/H06/batse_zstat.txt']);
ZModel.(ZModel.ID{2}) = importdata([rootPath,'out/L08/batse_zstat.txt']);
ZModel.(ZModel.ID{3}) = importdata([rootPath,'out/B10/batse_zstat.txt']);

PI50 = cell(ZModel.count,1);
PI90 = cell(ZModel.count,1);
for imodel = 1:ZModel.count

    % generate data
    PI50{imodel}.Data = ZModel.(ZModel.ID{imodel}).data(:,7) - ZModel.(ZModel.ID{imodel}).data(:,5) ;
    PI90{imodel}.Data = ZModel.(ZModel.ID{imodel}).data(:,8) - ZModel.(ZModel.ID{imodel}).data(:,4) ;
    PI50{imodel}.avgData = mean(PI50{imodel}.Data);
    PI90{imodel}.avgData = mean(PI90{imodel}.Data);

    figure('visible','off','Color','none');
    h = histogram( log( PI50{imodel}.Data ) );
    h.BinWidth = binWidth;
    h.BinLimits = BinLimits;
    PI50{imodel}.BinEdges = h.BinEdges(1:end-1);
    PI50{imodel}.BinCounts = h.BinCounts;
    close(gcf);

    figure('visible','off','Color','none');
    h = histogram( log( PI90{imodel}.Data ) );
    h.BinWidth = binWidth;
    h.BinLimits = BinLimits;
    PI90{imodel}.BinEdges = h.BinEdges(1:end-1);
    PI90{imodel}.BinCounts = h.BinCounts;
    close(gcf);

end

% average uncertainty
disp(['PI50.avgData: ', num2str(PI50{1}.avgData),' ', num2str(PI50{2}.avgData),' ', num2str(PI50{3}.avgData) ]);
disp(['mean(PI50.avgData): ', num2str( (PI50{1}.avgData + PI50{2}.avgData + PI50{3}.avgData)/3) ]);

disp(['PI90.avgData: ', num2str(PI90{1}.avgData),' ', num2str(PI90{2}.avgData),' ', num2str(PI90{3}.avgData) ]);
disp(['mean(PI90.avgData): ', num2str( (PI90{1}.avgData + PI90{2}.avgData + PI90{3}.avgData)/3) ]);

% now make plots
%figure('visible','off','Color','none');
figure;
hold on; box on;
for imodel = 1:ZModel.count
    ax = gca; ax.ColorOrderIndex = imodel;
    plot( exp( PI50{imodel}.BinEdges ) ...
        , PI50{imodel}.BinCounts ...
        , '-', 'linewidth', lineWidth ...
        );
    ax = gca; ax.ColorOrderIndex = imodel;
    plot( exp( PI90{imodel}.BinEdges ) ...
        , PI90{imodel}.BinCounts ...
        , ':', 'linewidth', lineWidth ...
        );
end
xlabel(['Predicted Redshift Interval'], 'fontSize', fontSize)
ylabel(['BATSE LGRB Count'], 'fontSize', fontSize)
xlim(RangeZ);
set(gca,'xscale','log')
%set(gca,'yscale','log')
legend  ( LegendArr ...
        , 'location' , 'northeast' ...
        , 'fontSize' , fontSize ...
        , 'color' , 'none' ...
        )
set(gca,'color','none', 'fontSize', fontSize)
%export_fig ([ZModel.ID{imodel},ZModel.ID{jRival},'.png'],'-m2 -transparent')
hold off;
%close(gcf);

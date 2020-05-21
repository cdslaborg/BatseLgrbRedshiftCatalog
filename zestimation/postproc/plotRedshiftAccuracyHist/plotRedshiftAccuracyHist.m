close all;
%clear all;
format compact; format long;
filePath = mfilename("fullpath");
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath);
addpath(genpath("../../../../../lib/matlab/")) % lib codes

fontSize = 13;
lineWidth = 2.5;
SNErrCol = {4,8}; % 90%
%SNErrCol = {5,7}; % 50%
RangeZ = [0.08,30.0];
binWidth = 0.25;
BinLimits = [ -2 2 ] * log(10);

outDirRoot = fullfile(scriptPath,"out");
mkdir(outDirRoot);

outDir = fullfile(outDirRoot,kfac,"PI"+string(confidence),scale);
mkdir(outDir);

if confidence==50
    SNErrCol = {5,7}; % 50%
elseif confidence==90
    SNErrCol = {4,8}; % 90%
else
    error(["The requested confidence level is not supported:", num2str(confidence)]);
end

rootPath = fullfile("../../build/winx64/intel/19.0.4.245/release/static/heap/serial/fortran", kfac);

% read rival data
ZModel.ID = ["H06","L08","B10","M14","M17","F18"];%,"B04","F00","Y04"];
ZModel.count = length(ZModel.ID);
LegendArr = [];
for imodel = 1:ZModel.count
    zstatFilePath = fullfile(rootPath,ZModel.ID(imodel),"bin","out","batse_zstat.txt");
    ZModel.(ZModel.ID(imodel)) = importdata(zstatFilePath);
    LegendArr = [ LegendArr
                , "PI_{50%}: " + ZModel.ID(imodel) ...
                , "PI_{90%}: " + ZModel.ID(imodel) ...
                ];
end

PI50 = cell(ZModel.count,1);
PI90 = cell(ZModel.count,1);
for imodel = 1:ZModel.count

    % generate data
    PI50{imodel}.Data = ZModel.(ZModel.ID(imodel)).data(:,7) - ZModel.(ZModel.ID(imodel)).data(:,5) ;
    PI90{imodel}.Data = ZModel.(ZModel.ID(imodel)).data(:,8) - ZModel.(ZModel.ID(imodel)).data(:,4) ;
    PI50{imodel}.avgData = mean(PI50{imodel}.Data);
    PI90{imodel}.avgData = mean(PI90{imodel}.Data);

    figure("visible","off","Color","none");
    h = histogram( log( PI50{imodel}.Data ) );
    h.BinWidth = binWidth;
    h.BinLimits = BinLimits;
    PI50{imodel}.BinEdges = h.BinEdges(1:end-1);
    PI50{imodel}.BinCounts = h.BinCounts;
    close(gcf);

    figure("visible","off","Color","none");
    h = histogram( log( PI90{imodel}.Data ) );
    h.BinWidth = binWidth;
    h.BinLimits = BinLimits;
    PI90{imodel}.BinEdges = h.BinEdges(1:end-1);
    PI90{imodel}.BinCounts = h.BinCounts;
    close(gcf);

end

% average uncertainty

disp( "PI50.avgData: " + string(PI50{1}.avgData) + " " + string(PI50{2}.avgData) + " " + string(PI50{3}.avgData) );
disp( "mean(PI50.avgData): " + string((PI50{1}.avgData + PI50{2}.avgData + PI50{3}.avgData)/3) );

disp( "PI90.avgData: " + string(PI90{1}.avgData) + " " + string(PI90{2}.avgData) + " " + string(PI90{3}.avgData) );
disp( "mean(PI90.avgData): " + string((PI90{1}.avgData + PI90{2}.avgData + PI90{3}.avgData)/3) );

% now make plots

%figure("visible","off","Color","none");
figure;
hold on; box on;
for imodel = 1:ZModel.count
    ax = gca; ax.ColorOrderIndex = imodel;
    plot( exp( PI50{imodel}.BinEdges ) ...
        , PI50{imodel}.BinCounts ...
        , "-", "linewidth", lineWidth ...
        );
    ax = gca; ax.ColorOrderIndex = imodel;
    plot( exp( PI90{imodel}.BinEdges ) ...
        , PI90{imodel}.BinCounts ...
        , ":", "linewidth", lineWidth ...
        );
end
xlabel("Predicted Redshift Interval", "fontSize", fontSize)
ylabel("BATSE LGRB Count", "fontSize", fontSize)
xlim(RangeZ);
set(gca,"xscale","log")
%set(gca,"yscale","log")
legend  ( LegendArr ...
        , "location" , "northeast" ...
        , "fontSize" , fontSize ...
        , "color" , "none" ...
        )
set(gca,"color","none", "fontSize", fontSize)
%export_fig ([ZModel.ID(imodel),ZModel.ID{jRival},".png"],"-m2 -transparent")
hold off;
%close(gcf);

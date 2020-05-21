% comparing old rival estimates with each other 
close all;
clear all;
format compact; format long;
addpath(genpath("../")) % local lib codes
addpath(genpath("../../../../../lib/matlab/")) % lib codes

% change directory to the srouce code directory
filePath = mfilename("fullpath");
[scriptPath,~,~] = fileparts(filePath); cd(scriptPath); % Change working directory to source code directory.
cd(scriptPath); % Change working directory to source code directory.

% Read BATSE LGRB data
inPath = "../../in/";
nvar = 4;
dum = importdata(fullfile(inPath,"batse_1366_lgrb_pbol_epk_sbol(0.001,20000).txt"));
VarName = ["Pbol","Sbol","Epk","T90"];
AxisLabel = [ "Bolometric Peak Flux: P_{bol} [ergs/s/cm^2]" ...
            , "Bolometric Fluence: S_{bol} [ergs/cm^2]" ...
            , "Observed Spectral Peak Energy: E_{p} [keV]" ...
            , "Observed Duration: T_{90} [s]" ...
            ];
Prop.Trigger      = dum.data(:,1);
Prop.(VarName{1}).Data = exp( dum.data(:,2) );
Prop.(VarName{2}).Data = exp( dum.data(:,3) );
Prop.(VarName{3}).Data = exp( dum.data(:,4) );
Prop.(VarName{4}).Data = exp( dum.data(:,8) );

Prop.(VarName{1}).legendLoc = "northeast";
Prop.(VarName{2}).legendLoc = "northeast";
Prop.(VarName{3}).legendLoc = "southwest";
Prop.(VarName{4}).legendLoc = "southwest";

fontSize = 13;
errBarLineWidth = 0.5;
kfac = "kfacOneThird";
%scale = "linear";
scale = "log";
confidence = 90;

outDirRoot = fullfile(scriptPath,"out");
mkdir(outDirRoot);

outDir = fullfile(outDirRoot,kfac,"PI"+string(confidence),scale);
mkdir(outDir);

disp("confidence level: "+string(confidence)+"%");
if confidence==50
    SNErrCol = {5,7}; % 50%
elseif confidence==90
    SNErrCol = {4,8}; % 90%
else
    error(["The requested confidence level is not supported:", num2str(confidence)]);
end
mkdir(outDir);

rootPath = "../../build/winx64/intel/19.0.4.245/release/static/heap/serial/fortran/" + kfac;

% read BATSE redshift data

ZModel.Ref = [ "This Work (H06 Rate)"   ...
             , "This Work (L08 Rate)"   ...
             , "This Work (B10 Rate)"   ...
             , "This Work (M14 Rate)"   ...
             , "This Work (M17 Rate)"   ...
             , "This Work (F18 Rate)"   ...
             , "Band (2004)"            ...
             , "Fenimore (2000)"        ...
             , "Yonetoku (2004)"        ...
             ];
ZModel.ID = ["H06","L08","B10","M14","M17","F18","B04","F00","Y04"];
ZModel.count = length(ZModel.ID);

ZModel.(ZModel.ID{1}) = importdata(fullfile(rootPath,"H06","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{2}) = importdata(fullfile(rootPath,"L08","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{3}) = importdata(fullfile(rootPath,"B10","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{4}) = importdata(fullfile(rootPath,"M14","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{5}) = importdata(fullfile(rootPath,"M17","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{6}) = importdata(fullfile(rootPath,"F18","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{7}) = importdata(fullfile(inPath,"RedshiftBand2004.txt"));
ZModel.(ZModel.ID{8}) = importdata(fullfile(inPath,"RedshiftFenimore2000.txt"));
ZModel.(ZModel.ID{9}) = importdata(fullfile(inPath,"RedshiftYonetoku2004.txt"));

% redshift ranges for the plots
ZModel.(ZModel.ID{1}).RangeZ = [0.1,10];
ZModel.(ZModel.ID{2}).RangeZ = [0.1,10];
ZModel.(ZModel.ID{3}).RangeZ = [0.1,10];
ZModel.(ZModel.ID{4}).RangeZ = [0.1,10];
ZModel.(ZModel.ID{5}).RangeZ = [0.1,10];
ZModel.(ZModel.ID{6}).RangeZ = [0.1,10];
ZModel.(ZModel.ID{7}).RangeZ = [0.04,60.0];
ZModel.(ZModel.ID{8}).RangeZ = [0.04,60.0];
ZModel.(ZModel.ID{9}).RangeZ = [0.04,60.0];

% column numbers containing redshift data
ZModel.(ZModel.ID{1}).zcol = 2; % the column number of H06 redshift predictions
ZModel.(ZModel.ID{2}).zcol = 2; % the column number of L08 redshift predictions
ZModel.(ZModel.ID{3}).zcol = 2; % the column number of B10 redshift predictions
ZModel.(ZModel.ID{4}).zcol = 2; % the column number of M14 redshift predictions
ZModel.(ZModel.ID{5}).zcol = 2; % the column number of M17 redshift predictions
ZModel.(ZModel.ID{6}).zcol = 2; % the column number of F18 redshift predictions
ZModel.(ZModel.ID{7}).zcol = 2; % the column number of Band 2004 redshift predictions
ZModel.(ZModel.ID{8}).zcol = 5; % the column number of Fenimore 2000 redshift predictions
ZModel.(ZModel.ID{9}).zcol = 5; % the column number of Yonetoku 2004 redshift predictions

%Map = cell(ZModel.count*(ZModel.count-1)/2,1);
for izModel = 1:ZModel.count

    disp("Generating figures for redshift model: " + ZModel.ID(izModel) + "...");

    for ivar = 1:nvar

        disp("    Generating figure for: " + VarName(ivar));

        % generate a map of shared triggers between the two datasets
        Map = mapTriggers( ZModel.(ZModel.ID(izModel)).data(:,1) , Prop.Trigger );

        % generate data
        X.Data = ZModel.(ZModel.ID(izModel)).data( Map.Indx1 , ZModel.(ZModel.ID(izModel)).zcol );
        if izModel<7
            X.Err.Neg   = ZModel.(ZModel.ID(izModel)).data(Map.Indx1,ZModel.(ZModel.ID(izModel)).zcol) ...
                        - ZModel.(ZModel.ID(izModel)).data(Map.Indx1,SNErrCol{1});
            X.Err.Pos   = ZModel.(ZModel.ID(izModel)).data(Map.Indx1,ZModel.(ZModel.ID(izModel)).zcol) ...
                        - ZModel.(ZModel.ID(izModel)).data(Map.Indx1,SNErrCol{2});
            X.Err.Pos   = abs(X.Err.Pos);
        else
            X.Err.Neg   = zeros(length(X.Data),1);
            X.Err.Pos   = zeros(length(X.Data),1);
        end

        %Y.Data = ZModel.(ZModel.ID{iprop}).data( Map.Indx2 , ZModel.(ZModel.ID{iprop}).zcol );
        Y.Data = Prop.(VarName(ivar)).Data(Map.Indx2);
        Y.Err.Neg = zeros(length(Y.Data),1);
        Y.Err.Pos = zeros(length(Y.Data),1);

        figure("visible","off","Color","none");
        hold on; box on;

        %line(RangeZ, RangeZ, "color", [0.6 0.6 0.6], "linewidth", 2);
        ErrPlot = errorbar  ( X.Data ...
                            , Y.Data ...
                            , Y.Err.Neg ...
                            , Y.Err.Pos ...
                            , X.Err.Neg ...
                            , X.Err.Pos ...
                            , ".", "markersize", 8, "color", [0 1 0] ...
                            , "MarkerFaceColor" , [0.09 0.52 0.16] ...
                            , "MarkerEdgeColor" , [0.09 0.52 0.16] ...
                            , "CapSize", 0 ...
                            );
        ErrPlot.LineWidth = errBarLineWidth;
        xlabel("Redshift: " + ZModel.Ref(izModel), "Interpreter", "tex", "fontSize", fontSize)
        ylabel(AxisLabel(ivar), "Interpreter", "tex", "fontSize", fontSize)
        xlim(ZModel.(ZModel.ID(izModel)).RangeZ);
        %ylim(RangeZ);
        set(gca,"xscale",scale)
        set(gca,"yscale",scale)
        legend  ( string(length(Y.Data)) + " BATSE LGRBs" ...
                , "location" , Prop.(VarName(ivar)).legendLoc ...
                , "fontSize" , fontSize ...
                , "color" , "none" ...
                );
        %legend boxoff;
        %rho = sprintf("%0.2f", corr(log(X.Data),log(Y.Data)));
        %rhoPos = [0.05,40]; %[0.1,15];
        %text(rhoPos(1),rhoPos(2),["\rho = ",num2str(rho)],"HorizontalAlignment","left","Interpreter","tex", "fontSize" , fontSize )
        set(gca,"color","none", "fontSize", fontSize);
        set(gcf,"color","none");
        export_fig (fullfile(outDir,ZModel.ID(izModel)+VarName(ivar)+".png"),"-m2 -transparent");
        hold off;
        close(gcf);

    end
    
end

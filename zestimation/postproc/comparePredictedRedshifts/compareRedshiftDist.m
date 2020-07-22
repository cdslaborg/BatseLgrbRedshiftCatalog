% comparing old rival estimates with each other
close all;
clear all;
format compact; format long;
addpath(genpath('../')) % local lib codes
addpath(genpath('../../../../lib/matlab/')) % lib codes
%addpath(genpath('../../../../../libmatlab/'))

% change directory to the srouce code directory
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath); % Change working directory to source code directory.
cd(scriptPath); % Change working directory to source code directory.

fontSize = 13;
errBarLineWidth = 0.5;
kfac = "kfacOneThird";
confidence = 50;

outDirRoot = fullfile(scriptPath,"out");
mkdir(outDirRoot);

outDir = fullfile(outDirRoot,kfac,"PI"+num2str(confidence));
mkdir(outDir);

disp(['confidence level: ', num2str(confidence), '%']);
if confidence==50
    SNErrCol = {5,7}; % 50%
elseif confidence==90
    SNErrCol = {4,8}; % 90%
else
    error(['The requested confidence level is not supported:', num2str(confidence)]);
end

RangeZ = [0.04,60.0];
rootPath = "../../build/winx64/intel/19.0.4.245/release/static/heap/serial/fortran/" + kfac;
%rootPath = "../../build/winx64/intel/19.1.1.216/release/static/heap/serial/fortran/" + kfac;

% read rival data

ZModel.ID = ["H06","L08","B10","M14","M17","F18","B04","F00","Y04"];
ZModel.count = length(ZModel.ID);
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
ZModel.(ZModel.ID{1}) = importdata(fullfile(rootPath,"H06","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{2}) = importdata(fullfile(rootPath,"L08","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{3}) = importdata(fullfile(rootPath,"B10","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{4}) = importdata(fullfile(rootPath,"M14","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{5}) = importdata(fullfile(rootPath,"M17","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{6}) = importdata(fullfile(rootPath,"F18","bin","out","batse_zstat.txt"));
ZModel.(ZModel.ID{7}) = importdata("../../in/RedshiftBand2004.txt");
ZModel.(ZModel.ID{8}) = importdata("../../in/RedshiftFenimore2000.txt");
ZModel.(ZModel.ID{9}) = importdata("../../in/RedshiftYonetoku2004.txt");

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


counter = 0;
%Map = cell(ZModel.count*(ZModel.count-1)/2,1);
for iRival = 1:ZModel.count

    for jRival = iRival+1:ZModel.count

        % generate a map of shared triggers between the two datasets

        counter = counter + 1;
        %Map{counter} = mapTriggers( ZModel.(ZModel.ID{iRival}).data(:,1) , ZModel.(ZModel.ID{jRival}).data(:,1) );
        Map = mapTriggers( ZModel.(ZModel.ID{iRival}).data(:,1) , ZModel.(ZModel.ID{jRival}).data(:,1) );

        % generate data

        X.Data = ZModel.(ZModel.ID{iRival}).data( Map.Indx1 , ZModel.(ZModel.ID{iRival}).zcol );
        if iRival<7
            X.Err.Neg   = ZModel.(ZModel.ID{iRival}).data(Map.Indx1,ZModel.(ZModel.ID{iRival}).zcol) ...
                        - ZModel.(ZModel.ID{iRival}).data(Map.Indx1,SNErrCol{1});
            X.Err.Pos   = ZModel.(ZModel.ID{iRival}).data(Map.Indx1,ZModel.(ZModel.ID{iRival}).zcol) ...
                        - ZModel.(ZModel.ID{iRival}).data(Map.Indx1,SNErrCol{2});
            X.Err.Pos   = abs(X.Err.Pos);
        else
            X.Err.Neg   = zeros(length(X.Data),1);
            X.Err.Pos   = zeros(length(X.Data),1);
        end

        Y.Data = ZModel.(ZModel.ID{jRival}).data( Map.Indx2 , ZModel.(ZModel.ID{jRival}).zcol );
        if jRival<7
            Y.Err.Neg   = ZModel.(ZModel.ID{jRival}).data(Map.Indx1,ZModel.(ZModel.ID{jRival}).zcol) ...
                        - ZModel.(ZModel.ID{jRival}).data(Map.Indx1,SNErrCol{1});
            Y.Err.Pos   = ZModel.(ZModel.ID{jRival}).data(Map.Indx1,ZModel.(ZModel.ID{jRival}).zcol) ...
                        - ZModel.(ZModel.ID{jRival}).data(Map.Indx1,SNErrCol{2});
            Y.Err.Pos   = abs(Y.Err.Pos);
        else
            Y.Err.Neg   = zeros(length(Y.Data),1);
            Y.Err.Pos   = zeros(length(Y.Data),1);
        end

        figure('visible','off','Color','none');
        hold on; box on;

        line(RangeZ, RangeZ, 'color', [0.6 0.6 0.6], 'linewidth', 2);
        ErrPlot = errorbar  ( X.Data ...
                            , Y.Data ...
                            , Y.Err.Neg ...
                            , Y.Err.Pos ...
                            , X.Err.Neg ...
                            , X.Err.Pos ...
                            , '.', 'markersize', 8, 'color', [0 1 0] ...
                            , 'MarkerFaceColor' , [0.09 0.52 0.16] ...
                            , 'MarkerEdgeColor' , [0.09 0.52 0.16] ...
                            , 'CapSize', 0 ...
                            );
        ErrPlot.LineWidth = errBarLineWidth;
        xlabel("Predicted Redshift: " + ZModel.Ref(iRival), 'fontSize', fontSize)
        ylabel("Predicted Redshift: " + ZModel.Ref(jRival), 'fontSize', fontSize)
        xlim(RangeZ);
        ylim(RangeZ);
        set(gca,'xscale','log')
        set(gca,'yscale','log')
        legend  ( { 'equality line' , [num2str(length(Y.Data)),' BATSE LGRBs'] } ...
                , 'location' , 'southeast' ...
                , 'fontSize' , fontSize ...
                , 'color' , 'none' ...
                )
        %legend boxoff;
        rho = sprintf('%0.2f', corr(log(X.Data),log(Y.Data)));
        rhoPos = [0.05,40]; %[0.1,15];
        text(rhoPos(1),rhoPos(2),['\rho = ',num2str(rho)],'HorizontalAlignment','left','Interpreter','tex', 'fontSize' , fontSize )
        set(gca,'color','none', 'fontSize', fontSize)
        set(gcf,'color','none')
        export_fig ( fullfile(outDir,ZModel.ID(iRival)+ZModel.ID(jRival)+".png"), "-m2 -transparent" )
        hold off;
        close(gcf);

    end
    
end

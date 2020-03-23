% comparing old rival estimates with each other
close all;
clear all;
format compact; format long;
addpath(genpath('../')) % local lib codes
addpath(genpath('../../../../lib/matlab/')) % lib codes

% change directory to the srouce code directory
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath); % Change working directory to source code directory.
cd(scriptPath); % Change working directory to source code directory.

fontSize = 13;
errBarLineWidth = 0.5;
kfac = 'kfacOneThird';
confidence = 90;

outDir = [kfac,'/PI',num2str(confidence),'/'];
disp(['confidence level: ', num2str(confidence), '%']);
if confidence==50
    SNErrCol = {5,7}; % 50%
elseif confidence==90
    SNErrCol = {4,8}; % 90%
else
    error(['The requested confidence level is not supported:', num2str(confidence)]);
end
mkdir(outDir);

RangeZ = [0.04,60.0];
rootPath = ['../../winx64/intel/release/static/serial/',kfac,'/'];

% read rival data
ZModel.count = 6;
ZModel.Ref = { 'This Work (H06 Rate)' ...
             , 'This Work (L08 Rate)' ...
             , 'This Work (B10 Rate)' ...
             , 'Band (2004)' ...
             , 'Fenimore (2000)' ...
             , 'Yonetoku (2004)' ...
             };
ZModel.ID = {'S19H06','S19L08','S19B10','B04','F00','Y04'};
ZModel.(ZModel.ID{1}) = importdata([rootPath,'H06/bin/out/batse_zstat.txt']);
ZModel.(ZModel.ID{2}) = importdata([rootPath,'L08/bin/out/batse_zstat.txt']);
ZModel.(ZModel.ID{3}) = importdata([rootPath,'B10/bin/out/batse_zstat.txt']);
ZModel.(ZModel.ID{4}) = importdata(['../../in/RedshiftBand2004.txt']);
ZModel.(ZModel.ID{5}) = importdata(['../../in/RedshiftFenimore2000.txt']);
ZModel.(ZModel.ID{6}) = importdata(['../../in/RedshiftYonetoku2004.txt']);

% column numbers containing redshift data
ZModel.(ZModel.ID{1}).zcol = 2; % the column number of S19H06 redshift predictions
ZModel.(ZModel.ID{2}).zcol = 2; % the column number of S19L08 redshift predictions
ZModel.(ZModel.ID{3}).zcol = 2; % the column number of S19B10 redshift predictions
ZModel.(ZModel.ID{4}).zcol = 2; % the column number of Band 2004 redshift predictions
ZModel.(ZModel.ID{5}).zcol = 5; % the column number of Fenimore 2000 redshift predictions
ZModel.(ZModel.ID{6}).zcol = 5; % the column number of Yonetoku 2004 redshift predictions


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
        if iRival<4
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
        if jRival<4
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
        xlabel(['Predicted Redshift: ',ZModel.Ref{iRival}], 'fontSize', fontSize)
        ylabel(['Predicted Redshift: ',ZModel.Ref{jRival}], 'fontSize', fontSize)
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
        %export_fig ([ZModel.ID{iRival},'_',ZModel.ID{jRival},'.png'],'-m4 -transparent')
        export_fig ([outDir,ZModel.ID{iRival},ZModel.ID{jRival},'.png'],'-m2 -transparent')
        hold off;
        close(gcf);

    end
    
end

% comparing old rival estimates with each other 
close all;
clear all;
format compact; format long;
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath);
addpath(genpath('../../../../../lib/matlab/')) % lib codes

fontSize = 13;
synPath = '../../out/';
inPath = '../../in/';
zPath = '../../../zestimation/out/';

% import BATSE data
Dummy = importdata([inPath,'batse_1366_lgrb_pbol_epk_sbol(0.001,20000).txt']);
Batse.LogData = [ Dummy.data(:,2) ... % logPbol
                , Dummy.data(:,4) ... % logEpk
                , Dummy.data(:,3) ... % logSbol
                , Dummy.data(:,8) ... % logDur
                ];
Batse.Data = exp(Batse.LogData);
Batse.ngrb = length(Batse.Data(:,1));
Batse.Trigger = Dummy.data(:,1);

% import Amati relation data
Ghirlanda08 = importdata([inPath,'AmatiRelationGhirlanda2008.txt']);

% read synthetic data
ZModel.count = 3;
ZModel.Ref = { 'This Work (with H06 Rate)' ...
             , 'This Work (with L08 Rate)' ...
             , 'This Work (with B10 Rate)' ...
             ..., 'Band (2004)' ...
             ..., 'Fenimore (2000)' ...
             ..., 'Yonetoku (2004)' ...
             };
ZModel.IDsim = {'HB06','Li08','BB10'};%,'B04','F00','Y04'};
ZModel.ID = {'H06','L08','B10'};%,'B04','F00','Y04'};
nvar = 4;
VarPair = zeros(12,2);
VarPair(1:6,1:2)    = [ 1, 2 ... logLiso-logEpkz
                      ; 1, 3 ... logLiso-logEiso
                      ; 1, 4 ... logLiso-logDurz
                      ; 3, 2 ... logEiso-logEpkz
                      ; 3, 4 ... logEiso-logDurz
                      ; 4, 2 ... logEiso-logDurz
                      ];
VarPair(7:12,1:2) = VarPair(1:6,1:2) + 4;

VarName = {'Liso','Epkz','Eiso','T90z','Pbol','Sbol','Epk','T90'};
AxisLabel = { 'Bolometric Peak Luminosity: L_{iso} [ ergs/s ]' ...
            , 'Intrinsic Spectral Peak Energy: E_{pz} [ keV ]' ...
            , 'Isotropic Radiated Energy: E_{iso} [ ergs/s ]' ...
            , 'Intrinsic Duration: T_{90z} [ s ]' ...
            , 'Bolometric Peak Flux: P_{bol} [ ergs/s/cm^2 ]' ...
            , 'Observed Spectral Peak Energy: E_{p} [ keV ]' ...
            , 'Bolometric Fluence: S_{bol} [ ergs/cm^2 ]' ...
            , 'Observed Duration: T_{90} [ s ]' ...
            };
Log10VarLim =   [ 46, 56 ... log10Liso
                ; 0 , 5  ... log10Epkz
                ; 46, 56 ... log10Eiso
                ; -1, 4  ... log10Eiso
                ; -13, -2 ... log10Pbol
                ; -1 , 4 ... log10Epk
                ; -15, 0 ... log10Sbol
                ; -1, 4  ... log10Dur
                ];
VarLim =        [ 5.e46, 1.e55  ... log10Liso
                ; 1.e0 , 3.e4  ... log10Epkz
                ; 1.e46, 3.e56 ... log10Eiso
                ; 5.e-2, 2.e3  ... log10Durz
                ; 1.e-13, 1.e-2 ... log10Pbol
                ; 2.e-1 , 1.e4 ... log10Epk
                ; 1.e-13, 1.e0 ... log10Sbol
                ; 1.e-1, 5.e3  ... log10Dur
                ];

% read redshift grid data
MPC2CM = 3.09e24; % 1 Mega Parsec = MPC2CM centimeters.
LOGMPC2CMSQ4PI = log(4.0*pi) + 2.0*log(MPC2CM);  % log(MegaParsec2centimeters).
imodel = 1;
zgrid = importdata([zPath,'HB06/zgrid.txt']);
zgrid.count = length(zgrid.data);
zgrid.IntObsDiff = zeros(4,zgrid.count);    % terms to map observer-frame data to rest-frame, equivalent to logRestFrameProp-logObserverFrameProp
zgrid.IntObsDiff(1,:) = LOGMPC2CMSQ4PI + 2*zgrid.data(:,2);
zgrid.IntObsDiff(2,:) = log(zgrid.data(:,1)+1);
zgrid.IntObsDiff(3,:) = zgrid.IntObsDiff(1,:) - zgrid.IntObsDiff(2,:);
zgrid.IntObsDiff(4,:) = -zgrid.IntObsDiff(2,:);


nVarPair = length(VarPair);
for iVarPair = 1:1%nVarPair

    for imodel = 1:1%ZModel.count

        if ~isfield(ZModel,ZModel.ID{imodel})
            ZModel.(ZModel.ID{imodel}).Synthetic = importdata([synPath,ZModel.IDsim{imodel},'.csv']);
            ZModel.(ZModel.ID{imodel}).Synthetic.data(:,1:8) = exp( ZModel.(ZModel.ID{imodel}).Synthetic.data(:,1:8) );
        end

        synBegin = 1;
        synEnd = length(ZModel.(ZModel.ID{imodel}).Synthetic.data(:,1));%13660;
        ZModel.(ZModel.ID{imodel}).Synthetic.count = synEnd-synBegin+1;

        figure; hold on; colormap('cool');
        scatter ( ZModel.(ZModel.ID{imodel}).Synthetic.data(synBegin:synEnd, VarPair(iVarPair,1) ) ...
                , ZModel.(ZModel.ID{imodel}).Synthetic.data(synBegin:synEnd, VarPair(iVarPair,2) ) ...
                , 0.75*ones(ZModel.(ZModel.ID{imodel}).Synthetic.count,1) ...
                , ZModel.(ZModel.ID{imodel}).Synthetic.data(synBegin:synEnd,end) ...
                , '.' ..., 'filled' ...
                )
        CBar = colorbar;
        CBar.Label.String = 'Probability of Detection by BATSE LADs';
        CBar.Label.Interpreter = 'tex';
        CBar.Label.FontSize = fontSize;
        %plot( ZModel.(ZModel.ID{imodel}).Synthetic.data(synBegin:synEnd,3) ...
        %    , ZModel.(ZModel.ID{imodel}).Synthetic.data(synBegin:synEnd,2) ...
        %    , '.', 'MarkerSize', 0.5 ...
        %    )
        if iVarPair<7
            % plot BATSE data
            PropInt.count = zgrid.count * Batse.ngrb;
            PropInt.Cloud = zeros(4,PropInt.count);
            ColorMapCloud = zeros(PropInt.count,3);
            instanceCounter = 0;
            for igrb = 1:Batse.ngrb
                if mod(igrb,100)==0; disp(['processing igrb :',num2str(igrb)]); end
                zprobFilePath = [ZModel.(ZModel.ID{imodel}).zPath,'zprob_',sprintf('%04d',Batse.Trigger(igrb)),'.txt'];
                zprob = importdata(zprobFilePath);
                zprobCumSum = cumsum(zprob.data);
                zprobCumSum = zprobCumSum / zprobCumSum(end);
                q05loc = find(zprobCumSum>0.05); q05loc = q05loc(1);
                q95loc = find(zprobCumSum<0.95); q95loc = q95loc(1);
                zprobCount = q95loc - q05loc + 1;
                zprob = zprob.data(q05loc:q95loc);
                [ZprobOrdered,Indx] = sort(zprob,'ascend');
                ZprobColorMap = getUniColorMap('grey',zprobCount);
                ZprobColorMap = ZprobColorMap(Indx,1:3);
                for iz = q05loc:q95loc%1:zgrid.count
                    instanceCounter = instanceCounter + 1;
                    PropInt.Cloud(1:4,instanceCounter) = Batse.LogData(igrb,:)' + zgrid.IntObsDiff(:,iz);
                    ColorMapCloud(instanceCounter,1:3) = ZprobColorMap(iz,1:3);
                end
            end
            scatter ( exp(PropInt.Cloud(3,1:instanceCounter)) ...
                    , exp(PropInt.Cloud(2,1:instanceCounter)) ...
                    , 0.75*ones(instanceCounter,1) ...
                    ..., ColorMapCloud(1:instanceCounter,1:3) ...
                    , '.' ..., 'filled' ...
                    );
            set(gca,'xscale','log','fontsize',fontSize);
            set(gca,'yscale','log','fontsize',fontSize,'YDir','normal');
        elseif iVarPair==4
            errorbar( Ghirlanda08.data(:,5) ...
                    , Ghirlanda08.data(:,3) ...
                    , Ghirlanda08.data(:,4) ...
                    , Ghirlanda08.data(:,4) ...
                    , Ghirlanda08.data(:,6) ...
                    , Ghirlanda08.data(:,6) ...
                    , '.', 'MarkerSize', 11 ...
                    , 'LineWidth', 1 ...
                    , 'color', [0.2 0.2 0.2] ... 'black' ...
                    , 'CapSize', 0 ...
                    )
        elseif iVarPair>6
            plot( Batse.Data(:,VarPair(iVarPair-6,1)) ...
                , Batse.Data(:,VarPair(iVarPair-6,2)) ...
                , '.', 'MarkerSize', 6 ...
                , 'color', 'black' ...
                );
        end
        set(gca,'xscale','log','fontsize',fontSize);
        set(gca,'yscale','log','fontsize',fontSize);
        xlim( VarLim(VarPair(iVarPair,1),:) );
        ylim( VarLim(VarPair(iVarPair,2),:) );
        xlabel(AxisLabel{VarPair(iVarPair,1)}, 'Interpreter', 'tex', 'fontSize', fontSize);
        ylabel(AxisLabel{VarPair(iVarPair,2)}, 'Interpreter', 'tex', 'fontSize', fontSize);
        legend  ( {['Simulated LGRB: ',ZModel.ID{imodel},' Rate']} ...
                , 'location' , 'southeast' ...
                , 'fontSize' , fontSize ...
                , 'color' , 'none' ...
                , 'box', 'off' ...
                );
        hold off;

    end

end

return
% %ZModel.(ZModel.ID{4}) = importdata([rootPath,'in/RedshiftBand2004.txt']);
% %ZModel.(ZModel.ID{5}) = importdata([rootPath,'in/RedshiftFenimore2000.txt']);
% %ZModel.(ZModel.ID{6}) = importdata([rootPath,'in/RedshiftYonetoku2004.txt']);
% 
% % column numbers containing redshift data
% ZModel.(ZModel.ID{1}).zcol = 2; % the column number of S19H06 redshift predictions
% ZModel.(ZModel.ID{2}).zcol = 2; % the column number of S19L08 redshift predictions
% ZModel.(ZModel.ID{3}).zcol = 2; % the column number of S19B10 redshift predictions
% ZModel.(ZModel.ID{4}).zcol = 2; % the column number of Band 2004 redshift predictions
% ZModel.(ZModel.ID{5}).zcol = 5; % the column number of Fenimore 2000 redshift predictions
% ZModel.(ZModel.ID{6}).zcol = 5; % the column number of Yonetoku 2004 redshift predictions
% 
% 
% counter = 0;
% %Map = cell(ZModel.count*(ZModel.count-1)/2,1);
% for iRival = 1:ZModel.count
% 
%     for jRival = iRival+1:ZModel.count
% 
%         % generate a map of shared triggers between the two datasets
%         counter = counter + 1;
%         %Map{counter} = mapTriggers( ZModel.(ZModel.ID{iRival}).data(:,1) , ZModel.(ZModel.ID{jRival}).data(:,1) );
%         Map = mapTriggers( ZModel.(ZModel.ID{iRival}).data(:,1) , ZModel.(ZModel.ID{jRival}).data(:,1) );
% 
%         % generate data
%         X.Data = ZModel.(ZModel.ID{iRival}).data( Map.Indx1 , ZModel.(ZModel.ID{iRival}).zcol );
%         if iRival<4
%             X.Err.Neg   = ZModel.(ZModel.ID{iRival}).data(Map.Indx1,ZModel.(ZModel.ID{iRival}).zcol) ...
%                         - ZModel.(ZModel.ID{iRival}).data(Map.Indx1,SNErrCol{1});
%             X.Err.Pos   = ZModel.(ZModel.ID{iRival}).data(Map.Indx1,ZModel.(ZModel.ID{iRival}).zcol) ...
%                         - ZModel.(ZModel.ID{iRival}).data(Map.Indx1,SNErrCol{2});
%             X.Err.Pos   = abs(X.Err.Pos);
%         else
%             X.Err.Neg   = zeros(length(X.Data),1);
%             X.Err.Pos   = zeros(length(X.Data),1);
%         end
% 
%         Y.Data = ZModel.(ZModel.ID{jRival}).data( Map.Indx2 , ZModel.(ZModel.ID{jRival}).zcol );
%         if jRival<4
%             Y.Err.Neg   = ZModel.(ZModel.ID{jRival}).data(Map.Indx1,ZModel.(ZModel.ID{jRival}).zcol) ...
%                         - ZModel.(ZModel.ID{jRival}).data(Map.Indx1,SNErrCol{1});
%             Y.Err.Pos   = ZModel.(ZModel.ID{jRival}).data(Map.Indx1,ZModel.(ZModel.ID{jRival}).zcol) ...
%                         - ZModel.(ZModel.ID{jRival}).data(Map.Indx1,SNErrCol{2});
%             Y.Err.Pos   = abs(Y.Err.Pos);
%         else
%             Y.Err.Neg   = zeros(length(Y.Data),1);
%             Y.Err.Pos   = zeros(length(Y.Data),1);
%         end
% 
%         figure('visible','off','Color','none');
%         hold on; box on;
% 
%         line(RangeZ, RangeZ, 'color', [0.6 0.6 0.6], 'linewidth', 2);
%         ErrPlot = errorbar  ( X.Data ...
%                             , Y.Data ...
%                             , Y.Err.Neg ...
%                             , Y.Err.Pos ...
%                             , X.Err.Neg ...
%                             , X.Err.Pos ...
%                             , '.', 'markersize', 8, 'color', [0 1 0] ...
%                             , 'MarkerFaceColor' , [0.09 0.52 0.16] ...
%                             , 'MarkerEdgeColor' , [0.09 0.52 0.16] ...
%                             , 'CapSize', 0 ...
%                             );
%         ErrPlot.LineWidth = 0.25;
%         xlabel(['Predicted Redshift: ',ZModel.Ref{iRival}], 'fontSize', fontSize)
%         ylabel(['Predicted Redshift: ',ZModel.Ref{jRival}], 'fontSize', fontSize)
%         xlim(RangeZ);
%         ylim(RangeZ);
%         set(gca,'xscale','log')
%         set(gca,'yscale','log')
%         legend  ( { 'equality line' , [num2str(length(Y.Data)),' BATSE LGRBs'] } ...
%                 , 'location' , 'southeast' ...
%                 , 'fontSize' , fontSize ...
%                 , 'color' , 'none' ...
%                 )
%         %legend boxoff;
%         rho = sprintf('%0.2f', corr(log(X.Data),log(Y.Data)));
%         rhoPos = [0.05,40]; %[0.1,15];
%         text(rhoPos(1),rhoPos(2),['\rho = ',num2str(rho)],'HorizontalAlignment','left','Interpreter','tex', 'fontSize' , fontSize )
%         set(gca,'color','none', 'fontSize', fontSize)
%         set(gcf,'color','none')
%         %export_fig ([ZModel.ID{iRival},'_',ZModel.ID{jRival},'.png'],'-m4 -transparent')
%         export_fig ([ZModel.ID{iRival},ZModel.ID{jRival},'.png'],'-m2 -transparent')
%         hold off;
%         close(gcf);
% 
%     end
%     
% end

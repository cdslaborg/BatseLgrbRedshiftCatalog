close all;
clear all;
format compact; format long;
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath);
addpath(genpath('../../../../../libmatlab/')) % lib codes

zPlusOneRequested = 1;
bivarPlotsRequested = 1;
bivarFigExportRequested = 1;
histFigExportRequested = 1;
fontSize = 13;
kfacType = 'kfacOneThird';
synSamPath = ['../../build/winx64/intel/19.0.4.245/release/static/heap/serial/fortran/kfacOneThird/bin/out/'];
%outPath = '../../out/';
inPath = '../../in/';

% import BATSE data
Dummy = importdata([inPath,'batse_1366_lgrb_pbol_epk_sbol(0.001,20000).txt']);
Batse.LogData.Obs = [ Dummy.data(:,2) ... % logPbol
                    , Dummy.data(:,4) ... % logEpk
                    , Dummy.data(:,3) ... % logSbol
                    , Dummy.data(:,8) ... % logDur
                    ];
Batse.Data.Obs = exp(Batse.LogData.Obs);
Batse.ngrb = length(Batse.Data.Obs(:,1));
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
ZModel.ID = {'H06','L08','B10'};    %,'B04','F00','Y04'};
ZPath = cell(length(ZModel.ID),1);
for imodel = 1:length(ZModel.ID)
    ZPath{imodel} = ['../../../zestimation/build/winx64/intel/19.0.4.245/release/static/heap/serial/fortran/',kfacType,'/',ZModel.ID{imodel},'/bin/out/'];
    %ZPath{imodel} = ['../../../zestimation/winx64/intel/release/static/serial/',kfacType,'/',ZModel.ID{imodel},'/bin/out/'];
end

nvar = 4;
VarPair = zeros(20,2);
VarPair(1:6,1:2)    = [ 1, 2 ... logLiso-logEpkz
                      ; 1, 3 ... logLiso-logEiso
                      ; 1, 4 ... logLiso-logDurz
                      ; 3, 2 ... logEiso-logEpkz
                      ; 3, 4 ... logEiso-logDurz
                      ; 4, 2 ... logEiso-logDurz
                      ];
VarPair(7:12,1:2) = VarPair(1:6,1:2) + 4;
VarPair(13:20,1:2)  = [ 9, 1 ... redshift-logLiso
                      ; 9, 2 ... redshift-logLiso
                      ; 9, 3 ... redshift-logLiso
                      ; 9, 4 ... redshift-logLiso
                      ; 9, 5 ... redshift-logLiso
                      ; 9, 6 ... redshift-logEpkz
                      ; 9, 7 ... redshift-logEiso
                      ; 9, 8 ... redshift-logDurz
                      ];

VarName = {'Liso','Epkz','Eiso','T90z','Pbol','Sbol','Epk','T90','Redshift','RedshiftPlusOne'};%,'LumDis'};
AxisLabel = { 'Bolometric Peak Luminosity: L_{iso} [ ergs/s ]' ...
            , 'Intrinsic Spectral Peak Energy: E_{pz} [ keV ]' ...
            , 'Isotropic Radiated Energy: E_{iso} [ ergs/s ]' ...
            , 'Intrinsic Duration: T_{90z} [ s ]' ...
            , 'Bolometric Peak Flux: P_{bol} [ ergs/s/cm^2 ]' ...
            , 'Observed Spectral Peak Energy: E_{p} [ keV ]' ...
            , 'Bolometric Fluence: S_{bol} [ ergs/cm^2 ]' ...
            , 'Observed Duration: T_{90} [ s ]' ...
            , 'Redshift: z' ...
            , 'z + 1' ...
            };
Log10VarLim =   [ 46, 56 ... log10Liso
                ; 0 , 5  ... log10Epkz
                ; 46, 56 ... log10Eiso
                ; -1, 4  ... log10Eiso
                ; -13, -2 ... log10Pbol
                ; -1 , 4 ... log10Epk
                ; -15, 0 ... log10Sbol
                ; -1, 4  ... log10Dur
                ; -1, 1.5  ... redshift
                ; 0, 2  ... redshift+1
                ];
VarLim =        [ 5.e46, 1.e55  ... log10Liso
                ; 1.e0 , 3.e4  ... log10Epkz
                ; 1.e46, 3.e56 ... log10Eiso
                ; 5.e-2, 2.e3  ... log10T90z
                ; 1.e-13, 1.e-2 ... log10Pbol
                ; 2.e-1 , 1.e4 ... log10Epk
                ; 1.e-13, 1.e0 ... log10Sbol
                ; 1.e-1, 5.e3  ... log10Dur
                ; 1.e-1, 3.5e1  ... redshift
                ; 1.e0, 3.6e1  ... redshift+1
                ];


if ~exist(kfacType,'dir'); mkdir(kfacType); end

nVarPair = length(VarPair);
for iVarPair = 1:nVarPair

    disp(['processing variable pairs # ', num2str(iVarPair)]);

    for imodel = 1:ZModel.count

        if ~isfield(ZModel,ZModel.ID{imodel})

            ZModel.(ZModel.ID{imodel}).Synthetic = importdata([synSamPath,'syntheticSample',ZModel.ID{imodel},'.csv']);
            ZModel.(ZModel.ID{imodel}).Synthetic.data(:,1:8) = exp( ZModel.(ZModel.ID{imodel}).Synthetic.data(:,1:8) );

            % read redshift grid data
            MPC2CM = 3.09e24; % 1 Mega Parsec = MPC2CM centimeters.
            LOGMPC2CMSQ4PI = log(4.0*pi) + 2.0*log(MPC2CM);  % log(MegaParsec2centimeters).
            zestimationFilePath = getFullPath([ZPath{imodel},'batse_zstat.txt']);
            ZModel.(ZModel.ID{imodel}).zstat = importdata(zestimationFilePath);
            ZModel.(ZModel.ID{imodel}).zstat.count = length(ZModel.(ZModel.ID{imodel}).zstat.data(:,1));
            % terms to map observer-frame data to rest-frame, equivalent to logRestFrameProp-logObserverFrameProp
            ZModel.(ZModel.ID{imodel}).zstat.IntObsDiff = zeros(ZModel.(ZModel.ID{imodel}).zstat.count,4);
            ZModel.(ZModel.ID{imodel}).zstat.IntObsDiff(:,1) = LOGMPC2CMSQ4PI + 2*getLogLumDisWicMPC(ZModel.(ZModel.ID{imodel}).zstat.data(:,2)+1.0);
            ZModel.(ZModel.ID{imodel}).zstat.IntObsDiff(:,2) = log(ZModel.(ZModel.ID{imodel}).zstat.data(:,2)+1);
            ZModel.(ZModel.ID{imodel}).zstat.IntObsDiff(:,3) = ZModel.(ZModel.ID{imodel}).zstat.IntObsDiff(:,1) - ZModel.(ZModel.ID{imodel}).zstat.IntObsDiff(:,2);
            ZModel.(ZModel.ID{imodel}).zstat.IntObsDiff(:,4) = -ZModel.(ZModel.ID{imodel}).zstat.IntObsDiff(:,2)*0.66666666666;

            % compute Batse GRB Intrinsic Prob
            Batse.LogData.Int = Batse.LogData.Obs + ZModel.(ZModel.ID{imodel}).zstat.IntObsDiff;
            Batse.Data.Int = exp( Batse.LogData.Int );

        end

        synBegin = 1;
        synEnd = length(ZModel.(ZModel.ID{imodel}).Synthetic.data(:,1));%13660;
        ZModel.(ZModel.ID{imodel}).Synthetic.count = synEnd-synBegin+1;

        if bivarPlotsRequested

            if bivarFigExportRequested
                figure('visible','off','Color','none');
            else
                figure;
            end
            hold on; box on; colormap('cool');

            %Mask = zeros( length(ZModel.(ZModel.ID{imodel}).Synthetic.data(:,1)) , 1 ); Mask(synBegin:synEnd) = 1;
            Mask = ZModel.(ZModel.ID{imodel}).Synthetic.data(synBegin:synEnd,end)>-1;
            %Mask = ZModel.(ZModel.ID{imodel}).Synthetic.data(synBegin:synEnd,end)>0.5;

            VarPairX = VarPair(iVarPair,1);
            DataX = ZModel.(ZModel.ID{imodel}).Synthetic.data( Mask , VarPairX );
            if VarPairX==9 && zPlusOneRequested % corresponding to redshift variable
                DataX = DataX + 1.0;
                VarPairX = VarPairX + 1;
            end

            VarPairY = VarPair(iVarPair,2);
            DataY = ZModel.(ZModel.ID{imodel}).Synthetic.data( Mask , VarPairY );
            if VarPairY==9 && zPlusOneRequested % corresponding to redshift variable
                DataY = DataY + 1.0;
                VarPairY = VarPairY + 1;
            end

            scatter ( DataX ...
                    , DataY ...
                    ..., 0.75*ones(ZModel.(ZModel.ID{imodel}).Synthetic.count,1) ...
                    , 0.75*ones(sum(Mask),1) ...
                    , ZModel.(ZModel.ID{imodel}).Synthetic.data(Mask,end) ...
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
            %if iVarPair<7
            %    % plot BATSE data
            %    scatter ( Batse.Data.Int(:,VarPair(iVarPair,1)) ...
            %            , Batse.Data.Int(:,VarPair(iVarPair,2)) ...
            %            , 30*ones(Batse.ngrb,1) ...
            %            , 'black' ...
            %            ..., ColorMapCloud(1:instanceCounter,1:3) ...
            %            , '.' ..., 'filled' ...
            %            );
            %    set(gca,'xscale','log','fontsize',fontSize);
            %    set(gca,'yscale','log','fontsize',fontSize,'YDir','normal');
            if iVarPair==4
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
                        );
            %elseif iVarPair>6
            %    plot( Batse.Data.Obs(:,VarPair(iVarPair-6,1)) ...
            %        , Batse.Data.Obs(:,VarPair(iVarPair-6,2)) ...
            %        , '.', 'MarkerSize', 6 ...
            %        , 'color', 'black' ...
            %        );
            end
            set(gca,'xscale','log','fontsize',fontSize);
            set(gca,'yscale','log','fontsize',fontSize,'YDir','normal');

            xlim( VarLim(VarPairX,:) );
            ylim( VarLim(VarPairY,:) );
            xlabel(AxisLabel{VarPairX}, 'Interpreter', 'tex', 'fontSize', fontSize);
            ylabel(AxisLabel{VarPairY}, 'Interpreter', 'tex', 'fontSize', fontSize);
            legend  ( {['Simulated LGRB: ',ZModel.ID{imodel},' Rate']} ...
                    , 'location' , 'northeast' ...
                    , 'fontSize' , fontSize ...
                    , 'color' , 'none' ...
                    , 'box', 'off' ...
                    );

            set(gca, 'color', 'none', 'fontsize', fontSize);
            if bivarFigExportRequested
                fileName = [kfacType,'/',VarName{VarPairX},VarName{VarPairY},ZModel.ID{imodel},'.png'];
                export_fig (fileName,'-m2 -transparent');
                hold off; close(gcf);
            else
                hold off;
            end

        end % bivarPlotsRequested

    end % imodel

end

% get the hitogram data for z+1
ZDataAll.X = cell(ZModel.count,1);
ZDataAll.Y = cell(ZModel.count,1);
ZDataDetected.X = cell(ZModel.count,1);
ZDataDetected.Y = cell(ZModel.count,1);
ZRange = [-1 2]*log(10);
%ZRange = [0.1 100];
zBinWidth = 0.1;
for imodel = 1:ZModel.count

    % get histogram data for all data first:
    Mask = ZModel.(ZModel.ID{imodel}).Synthetic.data(:,end)>-0.5;
   %zdatadum = log( ZModel.(ZModel.ID{imodel}).Synthetic.data( Mask , end-1 ) + 1 );
    zdatadum = log( ZModel.(ZModel.ID{imodel}).Synthetic.data( Mask , end-1 ) );
    h = histogram( zdatadum );%, 'Normalization' , 'probability' );
    %h.BinLimits = [-1 2]*log(10);
    h.BinLimits = ZRange;
    h.BinWidth = zBinWidth;
    ZDataAll.X{imodel} = h.BinEdges(1:end-1);
    ZDataAll.Y{imodel} = h.Values * 1366 / length(ZModel.(ZModel.ID{imodel}).Synthetic.data(:,1)); %h.BinCounts;
    close(gcf);

    % get histogram data for BATSE detected LGRBs:
    Mask = ZModel.(ZModel.ID{imodel}).Synthetic.data(:,end)>0.5;
   %zdatadum = log( ZModel.(ZModel.ID{imodel}).Synthetic.data( Mask , end-1 ) + 1 );
    zdatadum = log( ZModel.(ZModel.ID{imodel}).Synthetic.data( Mask , end-1 ) );
    h = histogram( zdatadum );%, 'Normalization' , 'probability' );
    h.BinLimits = ZRange;
    h.BinWidth = zBinWidth;
    ZDataDetected.X{imodel} = h.BinEdges(1:end-1);
    ZDataDetected.Y{imodel} = h.Values * 1366 / length(ZModel.(ZModel.ID{imodel}).Synthetic.data(:,1)); %h.BinCounts;
    close(gcf);

end

% now make the plot of redshift histogram

if histFigExportRequested
    figure('visible','off','Color','none');
else
    figure;
end
hold on; box on;

    for imodel = 1:ZModel.count
        %disp(['plotting histogram for variable # ', num2str(iVarName)]);
        % reset MATLAB default color order
        ax = gca; ax.ColorOrderIndex = imodel;
        plot( exp(ZDataAll.X{imodel}) ...
            , ZDataAll.Y{imodel} ...
            , '-' , 'linewidth' , 2.5 );
        ax = gca; ax.ColorOrderIndex = imodel;
        plot( exp(ZDataDetected.X{imodel}) ...
            , ZDataDetected.Y{imodel} ...
            , ':', 'linewidth' , 2.5 );
        %hold on;
        %[f,xi] = ksdensity(zdatadum);
        %plot(exp(xi),f,'linewidth',3);
        %VarLim
    end

    %xlim( [1 50] );
    xlim( [0.1 20] );
    %ylim( VarLim(VarPair(iVarPair,2),:) );
    %xlabel('Redshift: z + 1', 'Interpreter', 'tex', 'fontSize', fontSize);
    xlabel('Redshift: z', 'Interpreter', 'tex', 'fontSize', fontSize);
    %ylabel('Normalized LGRB Rate: dN / d(log_{10}(z+1))  ', 'Interpreter', 'tex', 'fontSize', fontSize);
    ylabel('Normalized LGRB Rate: dN / d(log_{10}(z))  ', 'Interpreter', 'tex', 'fontSize', fontSize);
    legend  ( { [ZModel.ID{1},' Cosmic Rate'] ...
              , [ZModel.ID{1},' Observed Rate'] ...
              , [ZModel.ID{2},' Cosmic Rate'] ...
              , [ZModel.ID{2},' Observed Rate'] ...
              , [ZModel.ID{3},' Cosmic Rate'] ...
              , [ZModel.ID{3},' Observed Rate'] ...
              } ...
            , 'location' , 'northwest' ...
            , 'fontSize' , fontSize ...
            , 'color' , 'none' ...
            , 'box', 'off' ...
            );
    set(gca,'xscale','log','fontsize',fontSize);
    set(gca,'yscale','linear','fontsize',fontSize,'YDir','normal');
    set(gca,'color','none');

zdistOutPath = 'zdist/';
if ~exist(zdistOutPath,'dir'); mkdir(zdistOutPath); end;
if histFigExportRequested
    fileName = [zdistOutPath,'zdist.png'];
    export_fig (fileName,'-m4 -transparent');
    hold off; close(gcf);
else
    hold off;
end

% get statistics of different terms
MPC2CM = 3.09e24;   % 1 Mega Parsec = MPC2CM centimeters.
LOGMPC2CMSQ4PI = log(4.0*pi) + 2.0*log(MPC2CM);     % log(MegaParsec2centimeters).
Intrinsic = cell(ZModel.count,1);
Observed = cell(ZModel.count,1);
StaDev = cell(ZModel.count,1);
Resolution = zeros(ZModel.count,1);
Accuracy = cell(ZModel.count,1);
for imodel = 1:ZModel.count

    % cosmic rate events and statistics
    Intrinsic{imodel}.LogZPlusOne = ( ZModel.(ZModel.ID{imodel}).Synthetic.data( : , end-1 ) + 1.0 );
    Intrinsic{imodel}.LogLisoPbolDiff = LOGMPC2CMSQ4PI + 2 * getLogLumDisWicMPC( Intrinsic{imodel}.LogZPlusOne );
    Intrinsic{imodel}.LogZPlusOne = log( Intrinsic{imodel}.LogZPlusOne );
    Intrinsic{imodel}.LogEisoSbolDiff = Intrinsic{imodel}.LogLisoPbolDiff - Intrinsic{imodel}.LogZPlusOne;
    for ivar = 1:8
        fieldName = ['Log',VarName{ivar}];
        Intrinsic{imodel}.(fieldName) = log( ZModel.(ZModel.ID{imodel}).Synthetic.data( : , ivar ) );
    end

    % observed rate events and statistics
    Mask = ZModel.(ZModel.ID{imodel}).Synthetic.data(:,end)>0.5;
    Observed{imodel}.LogZPlusOne = Intrinsic{imodel}.LogZPlusOne(Mask);
    Observed{imodel}.LogLisoPbolDiff = Intrinsic{imodel}.LogLisoPbolDiff(Mask);
    Observed{imodel}.LogEisoSbolDiff = Intrinsic{imodel}.LogEisoSbolDiff(Mask);
    for ivar = 1:8
        fieldName = ['Log',VarName{ivar}];
        Observed{imodel}.(fieldName) = Intrinsic{imodel}.(fieldName)(Mask);
    end

    StaDev{imodel}.Int.log10ZPlusOne     = log10(exp(1)) * std(Intrinsic{imodel}.LogZPlusOne);
    StaDev{imodel}.Obs.log10ZPlusOne     = log10(exp(1)) * std(Observed{imodel}.LogZPlusOne);
    StaDev{imodel}.Int.log10LisoPbolDiff = log10(exp(1)) * std(Intrinsic{imodel}.LogLisoPbolDiff);
    StaDev{imodel}.Obs.log10LisoPbolDiff = log10(exp(1)) * std(Observed{imodel}.LogLisoPbolDiff);
    StaDev{imodel}.Int.log10EisoSbolDiff = log10(exp(1)) * std(Intrinsic{imodel}.LogEisoSbolDiff);
    StaDev{imodel}.Obs.log10EisoSbolDiff = log10(exp(1)) * std(Observed{imodel}.LogEisoSbolDiff);
    for ivar = 1:8
        fieldName = ['Log',VarName{ivar}];
        fieldNameStaDev = ['log10',VarName{ivar}];
        StaDev{imodel}.Int.(fieldNameStaDev) = log10(exp(1)) * std(Intrinsic{imodel}.(fieldName));
        StaDev{imodel}.Obs.(fieldNameStaDev) = log10(exp(1)) * std(Observed{imodel}.(fieldName));
    end

    % divide the standard deviation of the overall redshift distribution to
    % the mean of the predicted 90% intervals of individual Batse redshifts
    Resolution(imodel) = std(ZModel.(ZModel.ID{imodel}).Synthetic.data( Mask , end-1 )) / mean( ZModel.(ZModel.ID{imodel}).zstat.data(:,8) - ZModel.(ZModel.ID{imodel}).zstat.data(:,4) );
    Accuracy{imodel}.Obs.log10Liso = StaDev{imodel}.Obs.log10LisoPbolDiff / StaDev{imodel}.Obs.log10Liso;
    Accuracy{imodel}.Obs.log10Eiso = StaDev{imodel}.Obs.log10EisoSbolDiff / StaDev{imodel}.Obs.log10Eiso;
    Accuracy{imodel}.Obs.log10Epkz = StaDev{imodel}.Obs.log10ZPlusOne     / StaDev{imodel}.Obs.log10Epkz;
    Accuracy{imodel}.Obs.log10T90z = StaDev{imodel}.Obs.log10ZPlusOne     / StaDev{imodel}.Obs.log10T90z;
    Accuracy{imodel}.Int.log10Liso = StaDev{imodel}.Int.log10LisoPbolDiff / StaDev{imodel}.Int.log10Liso;
    Accuracy{imodel}.Int.log10Eiso = StaDev{imodel}.Int.log10EisoSbolDiff / StaDev{imodel}.Int.log10Eiso;
    Accuracy{imodel}.Int.log10Epkz = StaDev{imodel}.Int.log10ZPlusOne     / StaDev{imodel}.Int.log10Epkz;
    Accuracy{imodel}.Int.log10T90z = StaDev{imodel}.Int.log10ZPlusOne     / StaDev{imodel}.Int.log10T90z;

end


return





%h = histfit( ZModel.(ZModel.ID{imodel}).Synthetic.data( Mask , VarPair(iVarPair,1) ) , 328 , 'kernel' );
h = histogram( ZModel.(ZModel.ID{imodel}).Synthetic.data( Mask , VarPair(iVarPair,1) ) );
h.BinLimits = [0 100];
set(gca,'xscale','log','fontsize',fontSize);


hf = histfit( zdata ...
            , h.NumBins ...
            , 'kernel' );


% 2D plots
Mask = ZModel.H06.Synthetic.data(:,end)<=0.5;
plot( log10( ZModel.H06.Synthetic.data(Mask,3) ) ...
          , log10( ZModel.H06.Synthetic.data(Mask,2) ) ...
          , '.' ...
          )
hold on;
Mask = ZModel.H06.Synthetic.data(:,end)>0.5;
plot( log10( ZModel.H06.Synthetic.data(Mask,3) ) ...
          , log10( ZModel.H06.Synthetic.data(Mask,2) ) ...
          , '.' ...
          )
hold off;

Mask = ZModel.H06.Synthetic.data(:,end)>0.5;
histogram2( log10( ZModel.H06.Synthetic.data(Mask,3) ) ...
          , log10( ZModel.H06.Synthetic.data(Mask,2) ) ...
          , 'DisplayStyle', 'tile', 'ShowEmptyBins', 'off' ...
          );
colorbar; 
hold on;

plot( log10( Ghirlanda08.data(:,5) ) ...
	, log10( Ghirlanda08.data(:,3) ) ...
    , '.', 'markersize' , 60 ...
    )
hold off;


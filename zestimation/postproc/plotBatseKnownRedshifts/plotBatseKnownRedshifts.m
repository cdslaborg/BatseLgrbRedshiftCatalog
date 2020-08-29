close all;
%clear all;
format compact; format long;
filePath = mfilename('fullpath');
addpath(genpath("../lib/"));
%addpath(genpath('../../../../../lib/matlab/')) % lib codes
addpath(genpath('../../../../libmatlab/'))

% change directory to the srouce code directory
filePath = mfilename('fullpath');
[scriptPath,~,~] = fileparts(filePath); cd(scriptPath); % Change working directory to source code directory.
cd(scriptPath); % Change working directory to source code directory.
figureColor = "white";

inPath = "../../in/";
fontSize = 13;
errBarLineWidth = 1;
kfac = "kfacOneThird";
scale = "log";
%scale = "linear";
confidence = 90;
disp(["confidence level: ", num2str(confidence), "%"]);

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

rootPath = "../../build/winx64/intel/19.0.4.245/release/static/heap/serial/fortran/" + kfac;

figExportRequested = 1;
markerSize = 20;
if strcmp(scale,"log")
    RangeZ = [0.1,10.0];
elseif strcmp(scale,"linear")
    RangeZ = [0.1,4.0];
end

ZModel.Ref = [ "This Work (H06 Rate)"   ...
             , "This Work (L08 Rate)"   ...
             , "This Work (B10 Rate)"   ...
             , "This Work (M14 Rate)"   ...
             , "This Work (M17 Rate)"   ...
             ..., "This Work (F18 Rate)"   ...
             , "This Work (P15 Rate)"   ...
             , "Band (2004)"            ...
             , "Fenimore (2000)"        ...
             , "Yonetoku (2004)"        ...
             ];
ZModel.ID = ["H06","L08","B10","M14","M17","P15"]; %"F18"];%,"B04","F00","Y04"];
ZModel.count = length(ZModel.ID);

% read BATSE known redshifts

BatseZ = importdata(fullfile(inPath,"BatseKnownRedshifts.txt"));
Mask = BatseZ.data(:,1)==6665 | BatseZ.data(:,1)==6707;
BatseZ.data = BatseZ.data(~Mask,:);
BatseZ.count = length(BatseZ.data(:,1));
%BatseZ.data(1,2) = 0.84; % set the redshift of trigger 6225 its lower spctroscopic limit that most people assume as its redshift.

SumLogLike = cell(ZModel.count,1);

for imodel = 1:ZModel.count

    zDir = fullfile(rootPath,ZModel.ID(imodel),"bin","out");
    zgridFilePath = fullfile(zDir,"zgrid.txt");
    zgrid = importdata(zgridFilePath);

    %if ~isfield(ZModel,ZModel.ID(imodel))
        ZModel.(ZModel.ID(imodel)).zstat = importdata(fullfile(zDir,"batse_zstat.txt"));
        ZModel.(ZModel.ID(imodel)).zstat.count = length(ZModel.(ZModel.ID(imodel)).zstat.data(:,1));
    %end
    Map = mapTriggers( BatseZ.data(:,1) , ZModel.(ZModel.ID(imodel)).zstat.data(:,1) );
    X.Dat = BatseZ.data(Map.Indx1,2);
    X.Err.Neg = BatseZ.data(Map.Indx1,3);
    X.Err.Pos = BatseZ.data(Map.Indx1,4);

    % prepare predicted redshifts
    zpos = 2;
    Y.Dat = ZModel.(ZModel.ID(imodel)).zstat.data(Map.Indx2,zpos);
    Y.Err.Neg   = ZModel.(ZModel.ID(imodel)).zstat.data(Map.Indx2,zpos) ...
                - ZModel.(ZModel.ID(imodel)).zstat.data(Map.Indx2,SNErrCol{1});
    Y.Err.Pos   = ZModel.(ZModel.ID(imodel)).zstat.data(Map.Indx2,zpos) ...
                - ZModel.(ZModel.ID(imodel)).zstat.data(Map.Indx2,SNErrCol{2});
    Y.Err.Pos   = abs(Y.Err.Pos);

    % lets also get the likelihood of the known redshifts
    SumLogLike{imodel} = 0.0;
    for igrb = 1:BatseZ.count
        zdistFilePath = fullfile( zDir, "zprob_" + string(sprintf('%04d',BatseZ.data(igrb,1))) + ".txt" );
        if exist(zdistFilePath,"file")
            zdist = importdata(zdistFilePath);
            zdist.data = zdist.data / sum(zdist.data);
            notFound = 1;
            iz = 0;
            while (notFound)
                iz = iz + 1;
                if notFound && BatseZ.data(igrb,2)<=zgrid.data(iz,1)
                    SumLogLike{imodel} = SumLogLike{imodel} + log10(zdist.data(iz));
                    %disp(num2str(iz))
                    notFound = 0;
                end
                if iz>=length(zgrid.data)
                    error('fatal error occurred. This is serious!')
                end
            end
        else
            disp( "NOTE: " + zdistFilePath + " does not exist. skipping..." );
        end
    end

    if figExportRequested
        figure('visible','off','Color',figureColor);
    else
        figure('visible','on','Color',figureColor);
    end
    hold on; box on;

    % get sum of distance between predicted and measured redshift
%     DistSq = sum((Y.Dat-X.Dat).^2);
    
    
    line(RangeZ, RangeZ, 'color', [0.6 0.6 0.6], 'linewidth', 2);
    ErrPlot = errorbar  ( X.Dat ...
                        , Y.Dat ...
                        , Y.Err.Neg ...
                        , Y.Err.Pos ...
                        , X.Err.Neg ...
                        , X.Err.Pos ...
                        , '.', 'markersize', markerSize, 'color', [0 1 0] ...
                        , 'MarkerFaceColor' , [0.09 0.52 0.16] ...
                        , 'MarkerEdgeColor' , [0.09 0.52 0.16] ...
                        , 'CapSize', 0 ...
                        );
    ErrPlot.LineWidth = errBarLineWidth;
    xlabel("Associated Host Galaxy's Redshift", 'fontSize', fontSize)
    ylabel("Predicted Redshift: " + ZModel.Ref(imodel), "fontSize", fontSize)
    xlim(RangeZ);
    ylim(RangeZ);
    set(gca,'xscale',scale)
    set(gca,'yscale',scale)
    legend  ( [ "equality line" , string(length(Y.Dat)) + " BATSE LGRBs"] ...
            , 'location' , 'southeast' ...
            , 'fontSize' , fontSize ...
            , 'color' , figureColor ...
            );
        

        
%     dim = [.15 .4 .5 .5];
%     str = ['{D_i}^2: ',num2str(DistSq)];
%     annotation('textbox',dim,'String',str,'FitBoxToText','on');

    %legend boxoff;

%     rho = sprintf('%0.2f', corr(log(X.Dat),log(Y.Dat),'type','Pearson'));
%     rho = sprintf('%0.2f', corr(X.Dat,Y.Dat,'type','Pearson'));
%    rhoPos = [0.035,0.93]; %[0.1,15];
%     text( rhoPos(1), rhoPos(2)-0.08 ...
%        , ['\rho = ',num2str(rho)] ...
%        , 'HorizontalAlignment', 'left' ...
%        , 'units', 'normalized' ...
%        , 'Interpreter','tex', 'fontSize' , fontSize )
% 
% %    add the joint likelihood of measured redshifts
%    text( rhoPos(1), rhoPos(2) ...
%       , ['log_{10}\pi(Measured|Predicted) = ',sprintf('%.2f',round(SumLogLike{imodel},2))] ...
%       , 'HorizontalAlignment', 'left' ...
%       , 'units', 'normalized' ...
%       , 'Interpreter','tex', 'fontSize' , fontSize )
% 
%     set(gca,'color',figureColor, 'fontSize', fontSize)
%     set(gcf,'color',figureColor)
    
    
    if figExportRequested
        export_fig (fullfile(outDir,ZModel.ID(imodel)+"vsMeasuredZ.png"),"-m2 -transparent")
        hold off; close(gcf);
    else
        hold off;
    end

end


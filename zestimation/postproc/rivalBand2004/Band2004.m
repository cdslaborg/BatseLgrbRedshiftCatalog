close all;
clear all;
format compact; format long;
addpath(genpath('../')) % local lib codes
addpath(genpath('../../../../../lib/matlab/')) % lib codes
fontSize = 12;
%titleFontSize = fontSize - 2;

% read Yonetoku data
Rival = importdata('../../in/RedshiftBand2004.txt');

% read our predictions
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
end

imodelx = 2;
imodely = {1,3};
ErrCol = {4,8}; % 90%
%ErrCol = {5,7}; % 50%
icolSN = 2; % the column number of Shahmoradi Nemiroff redshift predictions
icolRival = 2; % the column number of Band 2004 redshift predictions
Map = cell(nModelSFR,1);

for imodel = 1:nModelSFR

    Map{imodel} = mapTriggers( StatZ{imodel}.data(:,1) , Rival.data(:,1) );

    %figure;
    figure('visible','off','Color','none');
    hold on; box on;
    dx = StatZ{imodel}.data( Map{imodel}.Indx1 , icolSN );
    dy = Rival.data( Map{imodel}.Indx2 , icolRival );
    yneg = zeros(length(dy),1);
    ypos = yneg;
    zmin = 0.04; %min( min(dx) , min(dy) );
    zmax = 60.0; %max( max(dx) , max(dy) );
    RangeZ = [zmin,zmax];
    line(RangeZ, RangeZ, 'color', 'black', 'linewidth', 2);
    p = errorbar( dx ...
            , dy ...
            , [] ... %yneg
            , [] ... %ypos
            ,     StatZ{imodelx}.data(Map{imodel}.Indx1,icolSN) - StatZ{imodelx}.data(Map{imodel}.Indx1,ErrCol{1}) ... %xneg
            , abs(StatZ{imodelx}.data(Map{imodel}.Indx1,icolSN) - StatZ{imodelx}.data(Map{imodel}.Indx1,ErrCol{2})) ... %xpos
            , '.', 'markersize', 7, 'color', 'red' ...
            , 'CapSize', 0 ...
            );
    p.LineWidth = 0.3;
    %plot( dx , dy ...
    %    , '.' , 'MarkerSize' , 8 ...
    %    )
    xlabel(['Predicted Redshift: This Work ( with ',nameOfSFR{imodel},' LGRB Rate )'], 'fontSize', fontSize)
    ylabel('Predicted Redshift: Band (2004)', 'fontSize', fontSize)
    xlim(RangeZ);
    ylim(RangeZ);
    set(gca,'xscale','log')
    set(gca,'yscale','log')
    legend  ( { 'equality line' , [num2str(length(dy)),' BATSE LGRBs'] } ...
            , 'location' , 'southeast' , 'fontSize' , fontSize , 'color' , 'none' ...
            )
    %legend boxoff;
    rho = sprintf('%0.2f', corr(log(dx),log(dy)));
    rhoPos = [0.05,40]; %[0.1,15];
    text(rhoPos(1),rhoPos(2),['\rho = ',num2str(rho)],'HorizontalAlignment','left','Interpreter','tex', 'fontSize' , fontSize )
    set(gca,'color','none')
    set(gcf,'color','none')
    export_fig ([nameOfSFR{imodel},'.png'],'-m4 -transparent')
    hold off;
    close(gcf);

end

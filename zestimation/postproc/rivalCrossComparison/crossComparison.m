% comparing old rival estimates with each other 
close all;
clear all;
format compact; format long;
addpath(genpath('../')) % local lib codes
addpath(genpath('../../../../../lib/matlab/')) % lib codes
fontSize = 12;
%titleFontSize = fontSize - 2;

% read rival data
Rival.count = 3;
Rival.Ref = {'Band (2004)','Fenimore (2000)','Yonetoku (2004)'};
Rival.Names = {'B04','F00','Y04'};
Rival.(Rival.Names{1}) = importdata('../../in/RedshiftBand2004.txt');
Rival.(Rival.Names{2}) = importdata('../../in/RedshiftFenimore2000.txt');
Rival.(Rival.Names{3}) = importdata('../../in/RedshiftYonetoku2004.txt');

% column numbers containing redshift data
Rival.(Rival.Names{1}).zicol = 2; % the column number of Band 2004 redshift predictions
Rival.(Rival.Names{2}).zicol = 5; % the column number of Fenimore 2000 redshift predictions
Rival.(Rival.Names{3}).zicol = 5; % the column number of Yonetoku 2004 redshift predictions


counter = 0;
Map = cell(Rival.count*(Rival.count-1)/2,1);
for iRival = 1:Rival.count

    for jRival = iRival+1:Rival.count

        counter = counter + 1;
        Map{counter} = mapTriggers( Rival.(Rival.Names{iRival}).data(:,1) , Rival.(Rival.Names{jRival}).data(:,1) );

        %figure;
        figure('visible','off','Color','none');
        hold on; box on;
        dx = Rival.(Rival.Names{iRival}).data( Map{counter}.Indx1 , Rival.(Rival.Names{iRival}).zicol );
        dy = Rival.(Rival.Names{jRival}).data( Map{counter}.Indx2 , Rival.(Rival.Names{jRival}).zicol );
        zmin = 0.04; %0.09; %min( min(dx) , min(dy) );
        zmax = 60.0; %20.0; %max( max(dx) , max(dy) );
        RangeZ = [zmin,zmax];
        line(RangeZ, RangeZ, 'color', 'black', 'linewidth', 2);
        plot    ( dx , dy ...
                , '.', 'markersize', 7, 'color', 'red' ...
                )
        xlabel(['Predicted Redshift: ',Rival.Ref{iRival}], 'fontSize', fontSize)
        ylabel(['Predicted Redshift: ',Rival.Ref{jRival}], 'fontSize', fontSize)
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
        export_fig ([Rival.Names{iRival},'_',Rival.Names{jRival},'.png'],'-m4 -transparent')
        hold off;
        close(gcf);

    end
    
end

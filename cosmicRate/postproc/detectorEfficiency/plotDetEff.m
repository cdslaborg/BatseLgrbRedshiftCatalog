close all;
clear all;
format compact; format long;
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath);
addpath(genpath('../../../../../lib/matlab/')) % lib codes

fontSize = 13;
outPath = './';
inPath = '../../in/';

pphRange = [0.1 10];
DetEff4B = importdata([inPath,'batseEfficiency4B.txt']); % Batse 4B catalog detection efficiency
DetEffSim.nmodel = 3;
DetEffSim.color = {'blue','red','black'};
DetEffSim.model = {'BATSE Catalog','LGRB Rate: H06','LGRB Rate: L08','LGRB Rate: B10'};
DetEffSim.avg = [-0.42 -0.47 -0.46];
DetEffSim.std = [0.14 0.12 0.12];
DetEffSim.pph = pphRange(1) : 0.02 : pphRange(2);
DetEffSim.eff = zeros(length(DetEffSim.pph),3);
for imodel = 1:DetEffSim.nmodel
    DetEffSim.eff(:,imodel) = 0.5 + 0.5 * erf((log10(DetEffSim.pph)-DetEffSim.avg(imodel))/(DetEffSim.std(imodel)*sqrt(2)));
end

figure('visible','off','Color','none');
hold on; box on; colormap('cool');
    plot( DetEff4B.data(:,7) ...
        , DetEff4B.data(:,9) ...
        , ':', 'linewidth', 3 ...
        , 'color', [0.5 0.5 0.5] ...
        )

    % reset MATLAB default color order
    ax = gca;
    ax.ColorOrderIndex = 1;

    for imodel = 1:DetEffSim.nmodel
        plot( DetEffSim.pph ...
            , DetEffSim.eff(:,imodel) ...
            , 'linewidth', 3 ...
            ..., 'color', DetEffSim.color{imodel} ...
            )
    end
    set(gca,'xscale','log');
    set(gca, 'fontsize',fontSize);
    xlim( pphRange );
    ylim( [0 1] );
    xlabel('1024ms Peak Photon Flux: P_{ph} [ ph / s / cm^2 ]', 'Interpreter', 'tex', 'fontSize', fontSize);
    ylabel('BATSE LGRB Detection Efficiency', 'Interpreter', 'tex', 'fontSize', fontSize);
    legend  ( DetEffSim.model ...
            , 'location' , 'southeast' ...
            , 'fontSize' , fontSize ...
            , 'color' , 'none' ...
            , 'box', 'off' ...
            );
    set(gca,'color','none');
export_fig ('BatseDetEff.png','-m2 -transparent')
hold off; close(gcf);

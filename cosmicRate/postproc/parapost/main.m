close all;
%clear all;
format compact; format long;
addpath(genpath('../../../../../lib/matlab/')) % lib codes
addpath(genpath('../../../../../20180101_ParaMonte/git/matlab/')) % ParaMonte lib codes

% change directory to the srouce code directory
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath); % Change working directory to source code directory.
cd(scriptPath); % Change working directory to source code directory.

fontSize = 15;
kfac = 'kfacOneThird';
outDir = [kfac,'/'];
mkdir(outDir)

Model.ID = {'H06','L08','B10'};
Model.count = 3;

paraPostReadRequested = true;
figExportRequested = true;
verboseRequested = true;
statRequested = true;

if paraPostReadRequested
    for imodel = 1:Model.count

        disp(['processing Parameter Posterior PDF data for model ',Model.ID{imodel},'...']);
        Model.(Model.ID{imodel}).dataDir = ['../../winx64/intel/release/static/serial/',kfac,Model.ID{imodel},'/bin/out/'];
        disp(['    input data path: ',Model.(Model.ID{imodel}).dataDir]);
        Model.(Model.ID{imodel}).ParaDRAM = ParaDRAM();
        Model.(Model.ID{imodel}).ParaDRAM.Output.read(Model.(Model.ID{imodel}).dataDir,verboseRequested,statRequested);

        % convert all location variables to scale of base 10
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.SampleLogFunc        = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.SampleLogFunc / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogLiso           = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogLiso    / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogEpkz           = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogEpkz    / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogEiso           = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogEiso    / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogT90z           = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogT90z    / log(10.0);
        
        % convert all scale variables to scale of base 10
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogLiso           = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogLiso ) / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogEpkz           = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogEpkz ) / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogEiso           = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogEiso ) / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogT90z           = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogT90z ) / log(10.0);

        % convert all Fisher z-values to correlations
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorLisoEpkz     = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorLisoEpkz );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorLisoEiso     = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorLisoEiso );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorLisoT90z     = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorLisoT90z );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorEpkzEiso     = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorEpkzEiso );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorEpkzT90z     = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorEpkzT90z );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorEisoT90z     = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorEisoT90z );
        
        % convert all BATSE threshold parameters to log10 scale
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogThresh         = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogThresh / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogThresh         = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.stdLogThresh ) / log(10.0);

        % now recompute all statistics
        ColHeader = {'SampleLogFunc','avgLogLiso','avgLogEpkz','avgLogEiso','avgLogT90z','stdLogLiso','stdLogEpkz','stdLogEiso','stdLogT90z' ...
                    ,'atanhCorLisoEpkz','atanhCorLisoEiso','atanhCorLisoT90z','atanhCorEpkzEiso','atanhCorEpkzT90z','atanhCorEisoT90z','avgLogThresh','stdLogThresh'};
        for icol = 1:length(ColHeader)
            Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol)      = mean( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.(ColHeader{icol}) );
            Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol)    = std( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.(ColHeader{icol}) );
        end

    end
end
cd(scriptPath); % Change working directory to source code directory.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate the parameter posterior PDF histograms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

HistLabelX =    { 'log_{10}( Posterior Probability Density )' ...
                , 'Mean: \mu( log_{10}( L_{iso} [ erg / s ] ) )' ...
                , 'Mean: \mu( log_{10}( E_{pz} [ keV ] ) )' ...
                , 'Mean: \mu( log_{10}( E_{iso} [ erg ] ) )' ...
                , 'Mean: \mu( log_{10}( T_{90z} [ s ] ) )' ...
                , 'Standard Deviation: \sigma( log_{10}( L_{iso} [ erg / s ] ) )' ...
                , 'Standard Deviation: \sigma( log_{10}( E_{pz} [ keV ] ) )' ...
                , 'Standard Deviation: \sigma( log_{10}( E_{iso} [ erg ] ) )' ...
                , 'Standard Deviation: \sigma( log_{10}( T_{90z} [ s ] ) )' ...
                , 'Pearson Correlation Coefficient: \rho( L_{iso} - E_{pz} )' ...
                , 'Pearson Correlation Coefficient: \rho( L_{iso} - E_{iso} )' ...
                , 'Pearson Correlation Coefficient: \rho( L_{iso} - T_{90z} )' ...
                , 'Pearson Correlation Coefficient: \rho( E_{iso} - E_{pz} )' ...
                , 'Pearson Correlation Coefficient: \rho( E_{pz} - T_{90z} )' ...
                , 'Pearson Correlation Coefficient: \rho( E_{iso} - T_{90z} )' ...
                , 'Detection Efficiency Mean: \mu_{th}' ...
                , 'Detection Efficiency Standard Deviation: \sigma_{th}' ...
                };

FigFileName =   { 'logFunc' ...
                , 'avgLogLiso' ...
                , 'avgLogEpkz' ...
                , 'avgLogEiso' ...
                , 'avgLogT90z' ...
                , 'stdLogLiso' ...
                , 'stdLogEpkz' ...
                , 'stdLogEiso' ...
                , 'stdLogT90z' ...
                , 'corLisoEpkz' ...
                , 'corLisoEiso' ...
                , 'corLisoT90z' ...
                , 'corEpkzEiso' ...
                , 'corEpkzT90z' ...
                , 'corEisoT90z' ...
                , 'avgLogThresh' ...
                , 'stdLogThresh' ...
                };

for icol = 1:length(ColHeader)

    if figExportRequested
        figure('visible','off','Color','none');
    else
        figure('visible','on'); %,'Color','none');
    end
    hold on; box on;

    for imodel = 1:Model.count

        histogram   ( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.(ColHeader{icol}) ...
                    , 'Normalization', 'pdf' ...
                    , 'EdgeColor', 'none' ...
                    )

    end

    xlabel(HistLabelX{icol}, 'fontSize', fontSize)
   %ylabel('Normalized Posterior PDF', 'fontSize', fontSize)
    ylabel('Normalized MCMC Count', 'fontSize', fontSize)
    legend  ( Model.ID ...
            , 'location' , 'northwest' ...
            , 'fontSize' , fontSize ...
            , 'color' , 'none' ...
            )

    if figExportRequested
        set ( gca , 'color' , 'none' , 'fontSize' , fontSize )
        export_fig ([outDir,FigFileName{icol},'.png'],'-m2 -transparent')
        hold off; close(gcf);
    else
        hold off;
    end

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate the latex table of parameter posterior PDF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp('generating the latex table of parameter posterior PDF...');

StatNameTex =   { '\mu_{\logten(\liso)}' ...
                , '\mu_{\logten(\epkz)}' ...
                , '\mu_{\logten(\eiso)}' ...
                , '\mu_{\logten(\durz)}' ...
                , '\sigma_{\logten(\liso)}' ...
                , '\sigma_{\logten(\epkz)}' ...
                , '\sigma_{\logten(\eiso)}' ...
                , '\sigma_{\logten(\durz)}' ...
                , '\rho_{\liso-\epkz}' ...
                , '\rho_{\liso-\eiso}' ...
                , '\rho_{\liso-\durz}' ...
                , '\rho_{\epkz-\eiso}' ...
                , '\rho_{\epkz-\durz}' ...
                , '\rho_{\eiso-\durz}' ...
                , '\mu_{th}' ...
                , '\mu_{th}' ...
                , '\sigma_{th}' ...
                };
                
lenLaTab = length(StatNameTex)+11;
LaTab = cell(lenLaTab,1);

irow = 1;
LaTab{irow}   = '\multicolumn{4}{c}{Location Parameters($\Mean$)} \\';
irow = irow + 1;
LaTab{irow}   = '\hline';

for icol = 1:4
    irow = irow + 1;
    LaTab{irow} = [ '$',StatNameTex{icol},'$ & $'...
                , sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.L08.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.L08.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ \\'];
end

irow = irow + 1;
LaTab{irow} = '\hline';
irow = irow + 1;
LaTab{irow} = '\multicolumn{4}{c}{Scale Parameters (diagonal elements of $\CovMat$)} \\';
irow = irow + 1;
LaTab{irow} = '\hline';

for icol = 5:8
    irow = irow + 1;
    LaTab{irow} = [ '$',StatNameTex{icol},'$ & $'...
                , sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.L08.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.L08.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ \\'];
end

irow = irow + 1;
LaTab{irow} = '\hline';
irow = irow + 1;
LaTab{irow} = '\multicolumn{4}{c}{Correlation Coefficients (non-diagonal elements of $\CovMat$)} \\';
irow = irow + 1;
LaTab{irow} = '\hline';

for icol = 9:14
    irow = irow + 1;
    LaTab{irow} = [ '$',StatNameTex{icol},'$ & $'...
                , sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.L08.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.L08.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ \\'];
end


irow = irow + 1;
LaTab{irow} = '\hline';
irow = irow + 1;
LaTab{irow} = '\multicolumn{4}{c}{BATSE Detection Efficiency (Sample Incompleteness)} \\';
irow = irow + 1;
LaTab{irow} = '\hline';

for icol = 15:16
    irow = irow + 1;
    LaTab{irow} = [ '$',StatNameTex{icol},'$ & $'...
                , sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.L08.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.L08.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Chain.Verbose.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Chain.Verbose.Stat.StaDev(icol),2)), '$ \\'];
end

% write to output file
fid = fopen([outDir,'tabParaPostContents.tex'],'w');
fprintf(fid,'%s\n',LaTab{:});
fclose(fid);

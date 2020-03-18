close all;
%clear all;
%clear classes;
format compact; format long;
addpath(genpath('../../../../../libmatlab/')) % lib codes
addpath(genpath('../../../lib/paramonte/')) % ParaMonte lib codes
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath); % Change working directory to source code directory.
cd(fileparts(mfilename('fullpath'))); % Change working directory to source code directory.

% change directory to the srouce code directory
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath); % Change working directory to source code directory.
cd(scriptPath); % Change working directory to source code directory.

fontSize = 15;
kfac = 'kfacOneThird';
outDir = [kfac,'/'];
mkdir(outDir)

Model.ID = {'H06','B10','M17'}; %, 'F18', 'L08'
Model.count = 3;

paraPostReadRequested = true;
figExportRequested = true;
verboseRequested = false;
statRequested = true;

if paraPostReadRequested
    for imodel = 1:Model.count

        disp(['processing Parameter Posterior PDF data for model ',Model.ID{imodel},'...']);
        Model.(Model.ID{imodel}).dataDir = ['../../build/linuxx64/intel/18.0.2.199/release/static/mpi/',kfac,Model.ID{imodel},'/romberg/bin/out/'];
        disp(['    input data path: ',Model.(Model.ID{imodel}).dataDir]);
        if ~isfield(Model.(Model.ID{imodel}),'ParaDRAM')
            Model.(Model.ID{imodel}).ParaDRAM = ParaDRAM();
            Model.(Model.ID{imodel}).ParaDRAM.Output.read(Model.(Model.ID{imodel}).dataDir,verboseRequested,statRequested);
        end

        % convert all location variables to scale of base 10
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.SampleLogFunc    = Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.SampleLogFunc / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogLiso       = Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogLiso    / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogEpkz       = Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogEpkz    / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogEiso       = Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogEiso    / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogT90z       = Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogT90z    / log(10.0);
        
        % convert all scale variables to scale of base 10
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogLiso    = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogLiso ) / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogEpkz    = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogEpkz ) / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogEiso    = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogEiso ) / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogT90z    = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogT90z ) / log(10.0);

        % convert all Fisher z-values to correlations
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorLisoEpkz = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorLisoEpkz );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorLisoEiso = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorLisoEiso );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorLisoT90z = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorLisoT90z );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorEpkzEiso = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorEpkzEiso );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorEpkzT90z = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorEpkzT90z );
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorEisoT90z = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.atanhCorEisoT90z );
        
        % convert all BATSE threshold parameters to log10 scale
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogThresh     = Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.avgLogThresh / log(10.0);
        Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogThresh  = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.logStdLogThresh ) / log(10.0);

        % now recompute all statistics
        ColHeader = {'SampleLogFunc','avgLogLiso','avgLogEpkz','avgLogEiso','avgLogT90z','logStdLogLiso','logStdLogEpkz','logStdLogEiso','logStdLogT90z' ...
                    ,'atanhCorLisoEpkz','atanhCorLisoEiso','atanhCorLisoT90z','atanhCorEpkzEiso','atanhCorEpkzT90z','atanhCorEisoT90z','avgLogThresh','logStdLogThresh'};
        for icol = 1:length(ColHeader)
            Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.Stat.Mean(icol)      = mean( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.(ColHeader{icol}) );
            Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.Stat.StaDev(icol)    = std( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.(ColHeader{icol}) );
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
                , 'logStdLogLiso' ...
                , 'logStdLogEpkz' ...
                , 'logStdLogEiso' ...
                , 'logStdLogT90z' ...
                , 'corLisoEpkz' ...
                , 'corLisoEiso' ...
                , 'corLisoT90z' ...
                , 'corEpkzEiso' ...
                , 'corEpkzT90z' ...
                , 'corEisoT90z' ...
                , 'avgLogThresh' ...
                , 'logStdLogThresh' ...
                };

for icol = 1:length(ColHeader)

    if figExportRequested
        figure('visible','off','Color','none');
    else
        figure('visible','on'); %,'Color','none');
    end
    hold on; box on;

    for imodel = 1:Model.count

        histogram   ( Model.(Model.ID{imodel}).ParaDRAM.Output.Sample.(ColHeader{icol}) ...
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
                , sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.M17.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.M17.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ \\'];
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
                , sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.M17.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.M17.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ \\'];
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
                , sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.M17.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.M17.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ \\'];
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
                , sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.H06.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.B10.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ & $' ...
                , sprintf('%.2f',round(Model.M17.ParaDRAM.Output.Sample.Stat.Mean(icol),2)), '\pm', sprintf('%.2f',round(Model.M17.ParaDRAM.Output.Sample.Stat.StaDev(icol),2)), '$ \\'];
end

% write to output file
fid = fopen([outDir,'tabParaPostContents.tex'],'w');
fprintf(fid,'%s\n',LaTab{:});
fclose(fid);

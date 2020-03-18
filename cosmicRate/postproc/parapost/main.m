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
if ~exist(outDir,'dir')
    mkdir(outDir)
end

Model.ID = {'H06', 'B10', 'M17', 'F18'}; %, 'L08'
Model.count = length(Model.ID);

figRequested = false;
paraPostReadRequested = false;
figExportRequested = true;
verboseRequested = true;
statRequested = true;

varnames = { 'SampleLogFunc' ...
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


if paraPostReadRequested
    for imodel = 1:Model.count

        disp(['processing Parameter Posterior PDF data for model ',Model.ID{imodel},'...']);

        Model.(Model.ID{imodel}).dataDir = ['../../build/linuxx64/intel/18.0.2.199/release/static/mpi/',kfac,Model.ID{imodel},'/romberg/bin/out/'];

        disp(['    input data path: ',Model.(Model.ID{imodel}).dataDir]);

        Model.(Model.ID{imodel}).ParaDRAM = ParaDRAM();
        Model.(Model.ID{imodel}).ParaDRAM.Output.read(Model.(Model.ID{imodel}).dataDir,verboseRequested,statRequested);

        % convert all location variables to scale of base 10
        Model.(Model.ID{imodel}).SampleLogFunc    = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.SampleLogFunc / log(10.0);
        Model.(Model.ID{imodel}).avgLogLiso       = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogLiso    / log(10.0);
        Model.(Model.ID{imodel}).avgLogEpkz       = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogEpkz    / log(10.0);
        Model.(Model.ID{imodel}).avgLogEiso       = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogEiso    / log(10.0);
        Model.(Model.ID{imodel}).avgLogT90z       = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogT90z    / log(10.0);
        
        % convert all scale variables to scale of base 10
        Model.(Model.ID{imodel}).stdLogLiso       = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.logStdLogLiso ) / log(10.0);
        Model.(Model.ID{imodel}).stdLogEpkz       = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.logStdLogEpkz ) / log(10.0);
        Model.(Model.ID{imodel}).stdLogEiso       = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.logStdLogEiso ) / log(10.0);
        Model.(Model.ID{imodel}).stdLogT90z       = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.logStdLogT90z ) / log(10.0);

        % convert all Fisher z-values to correlations
        Model.(Model.ID{imodel}).corLisoEpkz      = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorLisoEpkz );
        Model.(Model.ID{imodel}).corLisoEiso      = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorLisoEiso );
        Model.(Model.ID{imodel}).corLisoT90z      = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorLisoT90z );
        Model.(Model.ID{imodel}).corEpkzEiso      = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorEpkzEiso );
        Model.(Model.ID{imodel}).corEpkzT90z      = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorEpkzT90z );
        Model.(Model.ID{imodel}).corEisoT90z      = tanh( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.atanhCorEisoT90z );
        
        % convert all BATSE threshold parameters to log10 scale
        Model.(Model.ID{imodel}).avgLogThresh     = Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.avgLogThresh / log(10.0);
        Model.(Model.ID{imodel}).stdLogThresh     = exp( Model.(Model.ID{imodel}).ParaDRAM.Output.Chain.Verbose.logStdLogThresh ) / log(10.0);

        % now recompute all statistics
        for icol = 1:length(varnames)
            Model.(Model.ID{imodel}).stats.avg.(varnames{icol}) = mean( Model.(Model.ID{imodel}).(varnames{icol}) );
            Model.(Model.ID{imodel}).stats.std.(varnames{icol}) = std ( Model.(Model.ID{imodel}).(varnames{icol}) );
        end

    end
end
cd(scriptPath); % Change working directory to source code directory.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate the parameter posterior PDF histograms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

histLabelX =    { 'log_{10}( Posterior Probability Density )' ...
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

if figRequested
    for icol = 1:length(varnames)

        disp(['generting figure ',varnames{icol}]);

        if figExportRequested
            figure('visible','off','Color','none');
        else
            figure('visible','on'); %,'Color','none');
        end
        hold on; box on;

        h = cell(Model.count,1);
        maxBinWidth = -1;
        for imodel = 1:Model.count
            h{imodel} = histogram   ( Model.(Model.ID{imodel}).(varnames{icol}) ...
                                    , 'Normalization', 'pdf' ...
                                    , 'EdgeColor', 'none' ...
                                    );

            maxBinWidth = max( maxBinWidth, h{imodel}.BinWidth );
        end
        for imodel = 1:Model.count
            h{imodel}.BinWidth = 2 * maxBinWidth;
        end
        

        xlabel(histLabelX{icol}, 'fontSize', fontSize)
       %ylabel('Normalized Posterior PDF', 'fontSize', fontSize)
        ylabel('Normalized MCMC Count', 'fontSize', fontSize)
        if strcmp(varnames{icol}(1:3),'std') && ~ strcmp(varnames{icol}(1:3),'stdLogThresh')
            legendLocation = 'northeast';
        else
            legendLocation = 'northwest';
        end
        
        legend  ( Model.ID ...
                , 'location' , legendLocation ...
                , 'fontSize' , fontSize ...
                , 'color' , 'none' ...
                )

        if figExportRequested
            set ( gca , 'color' , 'none' , 'fontSize' , fontSize )
            export_fig ([outDir,varnames{icol},'.png'],'-m2 -transparent')
            hold off; close(gcf);
        else
            hold off;
        end

    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate the latex table of parameter posterior PDF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp('generating the latex table of parameter posterior PDF...');

texTabStatName= { '\mu_{\logten(\liso)}' ...
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
                , '\sigma_{th}' ...
                };
nvar = length(texTabStatName);

texTabEntry = cell(nvar,1);                
for ivar = 1:nvar
    texTabEntry{ivar} = [ '$', texTabStatName{ivar}, '$' ];
    for imodel = 1:Model.count
        texTabEntry{ivar} = [ texTabEntry{ivar}, ' & $' ...
                              , sprintf('%.2f',round(Model.(Model.ID{imodel}).stats.avg.(varnames{ivar+1}),2)), '\pm' ...
                              , sprintf('%.2f',round(Model.(Model.ID{imodel}).stats.std.(varnames{ivar+1}),2)), '$' ...
                              ];
    end
    texTabEntry{ivar} =  [ texTabEntry{ivar}, ' \\' ];
end

texTabLen = nvar + 26;
texTab = cell(texTabLen,1);

% generate table rows

citetalias = [];
for imodel = 1:Model.count
    if strcmp(Model.ID{imodel},'H06')
        citetalias = [ citetalias, ' & \citetalias{hopkins2006normalization}' ];
    elseif strcmp(Model.ID{imodel},'B10')
        citetalias = [ citetalias, ' & \citetalias{butler2010cosmic}' ];
    elseif strcmp(Model.ID{imodel},'M17')
        citetalias = [ citetalias, ' & \citetalias{madau2017radiation}' ];
    elseif strcmp(Model.ID{imodel},'F18')
        citetalias = [ citetalias, ' & \citetalias{fermi2018gamma}' ];
    else
        error(['unrecognized SFR model: ',Model.ID{imodel}]);
    end
end

irow = 1;
TAB = '    ';
texTab{irow} = ['\begin{table*}[t]%[tphb]']; irow = irow + 1;
texTab{irow} = [TAB,'\begin{center}']; irow = irow + 1;
texTab{irow} = [TAB,TAB,'\vspace{5mm}']; irow = irow + 1;
texTab{irow} = [TAB,TAB,'\caption{Mean best-fit parameters of the LGRB World Model, \eqref{eq:modelGeneric}, for the three redshift distribution scenarios considered.\label{tab:paraPostStat}}']; irow = irow + 1;
texTab{irow} = [TAB,TAB,'\begin{tabular}{', repmat('c ',1,Model.count+1),'}']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'Parameter', citetalias, ' \\']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\multicolumn{',num2str(Model.count+1),'}{c}{Location Parameters($\Mean$)} \\']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
for ivar = 1:4
texTab{irow} = [TAB,TAB,TAB,texTabEntry{ivar}]; irow = irow + 1;
end

texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\multicolumn{',num2str(Model.count+1),'}{c}{Scale Parameters (diagonal elements of $\CovMat$)} \\']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
for ivar = 5:8
texTab{irow} = [TAB,TAB,TAB,texTabEntry{ivar}]; irow = irow + 1;
end

texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\multicolumn{',num2str(Model.count+1),'}{c}{Correlation Coefficients (non-diagonal elements of $\CovMat$)} \\']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
for ivar = 9:14
texTab{irow} = [TAB,TAB,TAB,texTabEntry{ivar}]; irow = irow + 1;
end

texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\multicolumn{',num2str(Model.count+1),'}{c}{BATSE Detection Efficiency (Sample Incompleteness)} \\']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
for ivar = 15:16
texTab{irow} = [TAB,TAB,TAB,texTabEntry{ivar}]; irow = irow + 1;
end

texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
texTab{irow} = [TAB,TAB,TAB,'\hline']; irow = irow + 1;
texTab{irow} = [TAB,TAB,'\end{tabular}']; irow = irow + 1;
texTab{irow} = [TAB,'\end{center}']; irow = irow + 1;
texTab{irow} = [TAB,'{Note.--- The joint posterior distribution of the 16-dimensional parameters of the model resulting from the Markov chains are available for download at \url{https://github.com/shahmoradi/BatseRedshiftEstimates} for each of the three redshift distributions.}']; irow = irow + 1;
texTab{irow} = ['\end{table*}']; irow = irow + 1;


% write to output file
fid = fopen([outDir,'tabParaPost.tex'],'w');
fprintf(fid,'%s\n',texTab{:});
fclose(fid);

function read(ParaDRAMOutputObj,outputFileBase,verboseRequested,statRequested)
    % read - A method of class Folder. Given a Folder object, this function creates the corresponding directory for the path: ParaDRAMOutputObj.path.
    %
    % Syntax:  read(ParaDRAMOutputObj) or ParaDRAMOutputObj.read
    %
    % Inputs:
    %    ParaDRAMOutputObj - Folder object, containing the path of the directory and folder name to be created.
    %
    % Outputs:
    %    On output, a physical folder is created for the Folder object.
    %
    % Example: 
    %    No example available.
    %
    % Classes required: none
    % Other m-files required: none
    % Sub-functions: none
    % MAT-files required: none
    % Naming and abbreviation convention: See the project's README file for information.
    %
    % MATLAB version: 2016a
    %
    % See also: makeReadme
    %
    % Author: Amir Shahmoradi
    % Institute for Computational Engineering and Sciences (ICES)
    % The University of Texas at Austin
    % 201 E 24th Street, Stop C0200 | POB 6.328
    % Austin, TX 78712-1229
    % Phone: 512-232-7777
    % Email: amir@ices.utexas.edu
    % Email: amir@physics.utexas.edu
    % Email: a.shahmoradi@gmail.edu
    % Web: www.cdslab.org
    % Dec 2016; Last revision: 12-Jan-2017
    %
    %------------- BEGIN CODE --------------

    % get a list of the existing chain filenames
    if nargin>4
        error(['Number of input arguments to ParaDRAMOutput.read() is more than allowed.']);
    end

    if nargin<2
        ParaDRAMOutputObj.fileBase = ParaDRAMOutputObj.Default.fileBase;
    else
        if isempty(outputFileBase)
            ParaDRAMOutputObj.fileBase = ParaDRAMOutputObj.Default.fileBase;
        else
            ParaDRAMOutputObj.fileBase = outputFileBase;
        end
    end

    if nargin<3
        verboseRequested = 0; % This need improvement, for when it is not provided, it should choose the value smartly by detecting if the chain is verbose
        statRequested = 0;
    elseif nargin<4
        statRequested = 0;
    end

    % determine where the report files live
    [scriptPathDum,~,~] = fileparts(ParaDRAMOutputObj.fileBase);

    dummy = [ParaDRAMOutputObj.fileBase,'*',ParaDRAMOutputObj.Default.Suffix.reportFile];
    DummyFileList = dir(dummy);

    if isempty(DummyFileList)
        dummy = [ParaDRAMOutputObj.fileBase,'/*',ParaDRAMOutputObj.Default.Suffix.reportFile];
        DummyFileList = dir(dummy);
    end        

    if isempty(DummyFileList)

        error(['No file within or containing ''',dummy,''' exists.']);

    else

        ParaDRAMOutputObj.nReport = length(DummyFileList);
        disp([num2str(ParaDRAMOutputObj.nReport),' individual report files detected.']);
        for ifile = ParaDRAMOutputObj.nReport:-1:1

            % Construct Output struct
            disp(['    Processing output number ',num2str(ifile)]);
            ParaDRAMOutputObj.Report(ifile).File = DummyFileList(ifile);

            % find the corresponding chain file
            % MATLAB <2018 apparently does not have folder attribute in dir() output, so fix it here:
            if ~isfield(ParaDRAMOutputObj.Report(ifile).File,'folder')
                ParaDRAMOutputObj.Report(ifile).File.folder = scriptPathDum;
            end
            dummyChainFilename = [ ParaDRAMOutputObj.Report(ifile).File.folder , '/' , ParaDRAMOutputObj.Report(ifile).File.name(1:end-length(ParaDRAMOutputObj.Default.Suffix.reportFile)) , ParaDRAMOutputObj.Default.Suffix.chainFile ];
            DummyFile = dir(dummyChainFilename);
            if isempty(DummyFile)
                error(['No chain file containing ''',dummyChainFilename,''' could be found for the corresponding report file ''',ParaDRAMOutputObj.Report(ifile).File.name,'''.']);
            elseif length(DummyFile)>1
                error(['More than one (',length(DummyFile),') chain file containing ''',dummyChainFilename,''' were found for the corresponding report file ''',ParaDRAMOutputObj.Report(ifile).File.name,'''.']);
            end
            ParaDRAMOutputObj.Chain(ifile).File = DummyFile;
            filePath = [ ParaDRAMOutputObj.Report(ifile).File.folder , '/' , ParaDRAMOutputObj.Chain(ifile).File.name ];
            Dummy = importdata( filePath ); %, ' ' , 1 );
            ! find the column number corresponding to SampleWeight
            icolSampleWeight = 0;
            for i = 1:length(Dummy.colheaders)
                if strcmp(Dummy.colheaders{i},'SampleWeight')
                    icolSampleWeight = i;
                    break
                end
            end
            if icolSampleWeight==0
                error(['There is no column name corresponding to SampleWeight in the chain file ',filePath])
            end
            offset = icolSampleWeight + 1;
            dummySumWeights = sum(Dummy.data(:,icolSampleWeight));
            dummyLenData = length(Dummy.data(:,icolSampleWeight));
            ParaDRAMOutputObj.Chain(ifile).ncol = length(Dummy.colheaders);
            ParaDRAMOutputObj.Chain(ifile).nvar = ParaDRAMOutputObj.Chain(ifile).ncol - offset;
            ParaDRAMOutputObj.Chain(ifile).ColName = Dummy.colheaders;

            % For consistency in the extreme case of having extremely small-scale sampler:
            % One could simply assume that a Verbose chain is a compact chain with all weights being one, and then regenerate both chains.
%            if dummySumWeights == dummyLenData
%
%                disp(['        Chain file contains ',num2str(dummySumWeights),' in Verbose format.']);
%                for icol = 1:ParaDRAMOutputObj.Chain(ifile).ncol
%                    ParaDRAMOutputObj.Chain(ifile).Verbose.(Dummy.colheaders{icol}) = Dummy.data(:,icol);
%                end
%                ParaDRAMOutputObj.Chain(ifile).Verbose.nPoint = length(Dummy.data(:,end));
%
%                % initialize the compact arrays:
%                disp(['        Generating Chain structure in Compact format...']);
%                ParaDRAMOutputObj.Chain(ifile).Compact.nPoint = 1;
%                for icol = 1:ParaDRAMOutputObj.Chain(ifile).ncol
%                    ParaDRAMOutputObj.Chain(ifile).Verbose.(Dummy.colheaders{icol}) = zeros(ParaDRAMOutputObj.Chain(ifile).Verbose.nPoint,1);
%                end
%                % write the first row in compact arrays:
%                irow = 1;
%                for icol = 1:ParaDRAMOutputObj.Chain(ifile).ncol
%                    ParaDRAMOutputObj.Chain(ifile).Compact.(Dummy.colheaders{icol})(ParaDRAMOutputObj.Chain(ifile).Compact.nPoint) = Dummy.data(irow,icol);
%                end
%                for irow = 2:ParaDRAMOutputObj.Chain(ifile).Verbose.nPoint
%                    if all(Dummy.data(irow,:)==Dummy.data(irow-1,:))
%                        ParaDRAMOutputObj.Chain(ifile).Compact.(Dummy.colheaders{icolSampleWeight})(ParaDRAMOutputObj.Chain(ifile).Compact.nPoint) = ...
%                        ParaDRAMOutputObj.Chain(ifile).Compact.(Dummy.colheaders{icolSampleWeight})(ParaDRAMOutputObj.Chain(ifile).Compact.nPoint) + 1;
%                    else
%                        ParaDRAMOutputObj.Chain(ifile).Compact.nPoint = ParaDRAMOutputObj.Chain(ifile).Compact.nPoint + 1;
%                        for icol = 1:ParaDRAMOutputObj.Chain(ifile).ncol
%                            ParaDRAMOutputObj.Chain(ifile).Compact.(Dummy.colheaders{icol})(ParaDRAMOutputObj.Chain(ifile).Compact.nPoint) = Dummy.data(irow,icol);
%                        end
%                    end
%                end
%                % now resize the compact arrays appropriately:
%                for icol = 1:ParaDRAMOutputObj.Chain(ifile).ncol
%                    ParaDRAMOutputObj.Chain(ifile).Compact.(Dummy.colheaders{icol})  = ParaDRAMOutputObj.Chain(ifile).Compact.(Dummy.colheaders{icol})(1:ParaDRAMOutputObj.Chain(ifile).Compact.nPoint) ;
%                end
%
%                disp(['        Sample count in Compact Chain format: ',num2str(ParaDRAMOutputObj.Chain(ifile).Compact.nPoint)]);
%
%            else    % chain is in Compact format

                disp( ['        Chain file contains ' , num2str(dummyLenData),' in Compact format. Raw chain size: ' , num2str(dummySumWeights) ]);
                for icol = 1:ParaDRAMOutputObj.Chain(ifile).ncol
                    ParaDRAMOutputObj.Chain(ifile).Compact.(Dummy.colheaders{icol}) = Dummy.data(:,icol);
                end
                ParaDRAMOutputObj.Chain(ifile).Compact.nPoint = length(Dummy.data(:,end));

                % compute the stats of the compact chain if requested
                if statRequested
                    disp( ['        Computing the Compact Chain statistics (mean, stadev,covmat,cormat) ...' ]);
                    ParaDRAMOutputObj.Chain(ifile).Compact.Stat.VarName = cell(ParaDRAMOutputObj.Chain(ifile).ncol,1);
                    for icol = offset+1:ParaDRAMOutputObj.Chain(ifile).ncol
                        ParaDRAMOutputObj.Chain(ifile).Compact.Stat.VarName{icol-offset} = Dummy.colheaders{icol};
                    end
                    % compute the chain mean
                    ParaDRAMOutputObj.Chain(ifile).Compact.Stat.burninLocation  = max( [ 1, ParaDRAMOutputObj.Chain(ifile).Compact.BurninLocation(end) ] );
                    ParaDRAMOutputObj.Chain(ifile).Compact.Stat.nPointNonBurnin = ParaDRAMOutputObj.Chain(ifile).Compact.nPoint - ParaDRAMOutputObj.Chain(ifile).Compact.Stat.burninLocation + 1;
                    ParaDRAMOutputObj.Chain(ifile).Compact.Stat.Mean            = mean( Dummy.data(ParaDRAMOutputObj.Chain(ifile).Compact.Stat.burninLocation:end,offset+1:end))';
                    ParaDRAMOutputObj.Chain(ifile).Compact.Stat.StaDev          = std(  Dummy.data(ParaDRAMOutputObj.Chain(ifile).Compact.Stat.burninLocation:end,offset+1:end))';
                    ParaDRAMOutputObj.Chain(ifile).Compact.Stat.CorMat          = corr( Dummy.data(ParaDRAMOutputObj.Chain(ifile).Compact.Stat.burninLocation:end,offset+1:end))';
                    ParaDRAMOutputObj.Chain(ifile).Compact.Stat.CovMat          = cov(  Dummy.data(ParaDRAMOutputObj.Chain(ifile).Compact.Stat.burninLocation:end,offset+1:end))';
                end



                if verboseRequested

                    disp(['        Generating Chain structure in Verbose format...']);
                    ParaDRAMOutputObj.Chain(ifile).Verbose.nPoint = dummySumWeights;
                    for icol = 1:ParaDRAMOutputObj.Chain(ifile).ncol
                        ParaDRAMOutputObj.Chain(ifile).Verbose.(Dummy.colheaders{icol}) = zeros(ParaDRAMOutputObj.Chain(ifile).Verbose.nPoint,1);
                    end
                    ibegin = 1;
                    for irow = 1:ParaDRAMOutputObj.Chain(ifile).Compact.nPoint
                        iend = Dummy.data(irow,icolSampleWeight) + ibegin - 1;
                        for icol = 1:ParaDRAMOutputObj.Chain(ifile).ncol
                            ParaDRAMOutputObj.Chain(ifile).Verbose.(Dummy.colheaders{icol})(ibegin:iend) = Dummy.data(irow,icol);
                        end
                        ibegin = iend + 1;
                    end

                    disp(['        Sample count in Verbose Chain format: ',num2str(ParaDRAMOutputObj.Chain(ifile).Verbose.nPoint)]);

                    % compute the stats of the verbose chain if requested
                    if statRequested
                        disp( ['        Computing the Verbose Chain statistics (mean, stadev,covmat,cormat) ...' ]);
                        DumChain = zeros(ParaDRAMOutputObj.Chain(ifile).Verbose.nPoint,ParaDRAMOutputObj.Chain(ifile).ncol-offset);
                        ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.VarName = cell(ParaDRAMOutputObj.Chain(ifile).ncol,1);
                        for icol = offset+1:ParaDRAMOutputObj.Chain(ifile).ncol
                            DumChain(:,icol-offset) = ParaDRAMOutputObj.Chain(ifile).Verbose.(Dummy.colheaders{icol});
                            ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.VarName{icol-offset} = Dummy.colheaders{icol};
                        end
                        % compute the chain mean
                        ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.burninLocation  = max( [ 1, sum( ParaDRAMOutputObj.Chain(ifile).Compact.SampleWeight(1:ParaDRAMOutputObj.Chain(ifile).Compact.Stat.burninLocation) ) ] );
                        ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.nPointNonBurnin = ParaDRAMOutputObj.Chain(ifile).Verbose.nPoint - ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.burninLocation + 1;
                        ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.Mean            = mean( DumChain(ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.burninLocation:end,1:end))';
                        ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.StaDev          = std(  DumChain(ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.burninLocation:end,1:end))';
                        ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.CorMat          = corr( DumChain(ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.burninLocation:end,1:end))';
                        ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.CovMat          = cov(  DumChain(ParaDRAMOutputObj.Chain(ifile).Verbose.Stat.burninLocation:end,1:end))';
                    end

                end

%            end

            % find the corresponding progress file

            dummyProgressFilename = [ ParaDRAMOutputObj.Report(ifile).File.folder , '/' , ParaDRAMOutputObj.Report(ifile).File.name(1:end-length(ParaDRAMOutputObj.Default.Suffix.reportFile)) , ParaDRAMOutputObj.Default.Suffix.progressFile ];
            DummyFile = dir( dummyProgressFilename );
            if isempty(DummyFile)
                warning(['No progress file containing ''',dummyProgressFilename,''' could be found for the corresponding report file ''',ParaDRAMOutputObj.Report(ifile).File.name,'''.']);
            elseif length(DummyFile)>1
                error(['More than one (',length(DummyFile),') progress file containing ''',dummyProgressFilename,''' were found for the corresponding report file ''',ParaDRAMOutputObj.Report(ifile).File.name,'''.']);
            end
            if ~isempty(DummyFile)
                ParaDRAMOutputObj.Progress(ifile).File = DummyFile;
                filePath = [ ParaDRAMOutputObj.Report(ifile).File.folder , '/' , ParaDRAMOutputObj.Progress(ifile).File.name ];
                Progress = importdata( filePath );
                if isfield(Progress,'colheaders')
                    ParaDRAMOutputObj.Progress(ifile).ncol    = length(Progress.colheaders);
                    ParaDRAMOutputObj.Progress(ifile).ColName = Progress.colheaders;
                else
                    ParaDRAMOutputObj.Progress(ifile).ncol    = 0;
                    ParaDRAMOutputObj.Progress(ifile).ColName = [];
                end
                if isfield(Progress,'data')
                    ParaDRAMOutputObj.Progress(ifile).count   = length(Progress.data(:,1));
                else
                    ParaDRAMOutputObj.Progress(ifile).count   = 0;
                end
                for icol = 1:ParaDRAMOutputObj.Progress(ifile).ncol
                    ParaDRAMOutputObj.Progress(ifile).(ParaDRAMOutputObj.Progress(ifile).ColName{icol}) = Progress.data(:,icol);
                end
            end

            % read the sample file

            dummySampleFilename = [ ParaDRAMOutputObj.Report(ifile).File.folder , '/' , ParaDRAMOutputObj.Report(ifile).File.name(1:end-length(ParaDRAMOutputObj.Default.Suffix.reportFile)) , ParaDRAMOutputObj.Default.Suffix.sampleFile ];
            DummyFile = dir( dummySampleFilename );
            if isempty(DummyFile)
                warning(['No sample file containing ''',dummySampleFilename,''' could be found for the corresponding report file ''',ParaDRAMOutputObj.Report(ifile).File.name,'''.']);
                %ParaDRAMOutputObj.Sample(ifile).ncol = 0;
                %ParaDRAMOutputObj.Sample(ifile).nvar = 0;
                %ParaDRAMOutputObj.Sample(ifile).count = 0;
                %ParaDRAMOutputObj.Sample(ifile).ColName = [];
            elseif length(DummyFile)>1
                error(['More than one (',length(DummyFile),') sample file containing ''',dummySampleFilename,''' were found for the corresponding report file ''',ParaDRAMOutputObj.Report(ifile).File.name,'''.']);
            else
                ParaDRAMOutputObj.Sample(ifile).File = DummyFile;
                filePath = [ ParaDRAMOutputObj.Report(ifile).File.folder , '/' , ParaDRAMOutputObj.Sample(ifile).File.name ];
                Sample = importdata( filePath ); %, ' ' , 1 );
                if isfield(Sample,'colheaders')
                    ParaDRAMOutputObj.Sample(ifile).ncol    = length(Sample.colheaders);
                    ParaDRAMOutputObj.Sample(ifile).ColName = Sample.colheaders;
                else
                    ParaDRAMOutputObj.Sample(ifile).ncol    = 0;
                    ParaDRAMOutputObj.Sample(ifile).ColName = [];
                end
                if isfield(Sample,'data')
                    ParaDRAMOutputObj.Sample(ifile).nvar    = ParaDRAMOutputObj.Sample(ifile).ncol - 1;
                    ParaDRAMOutputObj.Sample(ifile).count   = length(Sample.data(:,1));
                else
                    ParaDRAMOutputObj.Sample(ifile).nvar    = 0;
                    ParaDRAMOutputObj.Sample(ifile).count   = 0;
                end
                for icol = 1:ParaDRAMOutputObj.Sample(ifile).ncol
                    ParaDRAMOutputObj.Sample(ifile).(ParaDRAMOutputObj.Sample(ifile).ColName{icol}) = Sample.data(:,icol);
                end
            end
        end


    end

%        ibegin = 1;
%        iend = length(ChainFiles)-1;
%        if iend<ibegin
%            return
%        end
%        ParaDRAMOutputObj.Chain = cell(ibegin:iend,1);
%        for ifile = ibegin:iend
%
%            if sum(Dummy(:,2))==length(Dummy(:,2))
%                Dummy = importdata( ChainFiles(ifile).name ); %, ' ' , 1 );
%            else
%            Chain{ifile} = zeros( sum(Dummy(:,2)) , length(Dummy(1,)) );
%    
%    
%            isample = 0;
%            for isampleWeighted = 1length(dw{ifile}(,1))
%                for iweight = 1dw{ifile}(isampleWeighted,2)
%                    isample = isample + 1;
%                    mcmc(isample,) = dw{ifile}(isampleWeighted,);
%                end
%            end
%            Progress = importdata( ProgressFiles(ifile).name ); %, ' ' , 1 );
%        end
%        Chain.ncol = length(Chain.data(1,));
%        for icol = 3Chain.ncol
%        
%            figure()
%            plot(Chain.data(,icol))
%            xlabel('MCMC Sample Number','fontsize',labelFontSize)
%            ylabel(Chain.textdata(icol),'fontsize',labelFontSize)
%            set(gca,'xscale','log')
%        
%            figure()
%            autocorr(Chain.data(,icol),length(Chain.data(,icol))-1)
%            xlabel('MCMC Sample Number','fontsize',labelFontSize)
%            ylabel(Chain.textdata(icol),'fontsize',labelFontSize)
%            set(gca,'xscale','log')
%        
%        end
%        Progress.ncol = length(Progress.data(1,));
%        for icol = 2Progress.ncol
%        
%            figure()
%            plot(Progress.data(,icol))
%            xlabel('MCMC Sample Number','fontsize',labelFontSize)
%            ylabel(Progress.textdata(icol),'fontsize',labelFontSize)
%            set(gca,'xscale','log')
%        
%        end
%        return
%        
%        newExpermient = false;
%        makeFrames = true;
%        expNum = 1; % experiment number
%        directory = '.';
%        label = 'GausBana2D';
%        
%        delimiterIn   = ' ';
%        headerlinesIn = 0;
%        x = importdata(strcat(directory,'sampleX.txt'),delimiterIn,headerlinesIn);
%        y = importdata(strcat(directory,'sampleY.txt'),delimiterIn,headerlinesIn);
%        z = importdata(strcat(directory,'sampleZ.txt'),delimiterIn,headerlinesIn);
%        %z = reshape(z,[551,551]);
%        %surf(x(1551),y(1551),z)
%        
%        % domain of functions
%        xmin=-8;xmax=6;ymin=-5.5;ymax=5.5;
%        x1 = xmin.02xmax; x2 = ymin.02ymax; % x1 represents -y axis
%        
%        % import MCMC sample data
%        file          = strcat(directory,ChainFiles);
%        delimiterIn   = ' ';
%        headerlinesIn = 1;
%        mcmc          = importdata(file,delimiterIn,headerlinesIn);
%        ncol          = length(mcmc.data(1,));
%        nsample       = length(mcmc.data(,1));

end
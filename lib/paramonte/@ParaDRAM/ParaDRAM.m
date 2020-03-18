% ParaDRAM - Class ParaDRAM properties and methods.
%
% Syntax:  ParaDRAM(path,code,info)
%
% Inputs:
%    path - An object of class String, containing system directory path to the inside of the ParaDRAM.
%    code - An object of class String, containing the source codes file names that generate the content of files in the content of this %
%    info - An object of class String, containing information about the ParaDRAM and its content (to appear in README file of the ParaDRAM).% Outputs:
%    To generate an empty ParaDRAM object, use ParaDRAM, or ParaDRAM() or ParaDRAM([],[],[]).
%
% Outputs:
%    on output, a ParaDRAM object is generated.
%
% Example: 
%    No example available.
%
% Classes required: none
% Other m-files required: none
% Subfunctions: none
% MAT-files required: none
% Naming and abbreviation convention: none
%
% MATLAB version: 2018a
%
% See also: none
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
% Web: www.shahmoradi.org
% Dec 2016; Last revision: 8-Jan-2017

%------------- BEGIN CODE --------------


classdef ParaDRAM < dynamicprops

    properties(Access = private)
        methodName = 'ParaDRAM';
    end

    properties
        Input = [];
        Output = [];
    end

    methods(Access = public)

        %function obj = ParaDRAM(inputFilePath,outputFileBase)
        function obj = ParaDRAM()
            % ParaDRAM(path,code,info): Constructor method. Generates a ParaDRAM object with the given input information.
            % To generate an empty ParaDRAM object, use ParaDRAM, or ParaDRAM() or ParaDRAM([],[],[]).
            %if nargin == 0
            %    warning(['no ',methodName,' input file path or output file base were provided by the user.']);
            %    disp([' ',methodName,' output files in current directory...']);
            %elseif nargin == 1
            %    if isempty(Simulation)
            %        warning(['no ',methodName,' simulation were provided by the user.'])
            %        disp(['Searching for all possible ',methodName,' output files in current directory...']);
            %    elseif isa(Simulation,'string')
            %        File = dir(Simulation);
            %        if isempty(File)
            %            error(['The input argument to ',methodName,' class constructor is a string, but does not correspond to any ',methodName,'  files.']);
            %        else
            %            for ifile = 1:length(File)
            %                if isempty()
            %            
            %            end
            %        end
            %        if isfolder(Simulation)
            %            disp(['Input directory given by the user.']);
            %            disp(['Searching for all possible ',methodName,' output files in current directory...']);
            %            
            %    end
            %else
            if nargin ~= 0
                error(['No input argument to ',methodName,' class constructor is allowed.']);
            end
            obj.Output = ParaDRAMOutput(obj.methodName);
        end

        %readChain(obj,filename)

    end % methods

end % class
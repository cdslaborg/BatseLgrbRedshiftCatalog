% ParaDRAMOutput - Class ParaDRAMOutput properties and methods.
%
% Syntax:  ParaDRAMOutput(path,code,info)
%
% Inputs:
%    path - An object of class String, containing system directory path to the inside of the ParaDRAM.
%    code - An object of class String, containing the source codes file names that generate the content of files in the content of this %
%    info - An object of class String, containing information about the ParaDRAM and its content (to appear in README file of the ParaDRAM).% Outputs:
%    To generate an empty Folder object, use Folder, or Folder() or Folder([],[],[]).
%
% Outputs:
%    on output, a Folder object is generated.
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
% MATLAB version: 2016a
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


classdef ParaDRAMOutput < dynamicprops

    properties(Access = private)
        Default = [];
    end


    properties
        fileBase    = [];
        Chain       = [];
        Sample      = [];
        Progress    = [];
        Report      = [];
        nReport     = [];
    end

    methods (Access = public)

        function ParaDRAMOutputObj = ParaDRAMOutput(methodName)
            % ParaDRAMOutput(path,code,info): Constructor method. Generates a ParaDRAMOutput object with the given input information.
            % To generate an empty Folder object, use Folder, or Folder() or Folder([],[],[]).
            %noFilePathMsg = ['No path was provided for ParaDRAMOutput object. ''', ParaDRAMOutputObj.Default.path, ''' will be used instead.'];
            %if nargin == 1
            %    ParaDRAMOutputObj.path = path;
            %elseif nargin == 0
            %    warning(noFilePathMsg);
            %    ParaDRAMOutputObj.path = [];
            %else
            %    error(['wrong number of input variables (',nargin,') to constructor method chain.']);
            %end
            %if isempty(ParaDRAMOutputObj.path)
            %    warning(noFilePathMsg);
            %    ParaDRAMOutputObj.path = ParaDRAMOutputObj.Default.path;
            %end
            ParaDRAMOutputObj.Default.fileBase            = [methodName,'*']  ;
            ParaDRAMOutputObj.Default.Suffix.chainFile    = '_chain.txt'      ;
            ParaDRAMOutputObj.Default.Suffix.progressFile = '_progress.txt'   ;
            ParaDRAMOutputObj.Default.Suffix.reportFile   = '_report.txt'     ;
            ParaDRAMOutputObj.Default.Suffix.sampleFile   = '_sample.txt'     ;
        end

        read(ParaDRAMOutputObj,outputFileBase,verboseRequested,statRequested);

    end % methods

end % class
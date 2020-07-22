close all;
clear all;
format compact; format long;
addpath(genpath('../../../../../lib/matlab/')) % lib codes
%addpath(genpath('../../../../../libmatlab/'))

% change directory to the srouce code directory
filePath = mfilename('fullpath');
[scriptPath,fileName,fileExt] = fileparts(filePath); cd(scriptPath); % Change working directory to source code directory.
cd(scriptPath); % Change working directory to source code directory.

inPath = '../../in/';

Batse.Ini = importdata([inPath,'BATSE_1366_LGRB_P1024ph_Epk_Sch23ph.txt']);
Batse.Bol = importdata([inPath,'batse_1366_lgrb_pbol_epk_sbol(0.001,20000).txt']);

AllData = [ Batse.Ini.data(:,1) ...
          , exp( Batse.Bol.data(:,2) ) ... Pbol
          , exp( Batse.Bol.data(:,3) ) ... Sbol
          , exp( Batse.Bol.data(:,4) ) ... Epk
          , exp( Batse.Bol.data(:,8) ) ... T90
          , exp( Batse.Ini.data(:,2) * log(10.0) ) ... Pph
          ];
AllData = sortrows(AllData,1);

ngrb = length(AllData);
LaTab = cell(ngrb,1);
for igrb = 1:ngrb
    LaTab{igrb} =   [ num2str(AllData(igrb,1)) ...
                    , ' & $' ...
                    ...
                    , sprintf('%.3E',AllData(igrb,2)) , '$ & $' ...
                    , sprintf('%.3E',AllData(igrb,3)) , '$ & $' ...
                    , sprintf('%.3E',AllData(igrb,4)) , '$ & $' ...
                    , sprintf('%.3E',AllData(igrb,5)) , '$ & $' ...
                    , sprintf('%.3E',AllData(igrb,6)) , '$ \\ [0.5ex]' ...
                    ];
end

% write to output file
fid = fopen(['tabBatseDataContents.tex'],'w');
fprintf(fid,'%s\n',LaTab{:});
fclose(fid);

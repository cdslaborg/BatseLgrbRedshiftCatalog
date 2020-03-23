% comparing old rival estimates with each other 
close all;
clear all;
format compact; format long;
addpath(genpath('../')) % local lib codes
addpath(genpath('../../../../lib/matlab/')) % lib codes

fontSize = 12;
SNErrCol = {4,8}; % 90%
%SNErrCol = {5,7}; % 50%
RangeZ = [0.04,60.0];
rootPath = '../../';

% read rival data
ZModel.count = 6;
ZModel.Ref = { 'H06 LGRB Rate' ...
             , 'L08 LGRB Rate' ...
             , 'B10 LGRB Rate' ...
             };
ZModel.ID = {'S19H06','S19L08','S19B10'};
ZModel.(ZModel.ID{1}) = importdata([rootPath,'out/HB06/batse_zdist.txt']);
ZModel.(ZModel.ID{2}) = importdata([rootPath,'out/Li08/batse_zdist.txt']);
ZModel.(ZModel.ID{3}) = importdata([rootPath,'out/BB10/batse_zdist.txt']);


figure;
hold on;
for iModel = 1:ZModel.count

    plot( ZModel.(ZModel.ID{iModel}).data(:,1) ...
        , ZModel.(ZModel.ID{iModel}).data(:,2) ...
        , 'linewidth', 2 ...
        )

end


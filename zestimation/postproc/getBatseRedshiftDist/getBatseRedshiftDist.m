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

kfac = "kfacOneThird";
rootPath = "../../build/winx64/intel/19.0.4.245/release/static/heap/serial/fortran/" + kfac;

ZModel.ID = ["H06","L08","B10","M14","M17","F18","P15"];%,"B04","F00","Y04"];
ZModel.count = length(ZModel.ID);

figure;
hold on;
for imodel = 1:ZModel.count
    zDir = fullfile(rootPath,ZModel.ID(imodel),"bin","out");
    ZModel.(ZModel.ID(imodel)) = importdata(fullfile(zDir,"batse_zdist.txt"));
    plot( ZModel.(ZModel.ID{imodel}).data(:,1) ...
        , ZModel.(ZModel.ID{imodel}).data(:,2) ...
        , 'linewidth', 2 ...
        );
end
xlim([0 7]);


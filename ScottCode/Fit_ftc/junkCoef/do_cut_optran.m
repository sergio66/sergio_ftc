% Program do_cut_optran.m
%
% Read in set1o and set3o optran files, cut the desired
% channels from each, and write out a single optran file.
%
% Expected filenames:
%    set1o list       : ../List/list1o
%    set3o list       : ../List/list3o
%    merged set1o data: merged_optran_fowp_<csname>.dat
%    merged set3o data: merged_optran_fmw_<csname>.dat
%
% Creates file : merged_optran_<csname>.dat
%

% Created: 17 April 2008, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code should not need to be modified

% Get channel set name
csname = input('Enter channel set name (m130x, m150, etc) : ','s');

% Read list files
load ../List/list1o
load ../List/list3o
id1 = round( list1o(:,1) ); % exact integer
id3 = round( list3o(:,1) ); % exact integer
clear list1o list3o
ichan = union(id1, id3);

nchan = length(ichan);

% Filenames
fname1    = ['merged_optran_fowp_' csname '.dat'];
fname3    = ['merged_optran_fmw_' csname '.dat'];
fname_out = ['merged_optran_' csname '.dat'];

% Read coef data files
set = 9; % optran
lmerged = 1; % true
[ichan1, fchan1, coef1, info1] = rdcoef(set, lmerged, fname1);
[ichan3, fchan3, coef3, info3] = rdcoef(set, lmerged, fname3);
ichan1 = round(ichan1); % exact integer
ichan3 = round(ichan3); % exact integer

% Create Empty output coef array
nlay = info1.nlay;
ncoef = info1.ncoefeach;
coef = zeros(nchan, nlay, ncoef);
fchan = zeros(nchan, 1);

% Pull out set1o data to keep
[junk, ikeep, junk2] = intersect(ichan1, id1);
[junk, iout, junk2] = intersect(ichan, id1);
coef(iout,:,:) = coef1(ikeep,:,:);
fchan(iout) = fchan1(ikeep);

% Pull out set3o data to keep
[junk, ikeep, junk2] = intersect(ichan3, id3);
[junk, iout, junk2] = intersect(ichan, id3);
coef(iout,:,:) = coef3(ikeep,:,:);
fchan(iout) = fchan3(ikeep);

% Create info_out
info_out = info1;
info_out.nchan = nchan;

% Write output file
[iok] = wrtcoef(ichan, fchan, coef, info_out, fname_out);
disp(['created ' fname_out ' containing ' int2str(nchan) ' channels'])

%%% end of program %%%

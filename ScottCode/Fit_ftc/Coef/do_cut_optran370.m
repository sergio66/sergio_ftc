% Program do_cut_optran370.m
%
% Read in set1o and set3o optran files, cut the desired
% channels from each, and write out a single optran file.
%
% Expected filenames:
%    set1o list       : ../List/list1o
%    set3o list       : ../List/list3o
%    merged set1o data: <csname>370_optran_fowp.dat
%    merged set3o data: <csname>370_optran_fmw.dat
%
% Creates file : merged_optran_<csname>.dat
%

% Created: 17 April 2008, Scott Hannon
% Update: 07 May 2008, S.Hannon - create "370" variant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code should not need to be modified

% Get channel set name
csname = input('Enter channel set name (m130x, m140x) : ','s');

% Read list files
load ../List/list1o
load ../List/list3o
id1 = round( list1o(:,1) ); % exact integer
id3 = round( list3o(:,1) ); % exact integer
clear list1o list3o
ichan = union(id1, id3);

nchan = length(ichan);

% Filenames
fname1    = [csname '370_optran_fow.dat'];
fname3    = [csname '370_optran_fmw.dat'];
fname_out = ['optran_' csname '370.dat'];

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

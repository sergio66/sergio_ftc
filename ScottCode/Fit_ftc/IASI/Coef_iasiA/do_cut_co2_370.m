% Program do_cut_co2_370.m
%
% Read in longco2 and shortco2 coef files, cut the desired
% channels from each, and write out a single co2 coef file.
%
% Expected filenames:
%    longco2 list  : ../List/listlongco2
%    shortco2 list : ../List/listshortco2
%    longco2 data  : <csname>_rawcoef_co2_fowp.dat
%    shortco2 data : <csname>_rawcoef_co2_fowp_sun.dat
%
% Creates file : ../CutCoef/co2_<csname>.dat
%

% Created: 18 April 2008, Scott Hannon - based on "do_cut_optran.m"
% Update: 07 May 2008, S.Hannon - create "370" variant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code should not need to be modified

% Get channel set name
csname = input('Enter channel set name (m130x, m140x) : ','s');

% Read list files
load ../List/listlongco2
load ../List/listshortco2
idl = round(  listlongco2(:,1) ); % exact integer
ids = round( listshortco2(:,1) ); % exact integer
clear listlongco2 listshortco2
ichan = union(idl, ids);

nchan = length(ichan);

% Filenames
fnamel    = [csname '_rawcoef370_co2_fowp.dat'];
fnames    = [csname '_rawcoef370_co2_fowp_sun.dat'];
fname_out = ['../CutCoef/co2_' csname '370.dat'];

% Read coef data files
set = 8; % CO2
lmerged = 0; % false
[ichanl, fchanl, coefl, infol] = rdcoef(set, lmerged, fnamel);
[ichans, fchans, coefs, infos] = rdcoef(set, lmerged, fnames);
ichanl = round(ichanl); % exact integer
ichans = round(ichans); % exact integer

% Create Empty output coef array
nlay = infol.nlay;
ncoef = infol.ncoefeach;
coef = zeros(nchan, nlay, ncoef);
fchan = zeros(nchan, 1);

% Pull out longco2 data to keep
[junk, ikeep, junk2] = intersect(ichanl, idl);
[junk, iout, junk2] = intersect(ichan, idl);
coef(iout,:,:) = coefl(ikeep,:,:);
fchan(iout) = fchanl(ikeep);

% Pull out shortco2 data to keep
[junk, ikeep, junk2] = intersect(ichans, ids);
[junk, iout, junk2] = intersect(ichan, ids);
coef(iout,:,:) = coefs(ikeep,:,:);
fchan(iout) = fchans(ikeep);

% Create info_out
info_out = infol;
info_out.nchan = nchan;

% Write output file
[iok] = wrtcoef(ichan, fchan, coef, info_out, fname_out);
disp(['created ' fname_out ' containing ' int2str(nchan) ' channels'])

%%% end of program %%%

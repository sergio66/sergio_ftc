% PROGRAM mat2dat.m
%
% Convert matlab NH3 coefs to binary fortran
%

% Created: 08 Jun 2011, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fin = input('Enter name of input mat file to read : ','s');
fout = input('Enter name of output dat file to create : ','s');

% Load input mat file
eval(['load ' fin]);

% Subset channels
fname = input('Enter name of NH3 channel list : ','s');
junk = load(fname);
idok = round(junk(:,1)); % exact integers
clear junk fname
[iok, i1, i2] = intersect(ichan,idok);
ichan = ichan(i1);
fchan = fchan(i1);
coef = allcoef(i1,:,:);

% Set required "info"
info.set = 10;
info.ncoef = 4;
info.nlay = 100;
info.nchan = length(ichan);

% Create output dat file
[iok] = wrtcoef(ichan, fchan, coef, info, fout);
if (iok == 0)
   disp('Error writing dat file')
else
  disp(['done writing NH3 dat file for ' int2str(iok) ' channels'])
end

%%% end of program %%%

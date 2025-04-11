function [ichan, fchan, coef] = rdcoef_so2(fname);

% function [ichan, fchan, coef] = rdcoef_so2(fname);
%
% Reads in binary FORTRAN offset SO2 data file created
% by program "wrtcoef_so2.m".
%
% Input:
%    fname = {string} name of binary FORTRAN data file to read
%
% Output:
%    ichan   = [nchan x 1] channel ID (PGE numbering)
%    fchan   = [nchan x 1] center frequency (wavenumber)
%    coef    = [nchan x 4] coeffcients for a x1000 change in SO2
%

% Created: 25 April 2003, Scott Hannon
% Update: 02 Jan 2008, S.Hannon - change nchan from hardcoeded to
%    determined from file size; fix ifm_exp (was wrong)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Number of coeffcients
ncoef = 4;

% Number of layers
nlay = 100;

% Expected value of ifm = 4*(1 + 1 + 4*100) = 1608
% This is 4 bytes each for (ichan, fchan, & 4 coefs)
ifm_exp = round( 4*(1 + 1 + ncoef*nlay) ); % exact integer

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d = dir(fname);
if (length(d) ~= 1)
   error(['unable to read file: ' fname])
end

junk = d.bytes/(ifm_exp + 8);
nchan = round(junk); % exact integer


% Open output file
fid=fopen(fname,'r','ieee-be');


% Dimension output arrays
ichan=zeros(nchan,1);
fchan=zeros(nchan,1);
coef=zeros(nchan,nlay,4);


% Loop over the channels
for ic=1:nchan

   % Read FORTRAN start-of-record marker
   [ifm,count]=fread(fid,1,'integer*4');
   if (count == 0)
      ic
      error('The FORTRAN data file contains fewer channels than expected')
   end
   if (ifm ~= ifm_exp)
      ifm
      ifm_exp
      error('FORTRAN start-of-record marker is unexpected size')
   end

   % Read data for this channel
   ichan(ic)=fread(fid,1,'integer*4');
   fchan(ic)=fread(fid,1,'real*4');
   coef(ic,:,:)=fread(fid,[4,100],'real*4')'; %'

   % Read FORTRAN end-of-record marker
   ifm=fread(fid,1,'integer*4');
   if (ifm ~= ifm_exp)
      ifm
      ifm_exp
      error('FORTRAN end-of-record marker is unexpected size')
   end

end % end of loop over bands

fclose(fid);

%%% end of function %%%

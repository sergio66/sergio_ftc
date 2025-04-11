function [ichan, fchan, coef] = rdcoef_n2o(fname);

% function [ichan, fchan, coef] = rdcoef_n2o(fname);
%
% Reads in binary FORTRAN offset N2O data file created
% by program "wrtcoef_n2o.m".
%
% Input:
%    fname = {string} name of binary FORTRAN data file to read
%
% Output:
%    ichan   = [nchan x 1] channel ID (PGE numbering)
%    fchan   = [nchan x 1] center frequency (wavenumber)
%    coef    = [nchan x 4] coeffcients for a x0.75 change in N2O
%

% Created: 28 June 2005, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Expected value of ifm = 4*(1 + 1 + 7*100) = 2808
% This is 4 bytes each for (ichan, fchan, & 7x100 coef*layer)
ifm_exp=2808;

nchan=398;
nlay=100;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Open output file
fid=fopen(fname,'r','ieee-be');


% Dimension output arrays
ichan=zeros(nchan,1);
fchan=zeros(nchan,1);
coef=zeros(nchan,nlay,7);


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
   coef(ic,:,:)=fread(fid,[7,100],'real*4')';

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

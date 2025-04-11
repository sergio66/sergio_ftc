function [idchan, freq, fcoef] = rd_therm6(fname);

% function [idchan, freq, fcoef] = rd_therm6(fname);
%
% Reads in binary FORTRAN refl thermal F-factor data file created
% by program "xwrt_therm.m". This is the "x" (6 term) version.
%
% Input:
%    fname = {string} name of binary FORTRAN data file to read
%
% Output:
%    idchan   = [nchan x 1] channel ID
%    freq     = [nchan x 1] center frequency
%    fcoef    = [nchan x 6] F-factor coeffcients
%

% Created: 16 April 2002, Scott Hannon
% Updated: 07 April 2005, Scott Hannon - updated for "x" verison
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Expected total number of channels
totchan=2378;

% Expected value of ifm = 4*(1 + 1 + 6) = 32
% This is 4 bytes each for (idchan, freq, & 6 coefs)
ifm_exp=32;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Open output file
fid=fopen(fname,'r','ieee-be');


% Dimension output arrays
idchan=zeros(totchan,1);
freq=zeros(totchan,1);
fcoef=zeros(totchan,6);


% Loop over the channels
for ic=1:totchan

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
   idchan(ic)=fread(fid,1,'integer*4');
   freq(ic)=fread(fid,1,'real*4');
   fcoef(ic,:)=fread(fid,[1,6],'real*4');

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

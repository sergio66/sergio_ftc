function [idchan, freq, layabove, fcoef] = rd_therm(fname);

% function [idchan, freq, layabove, fcoef] = rd_therm(fname);
%
% Reads in binary FORTRAN refl thermal F-factor data file created
% by program "wrt_therm.m".
%
% Input:
%    fname = {string} name of binary FORTRAN data file to read
%
% Output:
%    idchan   = [nchan x 1] channel ID
%    freq     = [nchan x 1] center frequency
%    layabove = [nchan x 1] layer above
%    fcoef    = [nchan x 5] F-factor coeffcients
%

% Created: 16 April 2002, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Expected total number of channels
totchan=2378;

% Expected value of ifm = 4*(1 + 1 + 1 + 5) = 32
% This is 4 bytes each for (idchan, freq, layabove, & 5 coefs)
ifm_exp=32;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Open output file
fid=fopen(fname,'r','ieee-be');


% Dimension output arrays
idchan=zeros(totchan,1);
freq=zeros(totchan,1);
layabove=zeros(totchan,1);
fcoef=zeros(totchan,5);


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
   layabove(ic)=fread(fid,1,'integer*4');
   fcoef(ic,:)=fread(fid,[1,5],'real*4');

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

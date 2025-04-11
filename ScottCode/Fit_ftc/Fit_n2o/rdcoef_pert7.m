function [ichan, fchan, coef] = rdcoef_pert7(fname);

% function [ichan, fchan, coef] = rdcoef_pert7(fname);
%
% Reads in binary FORTRAN data for perturbation gases using
% 7 predictors/coefs.
%
% Input:
%    fname = {string} name of binary FORTRAN data file to read
%
% Output:
%    ichan   = [nchan x 1] channel ID (PGE numbering)
%    fchan   = [nchan x 1] center frequency (wavenumber)
%    coef    = [nchan x 4] coeffcients
%

% Created: 25 April 2003, Scott Hannon
% Update: 15 Apr 2009, S.Hannon - minor update for cris
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nlay=100;

% Expected value of ifm = 4*(1 + 1 + 7*nlay)
% This is 4 bytes each for (ichan, fchan, & 7 coefs * nlay)
ifm_exp=4*(1 + 1 +7*nlay);

maxchan = 9000;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Open output file
fid=fopen(fname,'r','ieee-be');


% Dimension output arrays
ichan=zeros(maxchan,1);
fchan=zeros(maxchan,1);
coef=zeros(maxchan,nlay,4);


% Loop over the channels
ic = 0;
count = 1;
while (count > 0)
   % Read FORTRAN start-of-record marker
   [ifm,count]=fread(fid,1,'integer*4');
   if (count == 0)
      continue
   end
   if (ifm ~= ifm_exp)
      ifm
      ifm_exp
      error('FORTRAN start-of-record marker is unexpected size')
   end
   if (ic < maxchan)
      ic = ic + 1;
   else
      count = 0;
      disp('reached maxchan; quiting before end of data')
      continue
   end


   % Read data for this channel
   ichan(ic)=fread(fid,1,'integer*4');
   fchan(ic)=fread(fid,1,'real*4');
   coef(ic,:,:)=fread(fid,[7,nlay],'real*4')'; %'

   % Read FORTRAN end-of-record marker
   ifm=fread(fid,1,'integer*4');
   if (ifm ~= ifm_exp)
      ifm
      ifm_exp
      error('FORTRAN end-of-record marker is unexpected size')
   end

end % end of loop over bands

ind=1:ic;
ichan=ichan(ind);
fchan=fchan(ind);
coef=coef(ind,:,:);

fclose(fid);

%%% end of function %%%

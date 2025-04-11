function wrtcoef_guard(ichan, fchan, allcoef, outname);

% function wrtcoef_guard(ichan, fchan, allcoef, outname);
%
% Reads in a matlab file of offset guard channel coeffiencents and
% writes out a fortran binary data file for use with the
% fast model program.
%

% Created: 06 Nov 2009, Scott Hannon - created from wrtcoef_n2o_iasi.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[nchan, nlay, ncoef]=size(allcoef);

% Open fortran binary output file
fid=fopen(outname,'w','ieee-be');

% Record size for binary fortran file
ifm=4*(1 + 1 + ncoef*nlay); % (ichan=1, fchan=1, coefs=ncoefxnlay) 4 bytes each

% write out data to fortran file
for ic=1:nchan

   % Write to binary fortran file
   fwrite(fid,ifm,'integer*4');
   fwrite(fid,ichan(ic),'integer*4');
   fwrite(fid,fchan(ic),'real*4');
   for il=1:nlay
      fwrite(fid,allcoef(ic,il,1:ncoef),'real*4');
   end
   fwrite(fid,ifm,'integer*4');

end

% Close fortran file
fclose(fid);

%%% end of program %%%

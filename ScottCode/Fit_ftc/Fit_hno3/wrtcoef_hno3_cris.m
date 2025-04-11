% program wrtcoef_hno3
%
% Reads in a matlab file of offset HNO3 coeffiencents and
% writes out a fortran binary data file for use with the
% fast model program.
% NOTE: assumes the input MATLAB data is already "cut" to exclude
% unwanted channels.

% Created: 21 Apr 2009, Scott Hannon - created from wrtcoef_hno3_iasi.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read in matlab data
fname=input('Enter name of coef file to read (no .mat): ','s');
eval(['load ' fname])

[nchan, nlay, ncoef]=size(allcoef);
if (nlay ~= 100)
   error('Unexpected number of layers in coef data')
end
if (ncoef ~= 4)
   error('Unexpected number of coefficients in coef data')
end


outname=[fname '.dat']
listname=[fname '_list.txt'];

% Open fortran binary output file
fid=fopen(outname,'w','ieee-be');

% Record size for binary fortran file
ifm=4*(1 + 1 + 4*100); % (ichan=1, fchan=1, coefs=4x100) at 4 bytes each

% Initialize index array for list file
ncount=0;
ind=zeros(nchan,1);

iok=ones(nchan,1);

% write out data to fortran file
for ic=1:nchan
      % Write to binary fortran file
      fwrite(fid,ifm,'integer*4');
      fwrite(fid,ichan(ic),'integer*4');
      fwrite(fid,fchan(ic),'real*4');
      for il=1:100
         fwrite(fid,allcoef(ic,il,1:4),'real*4');
      end
      fwrite(fid,ifm,'integer*4');
      %
      % update list file index array
      ncount=ncount + 1;
      ind(ncount)=ic;
end

% Close fortran file
fclose(fid);

% Write list text file
junk=[ichan(ind(1:ncount)), fchan(ind(1:ncount)), maxhno3(ind(1:ncount))];
eval(['save ' listname ' junk -ascii'])


%%% end of program %%%

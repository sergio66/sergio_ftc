% program wrtcoef_ch4
%
% Reads in a matlab file of offset CH4 coeffiencents and
% writes out a fortran binary data file for use with the
% fast model program.
%

% Created: 21 Dec 2007, Scott Hannon - created from wrtcoef_so2.m
% Update: 03 Jun 2009, S.Hannon - modified IASI N2O version for CH4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit this section as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% minch4: minimum allowed maxch4 (skip channel if maxch4 < minch4)
%minch4 = 1.5E-5;
%minch4 = 1.495E-5;
minch4=1E-6;

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
if (ncoef ~= 7)
   error('Unexpected number of coefficients in coef data')
end


outname=[fname '.dat']
listname=[fname '_list.txt'];

% Open fortran binary output file
fid=fopen(outname,'w','ieee-be');

% Record size for binary fortran file
ifm=4*(1 + 1 + 7*100); % (ichan=1, fchan=1, coefs=4x100) at 4 bytes each

% Initialize index array for list file
ncount=0;
ind=zeros(nchan,1);


%%%
%iok=ones(nchan,1);
%%%
iok=zeros(nchan,1);
ii=find( fchan > 2400 );
iok(ii)=1;
%%%


% write out data to fortran file
for ic=1:nchan

   if (maxch4(ic) > minch4 & iok(ic) == 1)
      % Write to binary fortran file
      fwrite(fid,ifm,'integer*4');
      fwrite(fid,ichan(ic),'integer*4');
      fwrite(fid,fchan(ic),'real*4');
      for il=1:100
         fwrite(fid,allcoef(ic,il,1:7),'real*4');
      end
      fwrite(fid,ifm,'integer*4');
      %
      % update list file index array
      ncount=ncount + 1;
      ind(ncount)=ic;
   end

end

% Close fortran file
fclose(fid);

% Write list text file
junk=[ichan(ind(1:ncount)), fchan(ind(1:ncount)), maxch4(ind(1:ncount))];
eval(['save ' listname ' junk -ascii'])


%%% end of program %%%

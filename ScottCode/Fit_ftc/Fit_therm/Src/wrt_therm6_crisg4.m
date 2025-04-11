% program wrt_therm6_iasi
%
% Reads in matlab refl thermal F-factor fit results and
% writes out a fortran binary data file for use with the
% fast model program.
%

% Update: 07 April 2005, Scott Hannon - updated for "x" version
% Update: 19 April 2005, S.Hannon - set coef1 to 1 when all 6 coefs are zero
% Update: 25 April 2007, Scott Hannon - create "iasi" variant
% Update: 27 April 2009, S.Hannon - minor update for CrIS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit this section as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Name of input matlab data file to read
inname='therm_crisg4';


% Name of output binary fortran data file to create
outname='therm_crisg4.dat';


% Total number of channels expected
totchan=1329;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Open output file
fid=fopen(outname,'w','ieee-be');

% Record size
ifm=4*(1 + 1 + 6); % 4 bytes each * (idchan, freq, & 6 coefs)


   % Read in the matlab data
   eval(['load ' inname]);
   nchan=length(freq);

   % Make sure channel IDs are unique and sorted in ascending order
   [idsort,ind]=sort(idchan);
   if (length(unique(idchan)) ~= nchan)
      error('Some channel IDs are repeated in input matlab data file')
   end

   % write out data to fortran file
   for ii=1:nchan
      ic=ind(ii);

      fwrite(fid,ifm,'integer*4');
      fwrite(fid,idchan(ic),'integer*4');
      fwrite(fid,freq(ic),'real*4');

      % Prepare output for current channel
      junk = coefall(ic,:);
      jj = find(junk == 0);
      if (length(jj) == 6)
         % set coef(1)=1 so that F calc will always give 1
         junk(1) = 1;
      end
      fwrite(fid,junk,'real*4');

      fwrite(fid,ifm,'integer*4');
   end


fclose(fid);

if (nchan ~= totchan)
   nchan
   totchan
   error('Unexpected number of channels. Check & redo')
end

disp(['finished writing data to file ' outname ])

%%% end of program %%%

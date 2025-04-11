% program wrt_therm_airs
%
% Reads in matlab refl thermal F-factor fit results and
% writes out a fortran binary data file for use with the
% fast model program.
%

% Update: 07 April 2005, Scott Hannon - updated for "x" version
% Update: 19 April 2005, S.Hannon - set coef1 to 1 when all 6 coefs are zero
% Update: 22 Apr 2008, S.Hannon - partial re-write for April 2008 production
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit this section as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Total number of channels expected
totchan=2834;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% All channel IDs
allidchan = (1:totchan)'; %'
% Note: can treat idchan as index into allidchan


% Declare output data arrays
ncoef = 6;
allcoefall = zeros(totchan,ncoef);
allfreq    = zeros(totchan,1);
allrmsall  = zeros(totchan,1);
allrmsall2 = zeros(totchan,1);


% Read "long" band data
fname = input('Enter mat filename of long therm coef : ','s');
load(fname);
allcoefall(idchan,:) = coefall;
allfreq(idchan) = freq;
allidchan(idchan) = idchan;
allrmsall(idchan) = rmsall;
allrmsall2(idchan) = rmsall2;


% Read "short" band data
fname = input('Enter mat filename of short therm coef : ','s');
load(fname);
allcoefall(idchan,:) = coefall;
allfreq(idchan) = freq;
allidchan(idchan) = idchan;
allrmsall(idchan) = rmsall;
allrmsall2(idchan) = rmsall2;


% Open output file
outname = input('Enter name of fortran binary coef file to create : ','s');
fid=fopen(outname,'w','ieee-be');


% write out data to fortran file
ifm=4*(1 + 1 + 6); % 4 bytes each * (idchan, freq, & 6 coefs)
for ic=1:totchan
   fwrite(fid,ifm,'integer*4');
   fwrite(fid,ic,'integer*4');
   fwrite(fid,allfreq(ic),'real*4');
   %
   junk = allcoefall(ic,:);
   jj = find(junk == 0);
   if (length(jj) == 6)
      % set coef(1)=1 so that F calc will always give 1
      junk(1) = 1;
   end
   fwrite(fid,junk,'real*4');
   %
   fwrite(fid,ifm,'integer*4');
end

fclose(fid);

disp(['finished writing data to file ' outname ])

junk = [allidchan, allfreq, allrmsall, allrmsall2];
outname = input('Enter name of rms text file to create : ','s');
eval(['save ' outname ' junk -ascii'])

%%% end of program %%%

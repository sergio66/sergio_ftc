function [ok]=doall_wrtconvdat(band,comment, pnums, rtpfile, matfile, outfile);

% function [ok]=doall_wrtconvdat(band,comment,pnums,rtpfile,matfile,outfile);
%
% Routine to merge the matlab convolved l-to-s trans data (created
% by the "runtrans" kcarta & matlab script) and the fortran profile
% data (from an RTP file) and write out a fortran binary data
% file with all the data in the format needed for use with our
% fast model regression programs.  It loops over all the regression
% profiles and writes a sperate output file for each profile.
%
% Input:
%    band = {string} band for typeband {'', '_long', or '_short'}
%    comment = {string (max 35 char)}, comment string which will be
%       included in the comment/title portion of the output files.
%       The profile number will be appended to the end as " p<#>".
%       Example: 'IASI, 2cm Gauss, Dec07, FOW, p13'
%    pnums = [1 x nprof] list of integer profile numbers {eg [1:48]}
%    rtpfile = {string} name of RTP file with regression data profile
%    matfile = {string} name prefix of matlab convolved trans files.
%       The file names are assumed to be:
%       <whatever><pnum>'.mat'
%    outfile = string, name prefix of fortran output file to create.
%       The output file names will be:
%       <outfile><pnum>'.dat'
%
% Output:
%    ok = integer, 0 if the function detects a problem, otherwise 1
%
% Note: uses function "wrt_convdat.m".
%

% Original created by Scott Hannon, 6 August 1999
% Extensively revised to read in the profile data from RTP; April 2002
% Update: 9 May 2002, Scott Hannon - fix bug & check comment
% Update: 18 December 2007, S.Hannon - mods for IASI Dec07 production; add
%    subsetting for angles and channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info for supported typeband
alltypeband={'fow_long', 'fow_short', 'fmw', 'fcow'};
allngas=[      4,            4          3,     4]; 
allgasids=zeros( length(alltypeband), max(allngas) );
allgasids(1,1:4)=[2, 3, 1, -2];   % fow_long
allgasids(2,1:4)=[2, 3, 1, -2];   % fow_short
allgasids(3,1:3)=[2, 6, 1];       % fmw (long)
allgasids(4,1:4)=[2, 5, 3, 1];    % fcow (short)
allnangles      =[6, 12, 6, 12];  % number of angles
allidfirst      =[1,    4671, 2141, 5041];  % first channel ID
allidlast       =[6000, 8461, 4300, 6561];  % last channel ID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ok=0;

% Check comment
if (length(comment) > 35)
   error('comment string is too long; max allowed length is 35 char')
end


% Read the RTP profile data
addpath /asl/matlab/h4tools
[head, hattr, prof, pattr] = rtpread(rtpfile);
if (head.ptype ~= 1)
   error('rtpfile is not a "layers" profile')
end

% Loop over the profiles
for ip=1:length(pnums)
   ipnum=pnums(ip);
   disp(['processing profile ' int2str(ipnum)])

   %%%%%%%%%%%%%%%%%%%%%%%%%
   % Read in the matlab data
   % The matfile must contain:
   %       ctrans(nchan x [nang,nlay,nsets]) {layers bot to top}
   %       secang(nang)
   %       fchan(nchan)
   %       ichan(nchan)
   %       typeband
   %       nchan, nang, nlay, nsets
   mfile=[matfile int2str(ipnum) '.mat'];
   djunk=dir(mfile);
   [irow,icol]=size(djunk);
   if (irow ~= 1)
      error(['Error openning matlab file ' mfile])
   end
   clear djunk irow icol
   eval(['load ' mfile]);

   %%%
   % Append band to matfile typeband
   typeband = [typeband band];


   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   % Check typeband and determine gas IDs
   ii=strmatch(typeband,alltypeband);
   if ( length(ii) > 1 | ii < 1)
      ii
      typeband
      alltypeband
      ipnum
      error(['invalid type_band: ' typeband]);
   end
   ngas=allngas(ii);
   if (ngas ~= nsets)
     ngas
     nsets
     ipnum
     error('mismatch in expected ngas and matfile nsets')
   end
   gasids=allgasids(ii,1:ngas);


   %%%
   % Subset channels
   idfirst = allidfirst(ii);
   idlast = allidlast(ii);
   ichan = round(ichan); % exact integer
   ikeep = find(ichan >= idfirst & ichan <= idlast);
   if (length(ikeep) < nchan)
      ichan = ichan(ikeep);
      fchan = fchan(ikeep);
      nchan = length(ikeep);
      ctrans=ctrans(ikeep,:);
   end


   %%%
   % Subset angles
   nangles = allnangles(ii);
   if (nangles < nang)
      nals = round( nang*nlay*nsets ); % exact integer
      ijunk = zeros(nangles,nlay*nsets);
      for ia=1:nangles
         ijunk(ia,:) = ia:nang:nals;
      end
      ikeep = sort( reshape(ijunk,1,nangles*nlay*nsets) );
      ctrans = ctrans(:,ikeep);
      nang = nangles;
      secang = secang(1:nang);
   end


   %%%%%%%%%%%%%%%%%%
   % Load temperature
   ii=prof.nlevs(ipnum) - 1; % number of layers
   if (ii ~= nlay)
      ii
      nlay
      ipnum
      error('mismatch in number of layers')
   end
   temp=prof.ptemp(1:nlay,ipnum);


   %%%%%%%%%%%%%%%%%%
   % Load gas amounts
   amount=zeros(nlay,ngas);
   for ig=1:ngas

      ii=intersect( head.glist, abs(gasids(ig)) );
      if (length(ii) ~= 1)
         ipnum
         gasids(ig)
         head.glist
         error('required gas not found in RTP file')
      end
      ii=abs(gasids(ig));
      eval(['amount(:,ig)=prof.gas_' int2str(ii) '(1:nlay,ipnum);'])
   end
   % Convert amount from molecules/cm^2 to kmoles/cm^2
   amount=amount/6.02214199E+26;

   % Assign dummy res and rnfwhm
   res=zeros(nchan,1);
   rnfwhm=zeros(nchan,1);


   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   % Re-order the ctrans layers to run top to bottom
   if (ip ==1)
      indr=nlay:-1:1;
      nanglaygas=nang*nlay*ngas;
      nanglay=nang*nlay;
      indis=1:nanglaygas;
      indwant=indis;
      ilay=1:nlay;
      ileft=nang*(ilay-1);
      iright=nang*(nlay - ilay);
      for igas=1:ngas
         ioffset=(igas - 1)*nanglay;
         for ia=1:nang
            indwant(ioffset + ileft + ia)=indis(ioffset + iright + ia);
         end
      end
   end
   roctrans=ctrans(:,indwant)'; %'


   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   % Write out the merged profile + transmittance data file
   outname=[outfile int2str(ipnum) '.dat'];
   titlecom=[comment ' p#' int2str(ipnum)];
   [iok]=wrt_convdat(outname, nang, nlay, ngas, nchan, gasids, secang, ...
      titlecom, temp, amount, fchan, ichan, res, rnfwhm, roctrans);

   if (iok == 0)
      error(['Error detected writing ' outname]);
   end

end
ok=1;

%%% end of function %%%

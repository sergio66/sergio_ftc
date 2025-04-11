% Read the header section of a conv trans data file
% Mimics the FORTRAN version
% Assumes data file is IEEE big endian

% Last updated: Scott Hannon, 20 July 2000, open big endian
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxlay=100;
maxgas=3;
maxang=12;
%
%ctrans=zeros(maxlay,maxgas,maxang);
%
fname=input('Enter name of file to read: ','s')
%
% Open the input file
fid=fopen(fname,'r','ieee-be');
%
% Read the number of layers, gases, angles, and channels
ifm=fread(fid,1,'int');
nlay=fread(fid,1,'int')
ngas=fread(fid,1,'int')
nang=fread(fid,1,'int')
%%%
%nang=fread(fid,1,'int')
%nlay=fread(fid,1,'int')
%ngas=fread(fid,1,'int')
%%%
nchan=fread(fid,1,'int')
ifm=fread(fid,1,'int');
%
if (nlay ~= maxlay)
   error=['file has ' int2str(nlay) ' layers, expecting ' int2str(maxlay)]
end
%if (ngas ~= maxgas)
%   error=['file has ' int2str(ngas) ' gases, expecting ' int2str(maxgas)]
%end
%if (nang ~= maxang)
%   error=['file has ' int2str(nang) ' angles, expecting ' int2str(maxang)]
%end
%
% Read the gas IDs
ifm=fread(fid,1,'int');
gasid=fread(fid,ngas,'int')
ifm=fread(fid,1,'int');
%
% Read the angle secants
ifm=fread(fid,1,'int');
angsec=fread(fid,nang,'float')
ifm=fread(fid,1,'int');
%
% Read the title
ifm=fread(fid,1,'int');
title=fread(fid,ifm,'char')';
setstr(title)
ifm=fread(fid,1,'int');
%
% Read the temperature
ifm=fread(fid,1,'int');
%temp=fread(fid,maxlay,'float')
temp=fread(fid,maxlay,'real*4')
ifm=fread(fid,1,'int');
%
% Read the gas amounts
amount=zeros(maxlay,maxgas);
for igas=1:ngas
   ifm=fread(fid,1,'int');
%   amount(:,igas)=fread(fid,maxlay,'float');
   amount(:,igas)=fread(fid,maxlay,'real*4');
   ifm=fread(fid,1,'int');
end
%
amount
%
fclose(fid)

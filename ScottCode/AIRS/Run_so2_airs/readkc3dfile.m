function [data, wnums] = readkc3dfile(dfile);

% function [data, wnums] = readkc3dfile(dfile);
%
% Read binary data file (dfile) created by Scott's readkc3.m
%
% Input:
%    dfile = {string} filename of binary data file created by readkc3.m
%
% Output:
%    data  = [npts x ncol] data
%    wnums = [npts x 1] frequency points
%

% Created: 9 April 2002, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need modifying
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Open and read in the header of the temporary file
disp(['Reading data from temp file ' dfile]);
fid=fopen(dfile,'r'); % Note: assumed written in native format
npts=fread(fid,1,'integer*4');
ncol=fread(fid,1,'integer*4');

% Read freq points
wnums=fread(fid,[npts,1],'real*8');

% Read data
data=fread(fid,[npts,ncol],'real*4');

% Close file
fclose(fid);
clear fid

%%% end of function %%%

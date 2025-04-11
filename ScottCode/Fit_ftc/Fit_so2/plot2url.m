function [url] = plot2url(ifig);

% function [url] = plot2url(ifig);
%
% Create an online jpeg file of an existing MATLAB figure.
%
% Input:
%    ifig = [1 x 1] figure number {1 to 9, default=1}
%
% Output:
%    url = [string] URL of newly created jpeg file
%

% Created: 28 December 2007, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Filename and URL prefixes
fname_prefix = '/asl/ftp/pub/hannon/Junk/fig';
url_prefix = 'http://asl.umbc.edu/pub/hannon/Junk/fig';

if (nargin < 1)
   ifig = 1;
else
   if (ifig < 1 | ifig > 9)
      error('ifig outside expected range of 1-9')
   end
end
ifigstr = int2str(ifig);

fname = [fname_prefix ifigstr];
url = [url_prefix ifigstr '.jpg'];

eval(['print ' fname ' -djpeg']);


%%% end of function %%%

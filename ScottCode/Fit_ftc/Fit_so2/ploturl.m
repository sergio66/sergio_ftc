function [url] = ploturl(x,y,minmax,ifig);

% function [url] = ploturl(x,y,minmax,ifig);
%
% Plot x-vs-y data to an online jpeg file.
%
% Input:
%    x    = [nx x 1]  x-axis data
%    y    = [nx x ny] y-axis data
%    minmax = [1 x 4] {optional} min & max for x&y axes [xmin xmax ymin ymax]
%    ifig = [1 x 1] {optional} figure number {1 to 9, default=1}
%
% Output:
%    url = [string] URL of newly created jpeg file
%

% Created: 28 December 2007, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Get user name
[status, result] = system('whoami');
n = length(result) - 1;
user = result(1:n);
clear status result n

% Filename and URL prefixes
fname_prefix = ['/asl/ftp/pub/' user '/fig'];
url_prefix = ['http://asl.umbc.edu/pub/' user '/fig'];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (nargin < 2)
   error('insufficient input; minimum requirement is x & y')
end
if (nargin > 4)
   error('too many input arguments')
end


d = size(x);
if (length(d) ~= 2 | min(d) ~= 1)
   error('x must be a [nx x 1] array');
end
nx = length(x);
d = size(y);
if (length(d) ~= 2 | d(1) ~= nx)
   error('y must be a [nx x ny] array with nx=length(x)');
end

if (nargin == 4)
   % input arguments are: {x,y,minmax,ifig}
   d = size(minmax);
   if (min(d) ~= 1 | max(d) ~= 4)
      error('minmax must be a [1 x 4] array')
   end
   d = size(ifig);
   if (min(d) ~= 1 | max(d) ~= 1)
      error('ifig must be a [1 x 1] integer 1-9')
   end
   if (ifig < 1 | ifig > 9)
      error('ifig must be 1 to 9')
   end
else
   if (nargin == 2)
      minmax = [min(x) max(x) min(min(y)) max(max(y))];
      ifig = 1;
   else
      % 3rd argument can be minmax or ifig
      d = size(minmax);
      if (min(d) == 1 & max(d) == 1)
         ifig = minmax;
         minmax = [min(x) max(x) min(min(y)) max(max(y))];
         if (ifig < 1 | ifig > 9)
            error('ifig must be 1 to 9')
         end
      else
         if (min(d) ~= 1 | max(d) ~= 4)
           error('minmax must be [1 x 4]]')
         end
         ifig = 1;
      end
   end
end


ifigstr = int2str(ifig);

fname = [fname_prefix ifigstr];
url = [url_prefix ifigstr '.jpg'];

% Close ifig to force MATLAB to always open it anew
figure(ifig)
close(ifig)
figure(ifig)
plot(x,y),axis(minmax),grid

eval(['print ' fname ' -djpeg']);

%%% end of function %%%

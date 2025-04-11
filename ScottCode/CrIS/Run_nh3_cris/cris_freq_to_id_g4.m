function [id_g4] = cris_freq_to_id_g4(freq);

% function [id_g4] = cris_freq_to_id_g4(freq);
%
% Compute the CrIS "g4" channel IDs from channel freq.
%
% Input:
%    freq : [n x 1] channel freq {cm-1}
%
% Output:
%    id_g4 : [n x 1] channel ID
%

% Created: 17 Jun 2011, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Check input
d = size(freq);
if (length(d) ~= 2 | min(d) ~= 1)
  error('unexpected dimensions for input freq')
end
if (d(1) > d(2))
   lflip = 1;
else
   lflip = 0;
end

% The OPDs for the 3 bands
opd1 = 0.8;
opd2 = 0.4;
opd3 = 0.2;

% The channel spacing for the 3 bands
df1 = 1/(2*opd1);
df2 = 1/(2*opd2);
df3 = 1/(2*opd3);

% The start freqs for the 3 bands
fs1 =  650;
fs2 = 1210;
fs3 = 2155;

% The end freqs for the 3 bands
fe1 = 1095;
fe2 = 1750;
fe3 = 2550;

% The channel freqs for the 3 bands
f1 = fs1:df1:fe1;
f2 = fs2:df2:fe2;
f3 = fs3:df3:fe3;

% The channel freqs for the "g4" guard channels for the 3 band
f1g4 = [(fs1-4*df1):df1:(fs1-df1), (fe1+df1):df1:(fe1+df1*4)];
f2g4 = [(fs2-4*df2):df2:(fs2-df2), (fe2+df2):df2:(fe2+df2*4)];
f3g4 = [(fs3-4*df3):df3:(fs3-df3), (fe3+df3):df3:(fe3+df3*4)];

% All channel freqs, with ID = index
f = [f1, f2, f3, f1g4, f2g4, f3g4];

% Round all freqs to the nearest 0.001 cm^-1
fx = round(f*1000)/1000;
freqx = round(freq*1000)/1000;

[junk, i1, i2] = intersect(fx, freqx);
if (length(i2) < length(freq))
  error('input freq contained repeats or unexpected values')
end
id_g4 = i1(i2);
if (lflip == 1)
   id_g4 = id_g4'; %'
end


%%% end of function %%%

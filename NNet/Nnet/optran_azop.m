function azop = optran_azop;

% Load Scott's OPTRAN layers
load Inputs/olays
azop_extend = 1.2;  % New 704 profiles broke peak azop
azop = azop_extend*1E3*6.022E23*olays;
azop = flipud(azop);


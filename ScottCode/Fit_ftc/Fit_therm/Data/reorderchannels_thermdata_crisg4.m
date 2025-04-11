% Read in thermdata_crisg4.mat and reorder the channels so all the guard
% channels are moved to the end

idchan = (1:1329)'; %'
ind = [5:717, 726:1158, 1167:1325, 1:4, 718:725, 1159:1166, 1326:1329];
freq = freq(ind);
radnotherm = radnotherm(ind,:);
radwiththerm = radwiththerm(ind,:);
tauz=tauz(ind,:);

%%% end of program %%%


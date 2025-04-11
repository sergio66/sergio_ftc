cd /asl/rtp_xfs/rtprod_airs/2011/03/11

chans38 = [        41          54         181         273         317         359         445         449 ...
                  532         758         903         904        1000        1020        1034        1055 ...
                 1075        1103        1249        1282        1291        1447        1475        1557 ...
                 1604        1614        1618        1660        1790        1866        1867        1868 ...
                 1878        1888        2112        2140        2321        2333];
chans41 = [chans38 2325        2339        2353]; chans41 = sort(chans41);

fairs = instr_chans;

addpath /home/sergio/MATLABCODE/PLOTTER

iMin = 3; iMax = 3;
iMin = 0; iMax = 23;
inputs = [];
targets = [];
for ii = iMin : iMax
  fprintf(1,'reading in %2i \n',ii)
  fname = ['sergio_nadir_cloudy_airs_l1b_era_sarta_baum_ice.2011.03.11.' num2str(ii,'%02d') '.rtp'];
  [h,ha,p,pa] = rtpread(fname);
  clear emis
  for jj = 1 : length(p.rlat)
    emis(1,jj) = interp1(p.efreq(1:p.nemis(jj),jj),p.emis(1:p.nemis(jj),jj),820);
    emis(2,jj) = interp1(p.efreq(1:p.nemis(jj),jj),p.emis(1:p.nemis(jj),jj),900);
    emis(3,jj) = interp1(p.efreq(1:p.nemis(jj),jj),p.emis(1:p.nemis(jj),jj),960);    
  end
 scatter_coast(p.rlon,p.rlat,20,emis(3,:)); colorbar; colormap jet; pause(0.1); 
  xinputs =  [p.stemp; p.rlat; emis; rad2bt(h.vchan(chans41),p.rcalc(chans41,:))];
  xtargets = [max(p.cngwat.*p.cfrac,0); max(p.cngwat2.*p.cfrac2,0); max(p.cprtop,0); max(p.cprtop2,0); max(p.cpsize,0); max(p.cpsize2,0)];
  boo = find(p.cngwat == 0 | p.cfrac == 0 | p.cprtop < 10);
    xtargets([1 3 5],boo) = NaN;
  boo = find(p.cngwat2 == 0 | p.cfrac2 == 0 | p.cprtop2 < 10);
    xtargets([2 4 6],boo) = NaN;
  inputs = [inputs; xinputs'];
  targets = [targets; xtargets'];
  whos inputs targets
end

cd ~/MATLABCODE/NeuralNet

%{
If there is a physical or mathematical reason for the
output to be bounded, then you can either

a. scale the targets from [tlb tub] to [0 1] and use
   logsig. If the net is a classifier also consider
   softmax.
b. scale the targets from [tlb tub] to [-1 1] and use
   tansig
c. scale the targets to have zero-mean/unit-variance
   and use purelin
%}

inputs = inputs';
targets = targets';

%% create a network with N neurons
hiddenLayerSize = 2
net = fitnet(hiddenLayerSize);

%% how to divide the data
net.divideParam.trainRatio = 70/100;
net.divideParam.valRatio = 15/100;
net.divideParam.testRatio = 15/100;

%% train
net.trainFcn = 'trainbr';
net.trainFcn = 'trainscg';
[net,tr] = train(net,inputs,targets);

%% test network
outputs = net(inputs);
errors = gsubtract(targets,outputs);
performance = perform(net,targets,outputs)

disp('click regression')
%% view network
view(net)

clear boo emis fname h ha p pa iMin iMax ii jj ans chans38 fairs xinputs xtargets

%{
https://www.mathworks.com/matlabcentral/newsreader/view_thread/268851
1. You can save your trained network with :
    save net    % this will save the network called net as net.mat,
    you can then load it to the workspace with load net

2. To do your prediction, you can use sim(net, inputs). type doc sim

then eg
nana = load('net.mat');
plot(1:64800,nana.targets(1,:),1:64800,nana.outputs(1,:))
plot(1:64800,nana.targets(1,:)-nana.outputs(1,:))
plot(1:64800,nana.targets(1,:)-nana.outputs(1,:)-nana.errors(1,:))
ooIN  = find(isnan(nana.targets(1,:)));
ooOUT = find(isnan(nana.outputs(1,:)));
plot(nana.outputs(1,ooIN))

for ii = 1 : 6
  hist(nana.targets(ii,:)-nana.outputs(ii,:),100)
  dn = -10 : 0.1 : 10;
  dn = -02 : 0.1 : 02;
  plot(dn,hist((nana.targets(ii,:)-nana.outputs(ii,:))./nana.targets(ii,:),dn))
  title(num2str(ii))
  pause
end  
%}

clear boo* x* chisqr matr*

profile_list = 1 : 49;
profile_list = 1 : 48;

layers = 30;
layers = 1:100;
layers = 1:10;
layers = 2;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xOD = fitdeltaOD1(layers,:,:);
xOD = xOD(:);

iLogOrNorm = -1;
xOD0 = xOD;
if iLogOrNorm > 0
  xOD = log10(xOD);   %%% fit this deltaOD
  strx = 'log10(xOD)';
  stry = 'log10(yOD)';  
else
  strx = 'xOD';
  stry = 'yOD';  
end  

booW = log10(pred.gas_1); booW = booW(layers,profile_list,:); booW = booW(:);
booT = pred.ptemp/100;    booT = booT(layers,profile_list,:); booT = booT(:);
booS = pred.a;            booS = booS(layers,profile_list,:); booS = booS(:);

%lpa  = pred.layer_profile_angle;  lpa = lpa(layers,profile_list,:);   lpa = lpa(:);
lpaL = pred.layer_profile_angleL; lpaL = lpaL(layers,profile_list,:); lpaL = lpaL(:);
lpaP = pred.layer_profile_angleP; lpaP = lpaP(layers,profile_list,:); lpaP = lpaP(:);
lpaA = pred.layer_profile_angleA; lpaA = lpaA(layers,profile_list,:); lpaA = lpaA(:);

mmx = pred.mmwater; mmx = mmx(layers,profile_list,:); mmx = mmx(:);
st  = pred.stemp;   st  = st(layers,profile_list,:);   st = st(:);

boo1  = predsets.F.set123_1;  boo1  = boo1(layers,profile_list,:);  boo1 = boo1(:);
boo2  = predsets.F.set123_2;  boo2  = boo2(layers,profile_list,:);  boo2 = boo2(:);
boo3  = predsets.F.set123_3;  boo3  = boo3(layers,profile_list,:);  boo3 = boo3(:);
boo4  = predsets.F.set123_4;  boo4  = boo4(layers,profile_list,:);  boo4 = boo4(:);
boo5  = predsets.F.set123_5;  boo5  = boo5(layers,profile_list,:);  boo5 = boo5(:);
boo6  = predsets.F.set123_6;  boo6  = boo6(layers,profile_list,:);  boo6 = boo6(:);
boo7  = predsets.F.set123_7;  boo7  = boo7(layers,profile_list,:);  boo7 = boo7(:);
boo8  = predsets.F.set123_8;  boo8  = boo8(layers,profile_list,:);  boo8 = boo8(:);
booU  = ones(size(boo1));

ok    = find(abs(imag(xOD)) < eps);
%ok    = find(xOD > 0);
%ok    = find(xOD > 0 & abs(imag(xOD)) < eps);

xOD = xOD(ok);
booT  = booT(ok);
booW  = booW(ok);
booS  = booS(ok);
boo1  = boo1(ok);
boo2  = boo2(ok);
boo3  = boo3(ok);
boo4  = boo4(ok);
boo5  = boo5(ok);
boo6  = boo6(ok);
boo7  = boo7(ok);
boo8  = boo8(ok);
booU  = booU(ok);

%lpa = lpa(ok);
lpaP = lpaP(ok);
lpaA = lpaA(ok);
lpaL = lpaL(ok);

mmx = mmx(ok);
st  = st(ok);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matr4a = [booU booT booW booS];  %%% ORIG : see do_the_pred -- pretty darn good!

matr8 = [booU boo1 boo2 boo3 boo4 boo5 boo6 boo7 boo8];

%%%%%%%%%%%%%%%%%%%%%%%%%
matr = matr8;
%%%%%%%%%%%%%%%%%%%%%%%%%

fitc = matr\xOD;

predict = matr * fitc;
therror = xOD-predict;

figure(1); plot(xOD,predict,'bx',xOD,xOD,'k'); xlabel(strx); ylabel(stry)
figure(2); plot(xOD,therror,'.'); xlabel(strx); ylabel([strx '-' stry])
figure(3); scatter(xOD,therror,10,lpaP,'filled'); colorbar; colormap jet
figure(3); pcerror = (xOD-predict)./xOD*100; scatter(xOD,pcerror,10,lpaP,'filled'); colorbar; colormap jet
figure(3); plot(lpaP,pcerror,'.'); xlabel('profile'); ylabel('percent error')
figure(3); scatter(lpaP,pcerror,10,mmx,'filled'); xlabel('profile'); ylabel('percent error'); colorbar; title('colorbar = mmw')
  jett = jet(64); ; jett = jett(1:48,:); colormap(jett); 
figure(3); scatter(lpaP,pcerror,10,st,'filled'); xlabel('profile'); ylabel('percent error'); colorbar; title('colorbar = stemp')
  jett = jet(64); ; jett = jett(1:48,:); colormap(jett);  
figure(4); plot(1:length(xOD),xOD,'b',1:length(xOD),predict,'r'); hl = legend('xOD','predict','location','best');

for ii = 1 : 4
  figure(ii); title(['LayStart = ' num2str(layers(1))]);
end

disp('ret fitc = matr\xOD'); pause
pause(0.1); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hingeODs = [0.0553 0.190 0.385 0.540 1.000 1.650 2.900 3.490 4.410];
if iLogOrNorm > 0
  hingeODs = log10(hingeODs);
end  
weights  = [1      3     5     6     7     6     3     2     1];

wgtOD = interp1(hingeODs,weights,xOD,[],'extrap');
oo = find(xOD < 0.0553); wgtOD(oo) = 1;
oo = find(xOD > 4.410);  wgtOD(oo) = 1;
wgtmatr = diag(wgtOD);

fitc = (wgtmatr * matr)\(wgtmatr * xOD);

predict = matr * fitc;
therror = xOD-predict;

figure(1); plot(xOD,predict,'.',xOD,xOD,'r.'); xlabel('log10(deltaOD)'); ylabel('predicted delta OD')
figure(2); plot(xOD,therror,'.'); xlabel('log10(OD)'); ylabel('log10(OD)-log10(fittedOD)')
figure(3); scatter(xOD,therror,10,lpaP,'filled'); colorbar; colormap jet
figure(3); pcerror = (xOD-predict)./xOD*100; scatter(xOD,pcerror,10,lpaP,'filled'); colorbar; colormap jet
figure(3); plot(lpaP,pcerror,'.'); xlabel('profile'); ylabel('percent error')
figure(3); scatter(lpaP,pcerror,10,mmx,'filled'); xlabel('profile'); ylabel('percent error'); colorbar; title('colorbar = mmw')
  jett = jet(64); ; jett = jett(1:48,:); colormap(jett); 
figure(3); scatter(lpaP,pcerror,10,st,'filled'); xlabel('profile'); ylabel('percent error'); colorbar; title('colorbar = stemp')
  jett = jet(64); ; jett = jett(1:48,:); colormap(jett); 

disp('ret fitc = (wgtmatr * matr)\(wgtmatr * xOD)'); pause
pause(0.1); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
x = whos('boo*');
for ii = 1 : length(x)
  str = ['matr = ' x(ii).name ';'];
  eval(str);
  fitc = matr\xOD;
  predict = matr * fitc;
  therror = xOD - predict;
  figure(3); clf; plot(xOD,predict,'.'); xlabel('xOD'); ylabel('prediction');
  title(x(ii).name); pause(0.1); disp('ret'); pause
  chisqr(ii) = sum(therror.*therror);
end
plot(chisqr,'+-'); grid
disp('ret'); pause
pause(0.1); 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matr = [boo5 boo6 booS booT booW booU];
fitc = matr\xOD;

predict = matr * fitc;
therror = xOD-predict;

figure(1); plot(xOD,predict,'.',xOD,xOD,'r.'); xlabel('log10(deltaOD)'); ylabel('predicted delta OD')
figure(2); plot(xOD,therror,'.'); xlabel('log10(OD)'); ylabel('log10(OD)-log10(fittedOD)')
figure(3); scatter(xOD,therror,10,lpaP,'filled'); colorbar; colormap jet
figure(3); pcerror = (xOD-predict)./xOD*100; scatter(xOD,pcerror,10,lpaP,'filled'); colorbar; colormap jet
figure(3); plot(lpaP,pcerror,'.'); xlabel('profile'); ylabel('percent error')
figure(3); scatter(lpaP,pcerror,10,mmx,'filled'); xlabel('profile'); ylabel('percent error'); colorbar; title('colorbar = mmw')
  jett = jet(64); ; jett = jett(1:48,:); colormap(jett); 
figure(3); scatter(lpaP,pcerror,10,st,'filled'); xlabel('profile'); ylabel('percent error'); colorbar; title('colorbar = stemp')
  jett = jet(64); ; jett = jett(1:48,:); colormap(jett); 


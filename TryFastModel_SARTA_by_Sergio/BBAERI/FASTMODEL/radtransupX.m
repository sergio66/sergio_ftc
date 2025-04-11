function rad = radtransupX(raF,raaK,raT,SurfPres,TopPr,rAngle,iS,presslevels)

%% modelled on /home/sergio/MATLABCODE/RADTrans/CLEAR/radtransup25.m

% function rad=radtransupX(raF,raaK,raT,SurfPres,TopPr,rAngle,iS,airslevels)
% given abs coeffs raaK, wavevector raF 
%       vertical temp profile raT,
%       TopPr pressure (top), SurfPres pressure (bottom) parameters
%       satellite view angle
%       airslevels = pressure levels (1-101 for default AIRS)
%       iS=-1 : no solar      +1 solar
%
% this finds the fractional bottom layer (as done by kCARTA) and then does 
% radiative transfer from TopPres to SurfPres layer for sat angle rAngle
% this is for UPWARD LOOKING instr. 

%almost but not quite the kCARTA routine

[mm,nn] = size(raF);
if mm > nn
  raF = raF';
  end

[mm,nn] = size(raaK);
if mm > nn
  raaK = raaK';
  end

[mm,nn] = size(raT);
if mm > nn
  raT = raT';
  end

r1=1.1911E-5;
r2=1.4387863;

len=length(presslevels)-1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%now find bottom layer
%note that this is the lowest FULL layer (see below)!!!!!!!!!
if (SurfPres < presslevels(len+1))
  error('surf pressure too low!!!!!!!!!');
  end

if (SurfPres >= presslevels(1))      %%lowest AIRS layer = lowest full layer
  iL=1;
  SurfPres = presslevels(1);
else
  iL=1; 
  while presslevels(iL) > SurfPres
    iL=iL+1;
    end
  end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%now find top layer

if (TopPr > presslevels(1))
  error('top pressure too high!!!!!!!!!');
  end

if (TopPr <= presslevels(len+1))
  iU=len;
  TopPr=presslevels(len+1);
else
  iU=1; 
  while presslevels(iU) > TopPr
    iU=iU+1;
    end
  iU=iU-1;
  end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fprintf(1,'lowest, highest FULL layers to use = %4i  %4i \n',iL,iU);

if (iL > iU)
  error('need lower layer BELOW upper layer!!!!!!!');
  end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%find top layer fraction and good stuff
%
%      LAYER                          LEVEL
%
%
%       iU+1
%       
%              -----------------      iU+1
%
%
% +++++++++++++++++++++++++++++++++++++++++++++++ top pressure
%
%
%              -----------------      iU
%
%
%

partial_layer_top=raaK(iU,:);
rFracTop=(TopPr-presslevels(iU))/(presslevels(iU+1)-presslevels(iU));
TempHighestLayer=interptemp(raT,rFracTop,TopPr,-1,iU,presslevels);

fprintf(1,'upper partial layer %3i \n',iL-1);
fprintf(1,'frac= %8.6f Temp = %8.6f \n',rFracTop,TempHighestLayer);

%%%%%%%find bottom layer fraction and good stuff
% iL tells us the lowest "full layer" to use
% ie it tells the pressure of the lower boundary of the lowest "full layer"
%
%      LAYER                          LEVEL
%
%              -----------------      iL+1
%
%       iL
%       
%              -----------------      iL
%
%
% +++++++++++++++++++++++++++++++++++++++++++++++ surface pressure
%
%
%              -----------------      iL-1

if iL > 1
  %see kCARTA docs
  partial_layer=raaK(iL-1,:);
  rFracBot=(SurfPres-presslevels(iL))/(presslevels(iL-1)-presslevels(iL));
  partial_layer=partial_layer*rFracBot;
  TempLowestLayer=interptemp(raT,rFracBot,SurfPres,1,iL-1,presslevels);

  fprintf(1,'bottom partial layer %3i \n',iL-1);
  fprintf(1,'frac= %8.6f Temp = %8.6f \n',rFracBot,TempLowestLayer);

else
  partial_layer=raaK(1,:);
  rFracBot=1.0;
  partial_layer=partial_layer*rFracBot;
  TempLowestLayer=raT(1);

  fprintf(1,'bottom partial layer %3i \n',1);
  fprintf(1,'frac= %8.6f Temp = %8.6f \n',rFracBot,TempLowestLayer);
  end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% do solar contrib
radsolar=zeros(size(raF));
if iS > 0
  fprintf(1,'doing solar .......... \n');
  rCos=cos(rAngle*pi/180);
  Temp=5600.0;
  radsolar=ttorad(raF,Temp);           %assume emissivity=1.0, solar angle=0
  solidangle=1.0;                      %NO solid angle contribution
  radsolar=radsolar*solidangle; 
  for ii=iU:iU
    Temp=TempHighestLayer;
    raLayTrans=exp(-raaK(ii,:)*rFracTop/rCos);
    radsolar=radsolar.*raLayTrans;
    end
  for ii=iU-1:-1:iL
    Temp=raT(ii);
    raLayTrans=exp(-raaK(ii,:)/rCos);
    radsolar=radsolar.*raLayTrans;
    end
  
  %do emission thru lowest layer
  if (iL == 1) 
   iLL=iL;    %rad trans thru complete layer 1 has been done!!!
              %no need to do any more radtrans
  else
    iLL=iL-1; %rad transfer thru lowest complete layer has been done
              %do radtrans thru lowest frac layer
    Temp=TempLowestLayer;
    raLayTrans=exp(-partial_layer/rCos);
    raPlanck=ttorad(raF,Temp);
    radsolar=radsolar.*raLayTrans;
    end

  end

%%%%%%%%% do downward emission thru layers contrib
radthermal=zeros(size(raF));
iT=1;   %%%%%%%%%%%%%%%%%% we NEED this!!!!
if iT > 0
  fprintf(1,'doing emission thru layers .......... \n');
  rCos=cos(rAngle*pi/180);
  Temp=2.7;                              %cold blue yonder
  radthermal=ttorad(raF,Temp)';           %assume emissivity=1.0, solar angle=0
  for ii=iU:iU
    Temp=TempHighestLayer;
%%%    fprintf(1,'ii = %3i T = %8.6f k = %8.6f frac = %8.6f\n',ii,Temp,raaK(ii,1),rFracTop)
    raLayTrans=exp(-raaK(ii,:)*rFracTop/rCos);
    raPlanck=ttorad(raF,Temp)';
    raEmission=(1.0-raLayTrans).*raPlanck;
    radthermal=raEmission + radthermal.*raLayTrans;
%    t=radtot(raF(1),radthermal(1));
%    t=radthermal(1);
%    fprintf('ii   temp(layer) temp(rad) = %5i %9.5f %11.9e \n',ii,Temp,t);
    end

  for ii=iU-1:-1:iL
    Temp=raT(ii);
%%%    fprintf(1,'ii = %3i T = %8.6f k = %8.6f\n',ii,Temp,raaK(ii,1))
    raLayTrans=exp(-raaK(ii,:)/rCos);
    raPlanck=ttorad(raF,Temp)';
    raEmission=(1.0-raLayTrans).*raPlanck;
    radthermal=raEmission + radthermal.*raLayTrans;
%    t=radtot(raF(1),radthermal(1));
%    t=radthermal(1);
%    fprintf('ii   temp(layer) temp(rad) = %5i %9.5f %11.9e \n',ii,Temp,t);
    end

  %do emission thru lowest layer
  if (iL == 1) 
   iLL=iL;    %rad trans thru complete layer 1 has been done!!!
              %no need to do any more radtrans
  else
    iLL=iL-1; %rad transfer thru lowest complete layer has been done
              %do radtrans thru lowest frac layer
    if rFracBot > 1e-7
      Temp=TempLowestLayer;
      raLayTrans=exp(-partial_layer/rCos);
      raPlanck=ttorad(raF,Temp)';
      raEmission=(1.0-raLayTrans).*raPlanck;
      radthermal=raEmission + radthermal.*raLayTrans;
      fprintf(1,'doing final layer rFracBot = %9.5f\n',rFracBot);
%      t=radtot(raF(1),radthermal(1));
%      t=radthermal(1);
%      fprintf('ii  temp(layer)  temp(rad) = %5i %9.5f %11.9e \n',iLL,Temp,t);
      end
    end
  end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rad=radsolar+radthermal;

%% now make up La Familia de Predictors
%% Fixed and/or Temp

trefNP = ones(NP,1) * tref;
g1refNP = ones(NP,1) * g1ref;
g3refNP = ones(NP,1) * g3ref;

%% first do constant and temperature terms
ii=1;    vars(ii,:,:) = ones(size(ptemp));           names{ii} = 'constant';  types(ii) = 1;

%% then do nonlinear terms
ii=ii+1; vars(ii,:,:) = ptemp ./ (ones(NP,1)*tref);                  names{ii} = 'T/Tref';       types(ii) = 1;
ii=ii+1; vars(ii,:,:) = ...
             (cumsum(pwgt' .* ptemp') ./ cumsum(pwgt' .* trefNP'))'; names{ii} = 'presswgtTx';   types(ii) = 1;
ii=ii+1; vars(ii,:,:) = (ptemp./(ones(NP,1)*tref)).^(0.5);           names{ii} = 'sqrt(T/Tref)'; types(ii) = 1;
ii=ii+1; vars(ii,:,:) = (ptemp./(ones(NP,1)*tref)).^(2.0);           names{ii} = '(T/Tref)^2';   types(ii) = 1;

%% water
ii=ii+1;  iiW = ii; vars(ii,:,:) = gas_1 ./ (ones(NP,1)*g1ref);  names{ii} = 'g1/g1ref';         types(ii) = 2;
ii=ii+1; vars(ii,:,:) = sqrt(squeeze(vars(iiW,:,:)));            names{ii} = 'sqrt(g1/g1ref)';   types(ii) = 2;
ii=ii+1; vars(ii,:,:) = (squeeze(vars(iiW,:,:))).^2;             names{ii} = '(g1/g1ref)^2';     types(ii) = 2;
ii=ii+1; vars(ii,:,:) = (squeeze(vars(iiW,:,:))).^3/2;           names{ii} = '(g1/g1ref)^3/2';   types(ii) = 2;
ii=ii+1; vars(ii,:,:) = (squeeze(vars(iiW,:,:))).^2;             names{ii} = '(g1/g1ref)^3';     types(ii) = 2;
ii=ii+1; vars(ii,:,:) = (squeeze(vars(iiW,:,:))).^2;             names{ii} = '(g1/g1ref)^4';     types(ii) = 2;
ii=ii+1; vars(ii,:,:) = log10(squeeze(vars(iiW,:,:)));           names{ii} = 'log(g1/g1ref)';    types(ii) = 2;
ii=ii+1;  vars(ii,:,:) = mm'*ones(1,100);                        names{ii} = 'mm water';         types(ii) = 2;
ii=ii+1; wgtW = (pav.*gas_1); wgtWNP = (pav.*g1refNP);
                             vars(ii,:,:) = wgtW./wgtWNP;        names{ii} = 'pressWgtW';        types(ii) = 2;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-1,:,:)).^2;            names{ii} = 'pressWgtW^2';      types(ii) = 2;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-2,:,:)).^0.5;          names{ii} = 'pressWgtW^0.5';    types(ii) = 2;
  ii=ii+1; vars(ii,:,:) = log10(squeeze(vars(ii-3,:,:)));        names{ii} = 'log(pressWgtW)';   types(ii) = 2;
ii=ii+1; vars(ii,:,:) = ...
          (cumsum(pwgt' .* gas_1')./cumsum(pwgt'.*g1refNP'))';   names{ii} = 'presswgtWx';       types(ii) = 2;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-1,:,:)).^2;            names{ii} = 'presswgtWx.^2';    types(ii) = 2;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-2,:,:)).^0.5;          names{ii} = 'presswgtWx.^0.5';  types(ii) = 2;
  ii=ii+1; vars(ii,:,:) = log10(squeeze(vars(ii-3,:,:)));        names{ii} = 'log(presswgtWx)';  types(ii) = 2;
ii=ii+1; vars(ii,:,:) = (gas_1.*ptemp)./(g1refNP.*trefNP);       names{ii} = 'g1/g1ref * T/Tref';       types(ii) = 2;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-1,:,:)).^2;            names{ii} = '(g1/g1ref * T/Tref)^2';   types(ii) = 2;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-2,:,:)).^(0.5);        names{ii} = '(g1/g1ref * T/Tref)^0.5'; types(ii) = 2;
  ii=ii+1; vars(ii,:,:) = log10(squeeze(vars(ii-3,:,:)));        names{ii} = 'log(g1/g1ref * T/Tref)';  types(ii) = 2;

%% ozone
ii=ii+1; iiO = ii; vars(ii,:,:) = gas_3 ./ (ones(NP,1)*g3ref);  names{ii} = 'g3/g3ref';          types(ii) = 3;
ii=ii+1; vars(ii,:,:) = log10(squeeze(vars(iiO,:,:)));          names{ii} = 'log(g3/g3ref)';     types(ii) = 3;
ii=ii+1; vars(ii,:,:) = sqrt(squeeze(vars(iiO,:,:)));           names{ii} = 'sqrt(g3/g3ref)';    types(ii) = 3;
ii=ii+1; vars(ii,:,:) = (squeeze(vars(iiO,:,:))).^2;            names{ii} = '(g3/g3ref)^2';      types(ii) = 3;
ii=ii+1; wgtO = (pav.*gas_3); wgtONP = (pav.*g3refNP);
                            vars(ii,:,:) = wgtO./wgtONP;        names{ii} = 'pressWgtO';        types(ii) = 3;
ii=ii+1; vars(ii,:,:) = ...
           (cumsum(pwgt' .* gas_3')./cumsum(pwgt'.*g3refNP'))'; names{ii} = 'presswgtOx';       types(ii) = 3;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-1,:,:)).^2;           names{ii} = 'presswgtOx.^2';    types(ii) = 3;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-2,:,:)).^0.5;         names{ii} = 'presswgtOx.^0.5';  types(ii) = 3;
ii=ii+1; vars(ii,:,:) = (gas_3.*ptemp)./(g3refNP.*trefNP);      names{ii} = 'g3/g3ref * T/Tref';       types(ii) = 3;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-1,:,:)).^2;           names{ii} = '(g3/g3ref * T/Tref)^2';   types(ii) = 3;
  ii=ii+1; vars(ii,:,:) = squeeze(vars(ii-2,:,:)).^0.5;         names{ii} = '(g3/g3ref * T/Tref)^0.5'; types(ii) = 3;

familyAll.vars  = vars;
familyAll.names = names;
familyAll.types = types;

[mmjunk nnjunk oojunk] = size(vars);
for ix = 1:mmjunk; plot(squeeze(vars(ix,:,:)));title(names{ix}); ret(0.1); end
clear mmjunk nnjunk oojunk
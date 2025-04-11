function kwel = optical_depth_fwo_fo(fwo,fo,tmin,nprof,nsecang,chi);

% Subset l2s for LW secant angles and channel of interest
fwo = squeeze(fwo(:,1:nsecang,chi,:));
fwo(:,:,101) = 1; 
fo = squeeze(fo(:,1:nsecang,chi,:));
fo(:,:,101) = 1; 

fwo(fwo(:) < tmin) = NaN;
fo(fo(:) < tmin) = NaN;
   
% Compute layer transmittances 
wel = squeeze((fwo(:,:,1:100)./fo(:,:,1:100) )./( (fwo(:,:,2:101)./fo(:,:,2:101))));

% Get things in proper order
wel = reshape(wel,nprof,100);

% Do the same with fwo, etc for ease of comparisons later and bad_fwo
fwo = reshape(fwo,nprof,101);

% Replace low/high transmittances with a threshold value
wel(wel < tmin) = tmin;

% Convert to absorption coefficients
kwel = -log(wel');
kwel(kwel<0) = 0;


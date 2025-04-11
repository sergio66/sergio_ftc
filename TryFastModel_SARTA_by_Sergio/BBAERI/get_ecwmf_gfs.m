addpath  /asl/matlab/gribtools/
addpath /asl/matlab/aslutil/
addpath /asl/matlab/science/

help rtpadd_ecmwf_data

%% baltimore lat lon
prof.rlat = ones(1,10) * 39.18;
prof.rlon = ones(1,10) * -76.67;
prof.rtime = mattime2tai(datenum(2012,2,27,15,0:10:90,0));
pattr = set_attr([],'rtime','time since 1993','profiles');

[head, hattr, prof, pattr] = rtpadd_ecmwf_data(struct, [], prof, pattr, ...
{'SP','SKT','10U','10V','TCC','CI','T','Q','O3'});

head.pfields = 0;
[head, hattr, prof2, pattr] = rtpadd_gfs(head, [], prof, pattr);

plot(prof.ptemp,prof.plevs,'b',prof2.ptemp,prof2.plevs,'r')
semilogy(prof.ptemp,prof.plevs,'b',prof2.ptemp,prof2.plevs,'r')


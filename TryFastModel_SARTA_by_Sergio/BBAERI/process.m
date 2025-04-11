xstartup

[r0,w] = readkcstd('test_up.dat');
[rc,w] = readkcstd('test_up_cld.dat');

blah = find(isnan(rc)); rc(blah) = 0.0;

addpath /home/sergio/MATLABCODE/FCONV/
addpath /home/sergio/MATLABCODE/FFTCONV/

[rch, wch] = xfconvkc_serg_iasi([r0 rc]', w, 'aeriB1', 'nb', 6);

figure(1); plot(wch,rad2bt(wch,rch(1,:)'),wch,rad2bt(wch,rch(2,:)'),'r')
  xlabel('Wavenumber cm-1'); ylabel('BT (K)')
  hl = legend('clear','cloud'); set(hl,'fontsize',10); title('MLW')
figure(2); plot(wch,rch(1,:)',wch,rch(2,:)','r')
  xlabel('Wavenumber cm-1'); ylabel('rad mW.cm2/sr-1/cm-1')
  hl = legend('clear','cloud'); set(hl,'fontsize',10); title('MLW')

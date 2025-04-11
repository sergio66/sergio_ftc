https://stackoverflow.com/questions/874461/read-mat-files-in-python
[sergio@strowinteract CLOUD_RCALC]$ python
Python 2.6.6 (r266:84292, Aug 18 2016, 15:13:37)
[GCC 4.4.7 20120313 (Red Hat 4.4.7-17)] on linux2
Type "help", "copyright", "credits" or "license" for more information.

import scipy.io
mat = scipy.io.loadmat('/asl/ftp/pub/sergio/EMADDY/CLOUD_RCALC//stats_365_2003.mat')
ff = mat.get('fuse');
ff
wow = mat.get('data');
wow

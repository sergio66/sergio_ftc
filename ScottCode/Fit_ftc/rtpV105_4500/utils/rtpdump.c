
/* rtpdump -- basic RTP dump utility
 *
 * USAGE
 *
 *   rtpdump [-achp] [-n k] rtpfile
 *
 * OPTIONS
 *
 *   -a      dump attributes
 *   -c      dump RTP channel info
 *   -h      dump header structure
 *   -p      dump profile structure
 *   -n <k>  select profile <k> for channel or profile 
 *           structure dumps; defaults to 1
 *
 * BUGS
 *
 *   the output is from debug and error dump routines and is not very
 *   fancy; the -p option only prints a subset of profile fields
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include "hdf.h"

#define RTPDEF
#include "rtp.h"

#define USAGE "Usage: rtpdump [-achp] [-n <k>] rtpfile\n"

main (int argc, char *argv[]) {

  int i, j, c, s1;

  struct rtp_head head;
  struct rtp_prof prof;

  struct ALIST (*halist)[], (*palist)[];
  int hnattr, pnattr;

  char *hfile;
  int rchan;

  int adump = 0;
  int cdump = 0;
  int hdump = 0;
  int pdump = 0;
  int pnum = 1;

  extern int getopt();
  extern char *optarg;
  extern int   optind;

  while ((c=getopt(argc, argv, "achpn:")) != EOF)
    switch (c) {
    case 'a': adump = 1; break;
    case 'c': cdump = 1; break;
    case 'h': hdump = 1; break;
    case 'p': pdump = 1; break;
    case 'n': pnum = atoi(optarg); break;
    case '?':       
      fprintf(stdout, USAGE);
      exit(2);
      break;
    }

  if (optind == argc) {
    fprintf(stdout, USAGE);
    exit(2);
  }
  hfile = argv[optind];

  /* read the header and attributes
   */
  s1 = rtpread1(hfile, 
		&head,
		&halist, &hnattr,
		&palist, &pnattr,        
		&rchan);

  if (s1 != 0) {
    fprintf(stderr, "rtpdump: header read failed\n");
    exit(2);
  }
  else {
    fprintf(stderr, "rtpdump: read header OK\n");
  }

  /* option for attribute dump */
  if (adump) {
    dump_attrs(halist, hnattr, "header attributes");
    dump_attrs(palist, pnattr, "profile attributes");
  }

  /* read to the specified profile
   */
  for (s1 = 1, i = 0; i < pnum && s1 == 1; i++) {
    profinit(&prof);
    s1 = rtpread2(rchan, &prof);
  }
  if (s1 != 1 || i == 0) {
    fprintf(stderr, "rtpdump: profile %d read failed\n", i);
    exit(2);
  }
  else {
    fprintf(stderr, "rtpdump: read profile %d\n", i);
  }
  
  /* option for channel info dump */
  if (cdump)
    dump_chan(rchan);

  /* option for header info dump */
  if (hdump)
    dump_hstr(&head);

  /* option for profile info dump */
  if (pdump) 
    dump_pstr(&head, &prof);

  rtpclose1(rchan);
}

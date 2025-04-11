
/* pvdefs.h  -- parameters and structures for using HDF vdatas
 */

/* set buffer size bounds; note that MAXVNAME, MAXANAME, and 
 * MAXATEXT must match the values set in rtpdefs.f
 */
#define MAXVNAME 64	/* bound vdata and field name length	*/
#define MAXANAME 64	/* bound attribute name length		*/
#define MAXATEXT 1024	/* bound single attribute value		*/
#define MAXFLIST 2048	/* bound concatenated vdata field names	*/


/* FLIST -- a structure for reporting vdata field info
 * 
 * An FLIST is an array of records describing the fields of a
 * vdata buffer or C structure, with one record for each vdata or
 * structure field.  "fname" is the field name, "htype" is the HDF
 * data type, and "order" is the number of htype elements.
 * 
 */
struct FLIST {
  char *fname;
  int htype;
  int order;
};


/* ALIST -- a structure for passing vdata attributes
 *
 * An ALIST is an array of records storing HDF attributes, used
 * to pass attributes to and from HDF files in a convenient way.
 * For HDF vdata, fname can be either a field name or vdata name,
 * aname is the attribute name, and atext the attribute value.
 *  
 */
struct ALIST {
  char *fname;
  char *aname;
  char *atext;
};


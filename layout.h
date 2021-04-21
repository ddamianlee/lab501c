//#define MAXLEN 99999999
POBJ_LAYOUT_BEGIN(rstore);
POBJ_LAYOUT_ROOT(rstore, struct my_root);
POBJ_LAYOUT_END(rstore);

struct my_root
{
	char r[16384];
};


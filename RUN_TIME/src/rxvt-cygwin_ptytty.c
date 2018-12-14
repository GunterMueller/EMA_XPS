/***

hg20081112:

http://www.aleph0.com/computing/rxvt-max/

v2.7.10-6
The snippets below are taken from ptytty.c


http://www.cygwin.com/cygwin-ug-net/using-specialnames.html

per default the specialfile /dev/ptmx is not found in Cygwin's
/dev directory! cygwin1.dll makes this work all the same!

***/


/* ------------------------------------------------------------------------- *
 *                  GET PSEUDO TELETYPE - MASTER AND SLAVE                   *
 * ------------------------------------------------------------------------- */
/*
 * Returns pty file descriptor, or -1 on failure 
 * If successful, ttydev is set to the name of the slave device.
 * fd_tty _may_ also be set to an open fd to the slave device
 */

int
rxvt_get_pty(int *fd_tty, const char **ttydev)
{
    int             pfd;
    {
	extern char    *ptsname();
	pfd = open("/dev/ptmx", O_RDWR | O_NOCTTY, 0);
	if (pfd >= 0) {
	    if (grantpt(pfd) == 0	/* change slave permissions */
		&& unlockpt(pfd) == 0) {	/* slave now unlocked */
		*ttydev = ptsname(pfd);	/* get slave's name */
		return pfd;
	    }
	    close(pfd);
	}
    }
    return -1;
}

/*----------------------------------------------------------------------*/
/*
 * Returns tty file descriptor, or -1 on failure 
 */
/* EXTPROTO */
int
rxvt_get_tty(const char *ttydev)
{
    return open(ttydev, O_RDWR | O_NOCTTY, 0);
}


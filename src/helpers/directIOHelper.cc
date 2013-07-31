/**
 * @file directIOHelper.cc
 * @author Beata Skiba
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#define FUSE_USE_VERSION 29

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif /* linux */

#include <fuse.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <sys/time.h>
#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif

#include <limits.h>

#include "helpers/directIOHelper.hh"
#include "glog/logging.h"

#include <iostream>
using namespace std;

static char root_path[PATH_MAX];
static int root_path_len;


static void free_path(const char *path)
{
	free((void*) path);
}

const char * get_full_path(const char *path)
{
    int path_len = root_path_len + strlen(path) + 2;

    char * new_path = (char *)calloc(path_len, sizeof(char));

    memcpy(new_path, root_path, root_path_len);
    new_path[root_path_len] = '/';
    strcat(new_path, path);

    return new_path;
}

int DirectIOHelper::sh_getattr(const char *path, struct stat *stbuf)
{
	int res;
	path = get_full_path(path);
	res = lstat(path, stbuf);
	free_path(path);
	if (res == -1)
		return -errno;
	return 0;
}

int DirectIOHelper::sh_access(const char *path, int mask)
{
	int res;
	path = get_full_path(path);

	res = access(path, mask);
	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_readlink(const char *path, char *buf, size_t size)
{
	int res;
	path = get_full_path(path);

	res = readlink(path, buf, size - 1);
	free_path(path);
	if (res == -1)
		return -errno;

	buf[res] = '\0';
	return 0;
}


int DirectIOHelper::sh_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
		       off_t offset, struct fuse_file_info *fi)
{
	DIR *dp;
	struct dirent *de;

	(void) offset;
	(void) fi;
	path = get_full_path(path);

	dp = opendir(path);
	free_path(path);
	if (dp == NULL)
		return -errno;

	while ((de = readdir(dp)) != NULL) {
		struct stat st;
		memset(&st, 0, sizeof(st));
		st.st_ino = de->d_ino;
		st.st_mode = de->d_type << 12;
		if (filler(buf, de->d_name, &st, 0))
			break;
	}

	closedir(dp);
	return 0;
}

int DirectIOHelper::sh_mknod(const char *path, mode_t mode, dev_t rdev)
{
	int res;
	path = get_full_path(path);

	/* On Linux this could just be 'mknod(path, mode, rdev)' but this
	   is more portable */
	if (S_ISREG(mode)) {
		res = open(path, O_CREAT | O_EXCL | O_WRONLY, mode);
		if (res >= 0)
			res = close(res);
	} else if (S_ISFIFO(mode))
		res = mkfifo(path, mode);
	else
		res = mknod(path, mode, rdev);

	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_mkdir(const char *path, mode_t mode)
{
	int res;
	path = get_full_path(path);

	res = mkdir(path, mode);
	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_unlink(const char *path)
{
	int res;
	path = get_full_path(path);

	res = unlink(path);
	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_rmdir(const char *path)
{
	int res;
	path = get_full_path(path);

	res = rmdir(path);
	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_symlink(const char *from, const char *to)
{
	int res;
	from = get_full_path(from);
	to = get_full_path(to);

	res = symlink(from, to);
	free_path(from);
	free_path(to);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_rename(const char *from, const char *to)
{
	int res;
	from = get_full_path(from);
	to = get_full_path(to);

	res = rename(from, to);
	free_path(from);
	free_path(to);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_link(const char *from, const char *to)
{
	int res;
	from = get_full_path(from);
	to = get_full_path(to);

	res = link(from, to);
	free_path(from);
	free_path(to);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_chmod(const char *path, mode_t mode)
{
	int res;
	path = get_full_path(path);

	res = chmod(path, mode);
	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_chown(const char *path, uid_t uid, gid_t gid)
{
	int res;
	path = get_full_path(path);

	res = lchown(path, uid, gid);
	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_truncate(const char *path, off_t size)
{
	int res;
	path = get_full_path(path);

	res = truncate(path, size);
	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}

#ifdef HAVE_UTIMENSAT
int DirectIOHelper::sh_utimens(const char *path, const struct timespec ts[2])
{
	int res;
	path = get_full_path(path);

	/* don't use utime/utimes since they follow symlinks */
	res = utimensat(0, path, ts, AT_SYMLINK_NOFOLLOW);

	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}
#endif /* HAVE_UTIMENSAT */

int DirectIOHelper::sh_open(const char *path, struct fuse_file_info *fi)
{
	int res;
	path = get_full_path(path);

	res = open(path, fi->flags);

	free_path(path);
	if (res == -1)
		return -errno;

	close(res);
	return 0;
}

int DirectIOHelper::sh_read(const char *path, char *buf, size_t size, off_t offset,
		    struct fuse_file_info *fi)
{
	int fd;
	int res;
	path = get_full_path(path);

	(void) fi;
	fd = open(path, O_RDONLY);

	free_path(path);
	if (fd == -1)
		return -errno;

	res = pread(fd, buf, size, offset);
	if (res == -1)
		res = -errno;

	close(fd);
	return res;
}

int DirectIOHelper::sh_write(const char *path, const char *buf, size_t size,
		     off_t offset, struct fuse_file_info *fi)
{
	int fd;
	int res;
	path = get_full_path(path);

	(void) fi;
	fd = open(path, O_WRONLY);

	free_path(path);
	if (fd == -1)
		return -errno;

	res = pwrite(fd, buf, size, offset);
	if (res == -1)
		res = -errno;

	close(fd);
	return res;
}

int DirectIOHelper::sh_statfs(const char *path, struct statvfs *stbuf)
{
	int res;
	path = get_full_path(path);

	res = statvfs(path, stbuf);

	free_path(path);
	if (res == -1)
		return -errno;

	return 0;
}

int DirectIOHelper::sh_release(const char *path, struct fuse_file_info *fi)
{
	/* Just a stub.	 This method is optional and can safely be left
	   unimplemented */

	(void) path;
	(void) fi;
	return 0;
}

int DirectIOHelper::sh_fsync(const char *path, int isdatasync,
		     struct fuse_file_info *fi)
{
	/* Just a stub.	 This method is optional and can safely be left
	   unimplemented */

	(void) path;
	(void) isdatasync;
	(void) fi;
	return 0;
}

#ifdef HAVE_POSIX_FALLOCATE
int DirectIOHelper::sh_fallocate(const char *path, int mode,
			off_t offset, off_t length, struct fuse_file_info *fi)
{
	int fd;
	int res;
	path = get_full_path(path);

	(void) fi;

	if (mode)
		return -EOPNOTSUPP;

	fd = open(path, O_WRONLY);

	free_path(path);
	if (fd == -1)
		return -errno;

	res = -posix_fallocate(fd, offset, length);

	close(fd);
	return res;
}
#endif  /* HAVE_POSIX_FALLOCATE */

#ifdef HAVE_SETXATTR
/* xattr operations are optional and can safely be left unimplemented */
int DirectIOHelper::sh_setxattr(const char *path, const char *name, const char *value,
			size_t size, int flags)
{
	path = get_full_path(path);

	int res = lsetxattr(path, name, value, size, flags);

	free_path(path);
	if (res == -1)
		return -errno;
	return 0;
}

int DirectIOHelper::sh_getxattr(const char *path, const char *name, char *value,
			size_t size)
{
	path = get_full_path(path);

	int res = lgetxattr(path, name, value, size);

	free_path(path);
	if (res == -1)
		return -errno;
	return res;
}

int DirectIOHelper::sh_listxattr(const char *path, char *list, size_t size)
{
	path = get_full_path(path);

	int res = llistxattr(path, list, size);

	free_path(path);
	if (res == -1)
		return -errno;
	return res;
}

int DirectIOHelper::sh_removexattr(const char *path, const char *name)
{
	path = get_full_path(path);

	int res = lremovexattr(path, name);

	free_path(path);
	if (res == -1)
		return -errno;
	return 0;
}

#endif /* HAVE_SETXATTR */

DirectIOHelper::DirectIOHelper(std::vector<std::string> args) {
    if(args.size())
        strncpy(root_path, args[0].c_str(), PATH_MAX);
    root_path_len = strlen(root_path);
    LOG(INFO) << "Creating DirectIOHelper with root: " << string(root_path);
}

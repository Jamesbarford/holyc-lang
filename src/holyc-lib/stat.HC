#define RM_RECURSE  0x1
#define RM_VERBOSE  0x2
#define RM_FORCE    0x4
#define INTERNAL_RM_DIRECORY 0x6

static I32 MkDirIfNotExist(U8 *path, I32 mode=MKDIR_DEFAULT)
{
  cStat st;
  if (stat(path, &st) != 0) {
    if (mkdir(path, mode) != 0) {
      /* Means it already exists */
      return 0;
    }
  } else if (!((st.st_mode & S_IFMT) == S_IFDIR)) {
    return -1;
  }
  return 0;
}

public I32 RmDir(U8 *path, Bool flags=0)
{
  I32 is_verbose = flags & RM_VERBOSE, result,r2;
  I64 pathlen,buflen;
  U8 *buf;
  cDIR *dir;
  cStat st;
  Dirent *ent;
  result = -1;

  if (!(flags & RM_RECURSE)) {
    result = rmdir(path);
    if (is_verbose) {
      if (result == 0) "removed dir '%s'\n",path;
      else             "ERROR removing dir '%s', is it empty?\n",path;
    }
    return result;
  }

  if ((dir = opendir(path)) == NULL) {
    return result;
  }

  result = 0;
  while (!result && (ent = readdir(dir))) {
    r2 = -1;
    if (!StrCmp(ent->name,".") || !StrCmp(ent->name,"..")) {
      continue;
    }

    buf = StrPrint(NULL,"%s/%s",path,ent->name);
    if (buf) {
      if (ent->type & DT_DIR) {
        r2 = RmDir(buf,flags);
      } else if (ent->type & DT_REG) {

        if ((r2 = unlink(buf)) == 0 && flags & RM_VERBOSE) {
          "Rm: %s\n",buf;
        } else {
          "Error Rm: %s\n",buf;
        }
      } else if (!stat(buf,&st)) {
          if ((st.st_mode & S_IFMT) == S_IFDIR) {
            r2 = RmDir(path,flags);
          } else {
            if ((r2 = unlink(buf)) == 0 && flags & RM_VERBOSE) {
              "Rm: %s\n",buf;
            } else {
              "Error Rm: %s\n",buf;
            }
          }
        }
      } else {
        r2 = -1;
        if (is_verbose) {
          "Unknown file type to delete: 0x%X\n",ent->type;
        }

      }
      Free(buf);
      result = r2;
  }
  closedir(dir);

  if (!result && path) {
    result = rmdir(path);
    if (flags & RM_VERBOSE) {
      if (result == 0) {
      "RmDir %s\n",path;
      } else {
        "ERROR: RmDir %s\n",path;
      }
    }
  }

  return result;
}

public I32 Rm(U8 *path, Bool flags=0)
{ /* DANGER Remove a file or directory, this is powerful but can cause a world 
   * of pain */
  I32 result;
  cStat st;

  if (stat(path,&st) == -1) {
    return -1;
  }

  if ((st.st_mode & S_IFMT) == S_IFDIR) {
    flags |= INTERNAL_RM_DIRECORY; 
  }

  if (flags == 0) {
    result = unlink(path);
    if (flags & RM_VERBOSE) {
      "Rm '%s'";
      if (result != 0) " failed\n";
      else             "\n";
    }
    return result;
  } 

  /* Just try to remove a directory */
  if (!(flags & RM_RECURSE) && flags & INTERNAL_RM_DIRECORY) {
    return RmDir(path,flags);
  }

  /* Destroy everything and everything inbetween */
  if (flags & (RM_RECURSE|RM_FORCE)) {
    return RmDir(path,flags);
  } 
  return 0;
}

public Bool MkDir(U8 *path, I32 mode=MKDIR_DEFAULT, Bool recurse=FALSE)
{ /* Make a directory, by default does not recurse folder permissions are: 
   * drwxr-xr-x */
  Bool relative = TRUE;
  U8 *ptr,tmp[DIR_MAXPATHLEN];
  I64 i;
  if (path == NULL) return -1;
  if (path[0] == '/') {
    relative = FALSE;
  }

  if (recurse == FALSE) {
    return MkDirIfNotExist(path,mode);
  } else {
    ptr = path;
    i = 0;
    if (*ptr == '.' && *(ptr + 1) == '/') {
      ptr += 2;
    }
    while (*ptr) {
      tmp[i++] = *ptr;
      if (i > 1 && *ptr == '/') {
        tmp[i] = '\0';
        if (MkDirIfNotExist(tmp,mode) != 0) {
          return FALSE;
        } 
      }
      ptr++;
    }
    if (*ptr == '\0' && *(ptr -1) != '/') {
      tmp[i] = '\0';
      MkDirIfNotExist(tmp,mode);
    }
  }
  return TRUE;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

typedef struct String String;
struct String
{
  char *data;
  size_t size;
};

typedef struct StringView StringView;
struct StringView
{
  const char *data;
  size_t size;
};

const char *
is_prefix(const char *prefix, const char *string)
{
  char p, s;

  do
    {
      p = *prefix++;
      s = *string++;
    }
  while (p != '\0' && p == s);

  return p != '\0' ? NULL : string - 1;
}

void *
malloc_or_exit(size_t size)
{
  void *data = malloc(size);

  if (data == NULL)
    assert(false);

  return data;
}

void *
realloc_or_exit(void *data, size_t size)
{
  void *new_data = realloc(data, size);

  if (new_data == NULL)
    assert(false);

  return new_data;
}

char *
read_entire_file(const char *filepath)
{
  enum
    {
      Ok,
      Failed_To_Open_File,
      Failed_To_Stat_File,
      Failed_To_Read_File,
      Failed_To_Close_File
    } status = Ok;

  int fd = open(filepath, O_RDONLY);

  if (fd == -1)
    {
      status = Failed_To_Open_File;
      goto skip_body;
    }

  off_t file_size = 0;

  {
    struct stat stats;

    if (fstat(fd, &stats) == -1)
      {
        status = Failed_To_Stat_File;
        goto skip_body;
      }

    file_size = stats.st_size;
  }

  char *file_data = malloc_or_exit(file_size + 1);

  if (read(fd, file_data, file_size) != file_size)
    {
      status = Failed_To_Read_File;
      goto skip_body;
    }

  file_data[file_size] = '\0';

  if (close(fd) == -1)
    {
      status = Failed_To_Close_File;
      goto skip_body;
    }

 skip_body:
  ;

  const char *action = NULL;

  switch (status)
    {
    case Ok:
      return file_data;
    case Failed_To_Open_File:
      action = "open";
      break;
    case Failed_To_Stat_File:
      action = "stat";
      break;
    case Failed_To_Read_File:
      action = "read";
      break;
    case Failed_To_Close_File:
      action = "close";
      break;
    }

  fprintf(stderr,
          "ERROR: failed to %s file \'%s\'.\n",
          action,
          filepath);
  assert(false);
}

String
copy_view_to_string(StringView view)
{
  String str;
  str.data = malloc_or_exit(view.size + 1);
  str.size = view.size;

  memcpy(str.data, view.data, view.size);
  str.data[view.size] = '\0';

  return str;
}

StringView
string2view(String str)
{
  return (StringView){ str.data, str.size };
}

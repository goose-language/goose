#include <regex.h>
#include "value.h"
#include <stdlib.h>
#include "conversion.h"
#include <stdio.h>
#include "list.h"
#include "io.h"
#include "error.h"

nanbox_t Regex_get(nanbox_t args) {
  int begin; int end;
  char* string_ = decode_string(index_(args, 0));
  char* regex = decode_string(index_(args, 1));
  regex_t re;
  regmatch_t match;
  char* word;
  int len;
  int status = regcomp(&re, regex, REG_EXTENDED);
  if (status != 0) {
    throwError("Regex::get: invalid regex");
  }
  status = regexec(&re, string_, 1, &match, 0);
  if (status != 0) {
    return emptyList();
  }
  
  begin = (int)match.rm_so;
  end = (int)match.rm_eo;
  len = end-begin;

  word = (char*) malloc(sizeof(char) * (len + 1));
  int j = 0;
  for (int i = begin; i < end; i++) {
    word[j] = string_[i];
    j += 1;
  }
  word[len] = '\0';
  regfree(&re);
  return string(word);
}

nanbox_t Regex_test(nanbox_t args) {
  char* string_ = decode_string(index_(args, 0));
  char* regex = decode_string(index_(args, 1));
  regex_t re;
  regmatch_t match;
  int status = regcomp(&re, regex, REG_EXTENDED);
  if (status != 0) {
    return boolean(0);
  }
  status = regexec(&re, string_, 1, &match, 0);
  if (status != 0) {
    return boolean(0);
  }
  regfree(&re);
  return boolean(1);
}
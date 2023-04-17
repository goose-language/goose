#include <curl/curl.h>
#include <stdio.h>
#include "value.h"
#include "conversion.h"
#include "error.h"
#include <string.h>

struct string {
  char *ptr;
  size_t len;
};

void init_string(struct string *s) {
  s->len = 0;
  s->ptr = malloc(s->len+1);
  if (s->ptr == NULL) {
    fprintf(stderr, "malloc() failed\n");
    exit(EXIT_FAILURE);
  }
  s->ptr[0] = '\0';
}

size_t writefunc(void *ptr, size_t size, size_t nmemb, struct string *s)
{
  size_t new_len = s->len + size*nmemb;
  s->ptr = realloc(s->ptr, new_len+1);
  if (s->ptr == NULL) {
    fprintf(stderr, "realloc() failed\n");
    exit(EXIT_FAILURE);
  }
  memcpy(s->ptr+s->len, ptr, size*nmemb);
  s->ptr[new_len] = '\0';
  s->len = new_len;

  return size*nmemb;
}


nanbox_t HTTP_fetch(nanbox_t args) {
  Array arguments = decode_pointer(args)->as_array;
  char* url = decode_string(arguments.data[0]);
  char* headers = decode_string(arguments.data[1]);

  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();

  if (curl) {
    struct string s;
    init_string(&s);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writefunc);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&s);

    res = curl_easy_perform(curl);

    if (res != CURLE_OK) {
      throwError("HTTP::fetch: fetch failed (%s)", curl_easy_strerror(res));
    }

    curl_easy_cleanup(curl);
    return string(s.ptr);
  } else {
    throwError("HTTP::fetch: failed to initialize curl");
  }
  
}
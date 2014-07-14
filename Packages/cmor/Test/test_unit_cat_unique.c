#include <string.h>
#include "cmor.h"

int fail(int code, char *expect, char *got) {
  printf("failed test %d, expect '%s' got '%s'\n", code, expect, got);
  return code;
}

int test_cmor_stringinstring(void) {
  printf("running cmor_stringinstring tests\n");
  if (cmor_stringinstring("Inverted axis: rlat.", "Inverted axis: rlat")) {
    /* should match */
  } else {
    printf("failed stringinstring test with period after string\n");
    return 1;
  }
}
  
int test_cat_unique_string(void) {
  char dest[CMOR_MAX_STRING];
  char src[CMOR_MAX_STRING];
  char expected[CMOR_MAX_STRING];
  strcpy(dest, "");
  strcpy(src, "rumble");
  strcpy(expected, "rumble");

  /* 1. simple test: add string to blank */
  printf("running cat_unique_string tests\n");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(1, expected, dest);
  }

  /* 2. simple test: add string to itself. Should be identical to above */
  strcpy(dest, "rumble");
  strcpy(src, "rumble");
  strcpy(expected, "rumble");

  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(2, expected, dest);
  }

  /* 3. simple test: add string to non-blank, unique*/
  strcpy(src, "jungle");
  strcpy(expected, "rumble jungle");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(3, expected, dest);
  }

  /* 4. simple test: add string that exists within another word */
  strcpy(dest, "rumble jungle");
  strcpy(src, "umb");
  strcpy(expected, "rumble jungle umb");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(4, expected, dest);
  }

  /* 5. simple test: add string to blank */
  strcpy(dest, "rumble");
  strcpy(src, "rum");
  strcpy(expected, "rumble rum");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(5, expected, dest);
  }

  /* 6. simple test: add string to blank */
  strcpy(dest, "rumble");
  strcpy(src, "ble");
  strcpy(expected, "rumble ble");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(6, expected, dest);
  }

  /* 7. simple test: add string to blank */
  strcpy(dest, "rumble jungle happy");
  strcpy(src, "ppy");
  strcpy(expected, "rumble jungle happy ppy");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(7, expected, dest);
  }

  /* 8. simple test: add string to blank */
  strcpy(dest, "rumble jungle happy");
  strcpy(src, "gle");
  strcpy(expected, "rumble jungle happy gle");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(8, expected, dest);
  }

  /* 9. simple test: add string to blank */
  strcpy(dest, "rumble jungle happy");
  strcpy(src, "jung");
  strcpy(expected, "rumble jungle happy jung");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(9, expected, dest);
  }
  /* 10. simple test: add string to blank */
  strcpy(dest, "rumble jumble ble");
  strcpy(src, "ble");
  strcpy(expected, "rumble jumble ble");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(10, expected, dest);
  }

  /* 11. simple test: add latest */
  strcpy(dest, "rumble jumble");
  strcpy(src, "jumble");
  strcpy(expected, "rumble jumble");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(11, expected, dest);
  }

  /* 12. simple test: add latest */
  strcpy(dest, "rumble jumble");
  strcpy(src, "rumble");
  strcpy(expected, "rumble jumble");
  cmor_cat_unique_string(dest,src);
  if (strcmp(dest,expected)) {
    return fail(12, expected, dest);
  }

  return 0;

}

int main(int argc, char **argv) {
  test_cmor_stringinstring();
  test_cat_unique_string();
}

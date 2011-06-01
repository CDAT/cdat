#include <stdio.h>
#include <string.h>
#define R_OK 4
int main() {
  FILE *fp;
  char s1[50];
  fp = popen("git log -n 1 --pretty=\"format:%H\"  ","r");
  fscanf(fp,"%s",s1);
  printf("%s\n",s1);
  return 0;
}

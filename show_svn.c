#include <unistd.h>
#include <stdio.h>
#include <string.h>
int main() {
  FILE *fp;
  char s1[50];
  int version,i;
  i = access(".svn/entries",R_OK);
  if (i < 0) {
    return 0;
  }
  else {
    fp = popen("svn info| grep Revision ","r");
    fscanf(fp,"%s%d",&s1[0],&version);
    printf("%i\n",version);
  }
  return 0;
}

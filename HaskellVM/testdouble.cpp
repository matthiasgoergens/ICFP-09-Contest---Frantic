#include <stdio.h>


int main(){
  double k = 1.23456;
  fwrite(&k,sizeof(double),1,stdout);
  //  int i = 5;
  //fwrite(&i,sizeof(int),1,stdout);
  return 0;
}

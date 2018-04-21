/* FEOF example */
#include <stdio.h>
int main()
{
   FILE * pFile;
   char buffer [100];
   pFile = fopen ("myfile.txt" , "r");
   if (pFile == NULL) perror ("Error opening file");
   else {
     while ( ! feof (pFile) ) {
       if ( fgets (buffer , 100 , pFile) == NULL ) break;
       fputs (buffer , stdout);
     }
     fclose (pFile);
   }
   return 0;
}

#include <stdio.h>
#include <dirent.h>
#include <string.h>
#include <fnmatch.h>
#include <sys/stat.h>
#include <unistd.h>

int csource;
int cheader;
int object;
int make;
int other ;	
int shell ;
int executable;

//Array Contains
int shellArray[2] = {33,35};
int execArray[4] = {70,76,69,127};
	
	
	

void printFiles(){
	
  printf("C source:      %d\n", csource);
  printf("C header:      %d\n", cheader);
  printf("Object:        %d\n", object);
  printf("Make:          %d\n", make);
  printf("Executable:    %d\n", executable);
  printf("Shell:         %d\n", shell);
  printf("Other:         %d\n", other);

	
}

int magicNum (int size, int numbers[],char *file){
	
	FILE *fp;
	
	int tomatch[sizeof(size)];
	
	
	fp = fopen(file,"r");
	
	
	if (!fp){
		
		
		return -1;
	}
	
	int a ;
	
	for (a=0;a<size;a++){
		
		
		
		fread(&tomatch,1,1,fp);
		printf("%d \n",tomatch[a]);
	}
	
	
	
	
	fclose(fp);

	for (a=0 ; a < size; a++){
		
	

		
		if ( tomatch[a] = numbers[a]){
		printf ("!!!!!\n");
			continue;
			
		}
		
		else {
			printf ("-1\n");
			return -1;
		}
	
	}

	
	printf ("0\n");
	return 0;

	
	
}


void main (int argc , char *argv[] ){
	
	
int a;

for (a=1 ; a < argc ; a++){
	
	
	
	int len = strlen(argv[a]);
	
	if (access(argv[a],F_OK)==-1){
		
		fprintf(stderr,"File does not exist %s \n",argv[a]);
		continue;
	}
	
	
	if (strcmp(".c",argv[a] + len -2 ) == 0){
		
		csource = csource +1;
		continue;
	
	}
	
	
	if (strcmp("makefile",argv[a] + len -8) == 0){
		
		make = make +1;
		continue;
	
	}
	if (strcmp("Makefile",argv[a]+len -8) == 0){
		
		make = make +1;
		continue;
	
	}
	
	if (strcmp(".o",argv[a] + len -2 ) == 0){
		
		object = object +1;
		continue;
	
	}
	
	if (strcmp(".h",argv[a] + len -2 ) == 0){
		
		cheader = cheader +1;
		continue;
	
	
	}
	
	if (strcmp ("/subdir/" , argv[a] ) == 0 ){
		
		other = other +1 ;
		continue;
	}
	
	if (magicNum (2 ,shellArray,argv[a]) == 0){
		
		
		
		shell = shell+1;
		
		continue;
	}
		
	
	
	
	if (magicNum (4, execArray,argv[a]) == 0){
		
		executable = executable+1;
		
		continue;
		
	}
	
	
	else {
		
	other = other +1;

	
	}
	
	
	
}

printFiles();




}
	
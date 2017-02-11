#include<stdio.h>
#include<string.h>
#include<math.h>
#include<stdlib.h>

#define minSIZE 5000
#define SIZE 38312
//#define SIZE 5000
#define PI 3.14159265
#define DISTANCE 0.25
#define NDISTANCE 0.35

double toRadian(double degrees) {
  return (degrees * PI) / 180.0;
}
double distancebetweenlatlon(double lat1, double lng1, double lat2, double lng2) {
  double earthRadius = 6378.137; // KM: use mile here if you want mile result 6378.137
  double dLat = toRadian(lat2 - lat1);
  double dLng = toRadian(lng2 - lng1);
  double a = pow(sin(dLat/2), 2)  + cos(toRadian(lat1)) * cos(toRadian(lat2)) * pow(sin(dLng/2), 2);
  double c = 2 * atan2(sqrt(a), sqrt(1-a));
  return earthRadius * c; // returns result kilometers
}
double calc_entropy(double p[], int length){
   int i;
   double entropy = 0.0;
   for(i=0; i< minSIZE; i++)
      if(p[i] > 0.0 && length > 1){
         entropy -= (p[i] * log2(p[i]))/(length * log2(length));
	  }
   //printf("entropy in subroutine is %f\n",entropy);
   return entropy;
}
int sumofchar(char ch[]){
	int sum = 0;
	for(int i = 0; i<strlen(ch); i++){
		sum = sum + ch[i];
	}
	//printf("sum of char = %d\n",sum);
	return sum;
}
const char* getfield(char* line, int num)
{
    const char* tok;
    for (tok = strtok(line, ",\t");
            tok && *tok;
            tok = strtok(NULL, ",\t"))
    {
        if (!--num)
            return tok;
    }
    return NULL;
}
int main()
{
	FILE* stream = fopen("finalcheckindata.csv", "r");
    //FILE* out = fopen("densityout.txt","w");
    FILE* out = fopen("tmpfileout.txt","w");
    char line[1024];
    //double neighbours[10] = {1,0,1,3,1,2,1,1};
    //calc_entropy(neighbours,8);
    //exit(0);
    double density[SIZE][2];
    int densitycount[SIZE] = {0};
    int venuetypes[SIZE];
    int checkincount[SIZE] = {0};
    double neighbour_entropy[SIZE] = {0};
    int i = 0, j = 0;
    printf("Reading Data...\n");
    while (fgets(line, 1024, stream))
    {
		if(j==0){
			j++;
			continue;
		}
		//printf("%s", line);
        char* tmp = strdup(line);
        char* tmp1 = strdup(line);
        char* tmp2 = strdup(line);
        double lat = atof(getfield(tmp,4));
        double lon = atof(getfield(tmp1,5));
        //strcpy(venuetypes[i],getfield(tmp2,3));
        venuetypes[i] = sumofchar(getfield(tmp2,6));
        density[i][0] = lat;
        density[i][1] = lon;
        i++;
        //printf("Lat & Lon (%lf, %lf) %d\n", lat, lon, i);
        free(tmp);
        if(i>SIZE)break;
    }
    printf("Calculating two features...for size = %d\n", SIZE);
    j = 0;
    for(i=0; i<SIZE; i++){
		int count = 0, nindex = 0;
		double neighbours[minSIZE] = {0};
		for(j=0; j<SIZE; j++){
			if(i!=j){
				double dis = distancebetweenlatlon(density[i][0], density[i][1], density[j][0], density[j][1]);
				if(dis < DISTANCE && dis!=0){
					count++;
				}
				if(dis < NDISTANCE && dis!=0 && venuetypes[i]!=venuetypes[j]){
					neighbours[venuetypes[j]]++;
					nindex++;
				}
			}
		}
		printf("%d ",i);
		neighbour_entropy[i] = calc_entropy(neighbours, nindex)==0?-2.0:calc_entropy(neighbours, nindex);
		densitycount[i] = count;
	}
    for(i=0; i<SIZE; i++){
		printf("\nWriting .. %d\n", i);
		for(j=0; j<2; j++){
				fprintf(out, "%lf ", density[i][j]);
		}
		fprintf(out, "%d ", densitycount[i]);
		fprintf(out, "%lf ", neighbour_entropy[i]);
		fprintf(out, "\n");
	}
	fclose(out);
	//printf("%lf", distancebetweenlatlon(40.719810,-74.002581,40.606800,-74.044170));
	return 0;
}

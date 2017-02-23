#include<stdio.h>
#include<string.h>
#include<math.h>
#include<stdlib.h>
#include<set>
#include<string>
#include<vector>
#include<iostream>
#include<fstream>

#define minSIZE 5000
#define SIZE 227427
//#define SIZE 50000
#define uSIZE 38312
#define PI 3.14159265
#define DISTANCE 0.25
#define NDISTANCE 0.35

using namespace std;

char* getfield(char* line, int num)
{
    char* tok;
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
	FILE* stream = fopen("dataset_TSMC2014_NYC.txt", "r");
    //FILE* out = fopen("densityout.txt","w");
    //FILE* out = fopen("tmpfileoutunique.txt","w");
    ofstream out ("tmpfileoutunique1.txt");
    char line[1024];
    vector<string> venueid;
    vector<string>::iterator uniquevenueiditr;
    set<string>::iterator venueiditr;
    vector<int> userid;
    vector<int>::iterator useriditr;
    set<string> uniquevenueid;
    
    vector<int> ncheckins;
    vector<int> uncheckins;
    int i = 0, j = 0;
    printf("Reading Data...\n");
    
    while (fgets(line, 1024, stream))
    {
		if(j==0){
			j++;
			continue;
		}
        char* tmp = strdup(line);
        char* tmp1 = strdup(line);
        userid.push_back(atoi(getfield(tmp,1)));
        char* vtmp = getfield(tmp1,2);
		venueid.push_back(vtmp);
		venueiditr = uniquevenueid.find(vtmp);
		if(venueiditr == uniquevenueid.end())
		{
			uniquevenueid.insert(vtmp);
		}
        //printf("%s %s\n", getfield(tmp,1), vtmp);
        //strcpy(venuetypes[i],getfield(tmp2,3));
        //venuetypes[i] = sumofchar(getfield(tmp2,6));
        free(tmp);
        //free(tmp1);
        //free(vtmp);
        i++;
        if(i>SIZE)break;
    }
    
    printf("Calculating one feature...for size = %d, i = %d\n", SIZE,i);
    int totalcheckins = 0;
    int venueindex = 0;
    int k=0;
    for(venueiditr=uniquevenueid.begin();venueiditr!=uniquevenueid.end();++venueiditr){
		
		
		string s = *venueiditr;
		int checkins = 0, ucheckins = 0;
		set<int> myset;
		set<int>::iterator it;
		
		for(uniquevenueiditr = venueid.begin(), useriditr = userid.begin();useriditr!=userid.end(); ++uniquevenueiditr, ++useriditr){
				string tmp = *uniquevenueiditr;
				//cout<<s<<":"<<tmp<<endl;
				if(s == tmp){
					checkins++;
					it = myset.find(*useriditr);
					if(it == myset.end()){
						ucheckins++;
						myset.insert(*useriditr);
						//userid.erase(*useriditr);
					}
					//venueid.erase(*uniquevenueiditr);
				}
	
		}
		checkins++;
		totalcheckins += checkins;
		ncheckins.push_back(checkins);
		uncheckins.push_back(ucheckins);
		venueindex++;
		printf("k=%d\n",k);
		k++;
	//	cout<<"unique:"<<*venueiditr<<" \n";
		
	}
    printf("venueidstr = %d, ncheckins = %d, ucheckins = %d\n", (int)uniquevenueid.size(), (int)ncheckins.size(), (int)uncheckins.size());
	printf("venueindex = %d\n",venueindex);
	vector<int>::iterator t;
	vector<int>::iterator t1;
	venueiditr = uniquevenueid.begin();
	i = 0;
	totalcheckins = 0;
	for(t=ncheckins.begin(), t1= uncheckins.begin(); t!=ncheckins.end(); ++t, ++t1, ++venueiditr){
		printf("\nWriting .. %d\n", i++);
		totalcheckins += (int) (*t);
		out<< *venueiditr << " ";
		out<<*t << " ";
		out<<*t1<<" ";
		out<<((1.0)*(*t1))/((1.0)*(*t))<<" ";
		out<<"\n";
	}
	printf("uniquevenuesize = %d, ncheckins = %d, ucheckins = %d\n", (int)uniquevenueid.size(), (int)ncheckins.size(), (int)uncheckins.size());
	printf("venuesize = %d, Total checkins = %d\n",(int)venueid.size(), totalcheckins);
	//fclose(out);
	//printf("%lf", distancebetweenlatlon(40.719810,-74.002581,40.606800,-74.044170));
	return 0;
}

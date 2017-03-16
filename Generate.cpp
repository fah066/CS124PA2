#include <iostream>
#include <fstream>
#include<ctime>
using namespace std;
int main(int argc, char** argv)
{
    int n =2049;
    ofstream myfile;
    string filename="2049.txt";
    myfile.open (filename);
    srand(time(NULL));
    int* mat = new int[2*n*n];
    for(int i=0;i<2*n*n;i++)
    {
        mat[i]=rand()%100;
        myfile <<mat[i]<<endl;
    }
    myfile.close();
    return 0;
}


#include<stdio.h>
#include <time.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <cmath>
#include <cstring>
#include<chrono>

using namespace std;

// This function allocates the matrix
inline int** initiate_matrix(int n)
{
	int** mat = new int*[n];
	for (int i = 0; i<n; ++i)
	{
		mat[i] = new int[n];
		//initialize all values as 0
		memset(mat[i], 0, sizeof(int)*n);
	}
	return (mat); // returns the pointer to the vector.
}

// This function frees the matrix
inline void free_matrix(int **M, int n)
{
	for (int i = 0; i < n; i++)
	{
		delete[] M[i];
	}

	delete[] M; // frees the pointer /
	M = NULL;
}

//sum two matrices
inline void sum(int **a, int **b, int **sum, int n) {

	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			sum[i][j] = a[i][j] + b[i][j];
		}
	}
}

// subtract two matrices
inline void subtract(int**a, int **b, int **sub, int tam) {
	for (int i = 0; i < tam; i++) {
		for (int j = 0; j < tam; j++) {
			sub[i][j] = a[i][j] - b[i][j];
		}
	}
}

// naive method
void naive(int** A, int** B, int** C, int n)
{
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++) {
			C[i][j] = 0;
			for (int k = 0; k<n; k++)
				C[i][j] += A[i][k] * B[k][j];
		}
}

// Strassen's method
void strassen(int **a, int **b, int **c, int n, int cross)
{
	if (n <= cross)
	{
		naive(a, b, c, n);
		return;
	}
	int newn = 0;
	if (n % 2 == 1)
	{
		newn= (n+1) / 2;
	}
	if (n % 2 == 0)
	{
		newn = (n) / 2;
	}
	int **a11, **a12, **a21, **a22;
	int **b11, **b12, **b21, **b22;
	int **c11, **c12, **c21, **c22;
	int **p1, **p2, **p3, **p4, **p5, **p6, **p7;

	// memory allocation:
	a11 = initiate_matrix(newn);
	a12 = initiate_matrix(newn);
	a21 = initiate_matrix(newn);
	a22 = initiate_matrix(newn);

	b11 = initiate_matrix(newn);
	b12 = initiate_matrix(newn);
	b21 = initiate_matrix(newn);
	b22 = initiate_matrix(newn);

	c11 = initiate_matrix(newn);
	c12 = initiate_matrix(newn);
	c21 = initiate_matrix(newn);
	c22 = initiate_matrix(newn);

	p1 = initiate_matrix(newn);
	p2 = initiate_matrix(newn);
	p3 = initiate_matrix(newn);
	p4 = initiate_matrix(newn);
	p5 = initiate_matrix(newn);
	p6 = initiate_matrix(newn);
	p7 = initiate_matrix(newn);

	int **parta = initiate_matrix(newn);
	int **partb = initiate_matrix(newn);

	//dividing the matrices in 4 sub-matrices:
	if(n%2==0)
	{
		for (int i = 0; i < newn; i++) {
			for (int j = 0; j < newn; j++) {
				a11[i][j] = a[i][j];
				a12[i][j] = a[i][j + newn];
				a21[i][j] = a[i + newn][j];
				a22[i][j] = a[i + newn][j + newn];

				b11[i][j] = b[i][j];
				b12[i][j] = b[i][j + newn];
				b21[i][j] = b[i + newn][j];
				b22[i][j] = b[i + newn][j + newn];
			}
		}
	}
	if (n % 2 == 1)
	{
		for (int i = 0; i < newn; i++) {
			for (int j = 0; j < newn; j++) {
				a11[i][j] = a[i][j];

				b11[i][j] = b[i][j];
			}
		}
		for (int i = 0; i < newn; i++)
		{
			for (int j = 0; j < (newn-1); j++) {
				a12[i][j] = a[i][j + newn];
				b12[i][j] = b[i][j + newn];
			}
			a12[i][newn-1] = 0;
			b12[i][newn-1] = 0;
		}
		for (int i = 0; i < (newn-1); i++) {
			for (int j = 0; j < newn; j++) {
				a21[i][j] = a[i + newn][j];
				b21[i][j] = b[i + newn][j];
			}
		}
		for (int j = 0; j < newn; j++) {
			a21[newn-1][j] = 0;
			b21[newn-1][j] = 0;
		}
		for (int i = 0; i < (newn-1); i++) {
			for (int j = 0; j < (newn-1); j++) {
				a22[i][j] = a[i+newn][j+newn];
				b22[i][j] = b[i+newn][j+newn];
				a22[newn - 1][j] = 0;
				b22[newn - 1][j] = 0;
			}
			a22[i][newn-1] = 0;
			b22[i][newn-1] = 0;
		}
		a22[newn-1][newn-1] = 0;
		b22[newn-1][newn-1] = 0;
	}

	// Calculating p1 to p7:

	sum(a11, a22, parta, newn); // a11 + a22
	sum(b11, b22, partb, newn); // b11 + b22
	strassen(parta, partb, p1, newn,cross); // p1 = (a11+a22) * (b11+b22)

	sum(a21, a22, parta, newn); // a21 + a22
	strassen(parta, b11, p2, newn,cross); // p2 = (a21+a22) * (b11)

	subtract(b12, b22, partb, newn); // b12 - b22
	strassen(a11, partb, p3, newn,cross); // p3 = (a11) * (b12 - b22)

	subtract(b21, b11, partb, newn); // b21 - b11
	strassen(a22, partb, p4, newn,cross); // p4 = (a22) * (b21 - b11)

	sum(a11, a12, parta, newn); // a11 + a12
	strassen(parta, b22, p5, newn,cross); // p5 = (a11+a12) * (b22)

	subtract(a21, a11, parta, newn); // a21 - a11
	sum(b11, b12, partb, newn); // b11 + b12
	strassen(parta, partb, p6, newn,cross); // p6 = (a21-a11) * (b11+b12)

	subtract(a12, a22, parta, newn); // a12 - a22
	sum(b21, b22, partb, newn); // b21 + b22
	strassen(parta, partb, p7, newn,cross); // p7 = (a12-a22) * (b21+b22)

	// calculating c21, c21, c11 e c22:

	sum(p3, p5, c12, newn); // c12 = p3 + p5
	sum(p2, p4, c21, newn); // c21 = p2 + p4

	sum(p1, p4, parta, newn); // p1 + p4
	sum(parta, p7, partb, newn); // p1 + p4 + p7
	subtract(partb, p5, c11, newn); // c11 = p1 + p4 - p5 + p7

	sum(p1, p3, parta, newn); // p1 + p3
	sum(parta, p6, partb, newn); // p1 + p3 + p6
	subtract(partb, p2, c22, newn); // c22 = p1 + p3 - p2 + p6

	// Grouping the results obtained in a single matrix:
	if(n%2==0)
	{
		for (int i = 0; i < newn; i++) {
			for (int j = 0; j < newn; j++) {
					c[i][j] = c11[i][j];
					c[i][j + newn] = c12[i][j];
					c[i + newn][j] = c21[i][j];
					c[i + newn][j + newn] = c22[i][j];
			}
		}
	}
	if (n % 2 == 1)
	{
		for (int i = 0; i < newn; i++) {
			for (int j = 0; j < newn; j++) {
				c[i][j] = c11[i][j];
			}
		}
		for (int i = 0; i < newn-1; i++) {
			for (int j = 0; j < newn; j++){
				c[newn+i][j] = c21[i][j];
			}
		}
		for (int i = 0; i < newn; i++) {
			for (int j = 0; j < newn - 1; j++) {
				c[i][newn + j] = c12[i][j];
			}
		}
		for (int i = 0; i < newn - 1; i++) {
			for (int j = 0; j < newn - 1; j++) {
				c[i + newn][j + newn] = c22[i][j];
			}
		}
	}

	free_matrix(a11, newn);
	free_matrix(a12, newn);
	free_matrix(a21, newn);
	free_matrix(a22, newn);

	free_matrix(b11, newn);
	free_matrix(b12, newn);
	free_matrix(b21, newn);
	free_matrix(b22, newn);

	free_matrix(c11, newn);
	free_matrix(c12, newn);
	free_matrix(c21, newn);
	free_matrix(c22, newn);

	free_matrix(p1, newn);
	free_matrix(p2, newn);
	free_matrix(p3, newn);
	free_matrix(p4, newn);
	free_matrix(p5, newn);
	free_matrix(p6, newn);
	free_matrix(p7, newn);
	free_matrix(parta, newn);
	free_matrix(partb, newn);

}

//  For test purpose, generate random matrices
void gen_matrix(int** M, int n)
{
	for (int i = 0; i<n; ++i)
	{
		for (int j = 0; j<n; ++j)
		{
			M[i][j] = rand() % 100;
			//M[i][j]=1;
		}
	}
}

// print matrix M
void print_diagonal(int** M, int n)
{
	for (int i = 0; i<n; ++i)
	{
		for (int j = 0; j<n; ++j)
		{
			cout << M[i][j] << " ";
		}
		cout << endl;
	}
	cout << endl;
}


/*------------------------------------------------------------------------------*/

int main(int argc, char** argv)
{
    for(int d=100;d<2100;d+=100)
    {
        for(int t=0;t<5;t++){
            cout<<d<<"\t";
            int mdim =d;
            srand(time(NULL));
            int** A = initiate_matrix(mdim);
            int** B = initiate_matrix(mdim);
            int** C = initiate_matrix(mdim);
            gen_matrix(A, mdim);
            gen_matrix(B, mdim);
            chrono::high_resolution_clock::time_point t1 = chrono::high_resolution_clock::now();
            strassen(A, B, C, mdim,1);
            chrono::high_resolution_clock::time_point t2 = chrono::high_resolution_clock::now();
            chrono::duration<double> time_span = chrono::duration_cast<chrono::duration<double>>(t2 - t1);
            cout <<"pure strassen"<<"\t"<<time_span.count() <<"\t";
            strassen(A, B, C, mdim,4);
            chrono::high_resolution_clock::time_point t3 = chrono::high_resolution_clock::now();
            chrono::duration<double> time_span2 = chrono::duration_cast<chrono::duration<double>>(t3 - t2);
            cout << "optimized strassen 4*4"<<"\t"<<time_span2.count() <<"\t";
            strassen(A, B, C, mdim,8);
            chrono::high_resolution_clock::time_point t4 = chrono::high_resolution_clock::now();
            chrono::duration<double> time_span3 = chrono::duration_cast<chrono::duration<double>>(t4 - t3);
            cout << "optimized strassen 8*8"<<"\t"<<time_span3.count() <<"\t";
            strassen(A, B, C, mdim,16);
            chrono::high_resolution_clock::time_point t5 = chrono::high_resolution_clock::now();
            chrono::duration<double> time_span4 = chrono::duration_cast<chrono::duration<double>>(t5 - t4);
            cout << "optimized strassen 16*16"<<"\t"<<time_span4.count() <<"\t";
            naive(A, B, C, mdim);
            chrono::high_resolution_clock::time_point t6 = chrono::high_resolution_clock::now();
            chrono::duration<double> time_span5 = chrono::duration_cast<chrono::duration<double>>(t6 - t5);
            cout<<"naive algorithm"<<"\t"<<time_span5.count()<<endl;
            free_matrix(A, mdim);
            free_matrix(B, mdim);
            free_matrix(C, mdim);
        }
    }
	return 0;
}
